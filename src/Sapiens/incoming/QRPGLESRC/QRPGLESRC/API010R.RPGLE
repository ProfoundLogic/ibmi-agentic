     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API010R - VENDOR MASTER MAINTENANCE
     H******************************************************************
     H* REVISIONS:
     H*
     H* 03/08/05  C:53  SUPPORT: WC081 # 45203
     H*                 DEFAULT CO# TO 001
     H******************************************************************
     FAPI010F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPVML1    UF A E           K DISK
     FBKSYL1    IF   E           K DISK
     FBKCAL1    IF   E           K DISK
     FAPAPL1    UF   E           K DISK

     D UA              S             10    DIM(3) CTDATA PERRCD(1)
     D SCF             S              1    DIM(24)                              FUNCTION FLAGS
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
     C     CAKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCGLD#
     C                   KFLD                    SCACC#
     C                   KFLD                    SCSUB#
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '4'           X@TYPE
     C                   MOVE      ' '           X@APP
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
     C*   F2 = CHART OF ACCOUNTS INQUIRY         *
     C********************************************
     C     *INKB         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      SCCO#         X@CO#
     C                   MOVE      SCFAC#        X@FAC#
     C                   MOVE      SCGLD#        X@GLD#
     C                   MOVE      SCACC#        X@ACC#
     C                   MOVE      SCSUB#        X@SUB#
     C                   CALL      'BKD010R'
     C                   PARM                    X@TYPE            1
     C                   PARM                    X@CO#             3 0
     C                   PARM                    X@FAC#            3 0
     C                   PARM                    X@GLD#            3 0
     C                   PARM                    X@ACC#            3 0
     C                   PARM                    X@SUB#            3 0
     C                   PARM                    X@APP             1
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     X@GLD#        IFGT      *ZERO
     C                   MOVEL     X@CO#         SCCO#
     C                   MOVEL     X@FAC#        SCFAC#
     C                   MOVEL     X@GLD#        SCGLD#
     C                   MOVEL     X@ACC#        SCACC#
     C                   MOVEL     X@SUB#        SCSUB#
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKB
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F4 = AUTO ASSIGN VENDOR NUMBER         *
     C********************************************
     C     *INKD         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C     'AP'          CHAIN     PAP01                              60
     C     *IN60         IFEQ      '0'
     C     ADDVDR        TAG
     C                   ADD       1             APVDR
     C                   MOVE      APVDR         SCVDR
     C     SCVDR         CHAIN     PVM01                              61
     C  N61              GOTO      ADDVDR
     C                   UPDATE    PAP01
     C                   ELSE
     C                   MOVE      *ZERO         SCVDR
     C                   END                                                    END *IN60
     C                   END                                                    END SCR01
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKD
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      ' '           X@TYPE
     C                   MOVE      ' '           QQRTC
     C                   CALL      'APD010R'
     C                   PARM                    SCVDR
     C                   PARM                    X@TYPE            1
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      SCR01T
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKE
     C********************************************
     C*   F8 = SEARCH LIST-CALL TERMS CODE       *
     C********************************************
     C     *INKH         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD509R'
     C                   PARM                    SCTERM
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   MOVE      '1'           *IN43
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C*
     C                   END                                                    END *INKH
     C********************************************
     C*   F9 = COMPANY FACILITY INQUIRY          *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      ' '           QQRTC
     C                   CALL      'API011R'
     C                   PARM                    SCVDR
     C                   PARM                    SCGLD#
     C                   PARM                    SCACC#
     C                   PARM                    SCSUB#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
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
     C     SCVDR         IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCVDR         IFGE      9990000
     C     SCVDR         ANDLE     9999998
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     SCVDR         CHAIN     PVM01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PVM01E
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C                   MOVE      VMCO#         SCCO#
     C                   MOVE      VMFAC#        SCFAC#
     C                   MOVE      VMNAME        SCNAME
     C                   MOVE      VMADR1        SCADR1
     C                   MOVE      VMADR2        SCADR2
     C                   MOVE      VMCITY        SCCITY
     C                   MOVE      VMST          SCST
     C                   MOVE      VMZIP1        SCZIP1
     C                   MOVE      VMZIP2        SCZIP2
     C                   MOVE      VMAREA        SCAREA
     C                   MOVE      VMPHN#        SCPHN#
     C                   MOVE      VMCONT        SCCONT
     C                   MOVE      VMTERM        SCTERM
     C                   MOVE      VMPCT4        SCPCT4
     C                   MOVE      VMFAXA        SCFAXA
     C                   MOVE      VMFAX#        SCFAX#
     C                   MOVE      VMFIDC        SCFIDC
     C                   MOVE      VMFEIN        SCFEIN
     C                   MOVE      VMALPH        SCALPH
     C                   MOVE      VMGLD#        SCGLD#
     C                   MOVE      VMACC#        SCACC#
     C                   MOVE      VMSUB#        SCSUB#
     C                   MOVE      VMTYPE        SCTYPE
     C                   MOVE      VMHOLD        SCHOLD
     C*
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCNAME
     C*** BEG CHG *** 03/08/05 ****************************
     C***********          MOVE *BLANKS   SCCO#
     C                   MOVE      '001'         SCCO#
     C*** BEG CHG *** 03/08/05 ****************************
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCITY
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *BLANKS       SCZIP1
     C                   MOVE      *BLANKS       SCZIP2
     C                   MOVE      *BLANKS       SCAREA
     C                   MOVE      *BLANKS       SCPHN#
     C                   MOVE      *BLANKS       SCCONT
     C                   MOVE      *BLANKS       SCTERM
     C                   MOVE      *BLANKS       SCPCT4
     C                   MOVE      *BLANKS       SCFAXA
     C                   MOVE      *BLANKS       SCFAX#
     C                   MOVE      *BLANKS       SCFIDC
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCALPH
     C                   MOVE      *BLANKS       SCGLD#
     C                   MOVE      *BLANKS       SCACC#
     C                   MOVE      *BLANKS       SCSUB#
     C                   MOVE      *BLANKS       SCHOLD
     C                   MOVE      *BLANKS       SCTYPE
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  NAME CANNOT BE EQUAL TO BLANKS
     C*
     C     SCNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C*  HOLD CHECKS MUST BE Y OR BLANK
     C*
     C     SCHOLD        IFNE      *BLANKS
     C     SCHOLD        ANDNE     'Y'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF ENTERED, ACCOUNT MUST EXIST
     C*
     C     SCGLD#        IFGT      *ZERO
     C     CAKEY         CHAIN     PCA01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     SCVDR         CHAIN     PVM01                              40
     C*
     C                   MOVE      SCVDR         VMVDR
     C                   MOVE      SCCO#         VMCO#
     C                   MOVE      SCFAC#        VMFAC#
     C                   MOVE      SCNAME        VMNAME
     C                   MOVE      SCADR1        VMADR1
     C                   MOVE      SCADR2        VMADR2
     C                   MOVE      SCCITY        VMCITY
     C                   MOVE      SCST          VMST
     C                   MOVE      SCZIP1        VMZIP1
     C                   MOVE      SCZIP2        VMZIP2
     C                   MOVE      SCAREA        VMAREA
     C                   MOVE      SCPHN#        VMPHN#
     C                   MOVE      SCCONT        VMCONT
     C                   MOVE      SCTERM        VMTERM
     C                   MOVE      SCPCT4        VMPCT4
     C                   MOVE      SCFAXA        VMFAXA
     C                   MOVE      SCFAX#        VMFAX#
     C                   MOVE      SCFIDC        VMFIDC
     C                   MOVE      SCFEIN        VMFEIN
     C                   MOVE      SCALPH        VMALPH
     C                   MOVE      SCGLD#        VMGLD#
     C                   MOVE      SCACC#        VMACC#
     C                   MOVE      SCSUB#        VMSUB#
     C                   MOVE      SCHOLD        VMHOLD
     C                   MOVE      SCTYPE        VMTYPE
     C*
     C                   MOVE      Q1USER        VMCUSR
     C                   MOVE      Q1PGM         VMIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VMCDT
     C*
     C                   Z-ADD     DSTIME        VMCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PVM01
     C                   ELSE
     C                   MOVE      Q1USER        VMIUSR
     C                   Z-ADD     *ZERO         VMPAMT
     C                   Z-ADD     *ZERO         VMTINV
     C                   Z-ADD     *ZERO         VMLPDT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VMIDT
     C*
     C                   Z-ADD     DSTIME        VMITIM
     C                   WRITE     PVM01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPVM01     E            PVM01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
