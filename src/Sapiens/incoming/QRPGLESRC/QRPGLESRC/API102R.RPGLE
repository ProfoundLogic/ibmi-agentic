     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API102R - WORK TEMPORARY VENDOR
     H******************************************************************
     H* REVISIONS:
     H* 03/06/96 C:22 FIX TO VENDOR ACCESS
     H******************************************************************
     FAPI102F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPVML5    UF A E           K DISK
     FBKSYL1    IF   E           K DISK

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
     D CMDACT          DS
     D  ACT                    1    200
     D QLFDAP          DS
     D  SSPGM                  1     10
     D  SSLIB                 11     20
     D HELPCT          DS
     D  HLP                    1     60

     C*
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    SCCO#
     C                   PARM                    SCVDR             7 0
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
     C     VMKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCVDR
     C*
     C*   PARM LIST FOR WINDOWS RETRIEVE PARMS.
     C*
     C     RTVPRM        PLIST
     C                   PARM      QLFDAP        PGMLIB           20
     C     CMDACT        PARM                    CMDACT
     C     BARDTA        PARM                    BARDTA           79
     C     INFOA1        PARM                    INFOA1           78
     C     INFOA2        PARM                    INFOA2           78
     C     HELPCT        PARM                    HELPCT
     C                   PARM      'A'           SSTYPE            1
     C     SSFLG1        PARM                    SSFLG1            1
     C     SSFLG2        PARM                    SSFLG2            1
     C     SSFLG3        PARM                    SSFLG3            1
     C     SSFLG4        PARM                    SSFLG4            1
     C     SSFLG5        PARM                    SSFLG5            1
     C                   PARM                    BARDSC           50
     C*
     C*   PARM LIST FOR WINDOWS CALL.
     C*
     C     EXCPRM        PLIST
     C                   PARM      QLFDAP        PGMLIB
     C                   PARM      'A'           SSTYPE            1
     C     ACTION        PARM      ACTION        ACTION            8
     C                   PARM      HELPCT        HELPCT
     C                   PARM      CMDACT        CMDACT
     C                   PARM      SSFLG1        SSFLG1            1
     C                   PARM      SSFLG2        SSFLG2            1
     C                   PARM      SSFLG3        SSFLG3            1
     C                   PARM      SSFLG4        SSFLG4            1
     C                   PARM      SSFLG5        SSFLG5            1
     C     SSEXT         PARM      ' '           SSEXT             1
     C     SSCNL         PARM      ' '           SSCNL             1
     C     SSRTN         PARM      ' '           SSRTN             1
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
     C     *IN54         IFEQ      '0'
     C                   MOVE      'KLBEFCB '    SSPGM
     C                   MOVEL     '*LIBL'       SSLIB
     C                   MOVEL     'ACTION'      ACTBAR           10
     C                   MOVE      'BAR '        ACTBAR
     C                   CALL      'QAB155'      RTVPRM
     C                   MOVE      *BLANKS       ACTION
     C                   MOVEL     SYPOP4        ACTION
     C                   END
     C*
     C     VMKY          CHAIN     PVM01                              40
     C     *IN40         IFEQ      '0'
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
     C                   MOVE      VMALPH        SCALPH
     C                   MOVE      VMGLD#        SCGLD#
     C                   MOVE      VMACC#        SCACC#
     C                   MOVE      VMSUB#        SCSUB#
     C                   MOVE      VMTYPE        SCTYPE
     C                   ELSE
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCCO#
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
     C                   MOVE      *BLANKS       SCALPH
     C                   MOVE      *BLANKS       SCGLD#
     C                   MOVE      *BLANKS       SCACC#
     C                   MOVE      *BLANKS       SCSUB#
     C                   MOVE      *BLANKS       SCTYPE
     C                   END
     C*
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
     C     QQRTC         IFEQ      'A'
     C                   MOVE      'A'           QQCALL
     C                   GOTO      TERM
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C     X@GLD#        IFGT      *ZERO
     C                   MOVEL     X@CO#         SCCO#
     C                   MOVEL     X@FAC#        SCFAC#
     C                   MOVEL     X@GLD#        SCGLD#
     C                   MOVEL     X@ACC#        SCACC#
     C                   MOVEL     X@SUB#        SCSUB#
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKB
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQCALL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F6 = OPERATIONS INQUIRY                *
     C********************************************
     C     *INKF         IFEQ      '1'
     C     SYPOP4        ANDNE     *BLANK
     C     SYINS4        ANDEQ     'Y'
     C                   MOVE      *BLANK        ACTION
     C                   MOVEL     SYPOP4        ACTION
     C                   CALL      'QAB152'      EXCPRM
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C********************************************
     C*   F8 = SEARCH LIST-CALL TERMS CODE       *
     C********************************************
     C     *INKH         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD509R'
     C                   PARM                    SCTERM
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   MOVE      'A'           QQCALL
     C                   GOTO      TERM
     C                   END
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKH
     C********************************************
     C*   F9 = COMPANY FACILITY INQUIRY          *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   MOVE      'A'           QQCALL
     C                   GOTO      TERM
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END                                                    END *INKE
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQCALL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C********************************************
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   PROCESS THE SCREEN
     C*
     C*  NAME CANNOT BE EQUAL TO BLANKS
     C*
     C     SCNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF ENTERED, ACCOUNT MUST EXIST
     C*
     C**         SCGLD#    IFGT *ZERO
     C**         CAKEY     CHAINPCA01                22
     C**         *IN22     IFEQ '1'
     C**                   GOTO STRT
     C**                   END
     C**                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     VMKY          CHAIN     PVM01                              40
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
     C                   MOVE      *BLANKS       VMFIDC
     C                   MOVE      *ZEROS        VMFEIN
     C                   MOVE      SCALPH        VMALPH
     C                   MOVE      SCGLD#        VMGLD#
     C                   MOVE      SCACC#        VMACC#
     C                   MOVE      SCSUB#        VMSUB#
     C                   MOVE      *BLANKS       VMHOLD
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
     C*
     C                   MOVE      '*'           QQCALL
     C                   MOVEL     '1'           *INLR
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
