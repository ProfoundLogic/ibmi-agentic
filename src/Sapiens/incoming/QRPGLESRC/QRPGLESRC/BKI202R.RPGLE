     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKI202R- WORK CHECK RECONCILIATION
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    REMARK  TYPE REQUIRED                                   *
     H*  21    REMARK  FROM REQUIRED                                   *
     H*  22    REMARK  TO REQUIRED                                     *
     H* SCR02:                                                         *
     H*  20    HEADING REQUIRED                                        *
     H*  21    Y/N REQUIRED FOR DIARY TYPE FLAG                        *
     H*  64    DELETE NOT ALLOWED                                      *
     H******************************************************************
     FBKI202F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR02S:RRN2)
     FBKCRL1    UF   E           K DISK
     FBKCFL1    IF   E           K DISK

     D*
     D OP              S             30    DIM(2) CTDATA PERRCD(1)
     D MS              S             23    DIM(2) CTDATA PERRCD(1)
     D*
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
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CHKKEY        KLIST
     C                   KFLD                    APPKEY
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCCHK#
     C*
     C     EMKEY         KLIST
     C                   KFLD                    CRCO#
     C                   KFLD                    CRFAC#
     C                   KFLD                    CRSS#
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   Z-ADD     0             RRN2              5 0
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     1             SCCO#
     C                   Z-ADD     1             SCFAC#
     C                   MOVE      '1'           *IN34
     C                   MOVEL     '1'           SCCODE
     C                   MOVE      'AP'          APPKEY
     C     QQCALL        IFEQ      'P'
     C                   MOVEL     '2'           SCCODE
     C                   MOVE      'PR'          APPKEY
     C                   END
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   END
     C**********************************************************************
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
     C*   F4 = SEARCH RECON LIST                 *
     C********************************************
     C     *INKD         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      SCCO#         XXCO#             3 0
     C                   MOVE      SCFAC#        XXFAC#            3 0
     C                   CALL      'BKD201R'
     C                   PARM                    APPKEY
     C                   PARM                    XXCO#
     C                   PARM                    XXFAC#
     C                   PARM                    SCCHK#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   Z-ADD     *ZERO         SFRNO             5 0
     C                   Z-ADD     SCCHK#        XXCHK             7 0
     C     TAGG          TAG
     C                   ADD       1             SFRNO
     C     SFRNO         CHAIN     SCR02S                             50
     C     *IN50         IFEQ      '0'
     C     SCCHK#        IFEQ      *ZERO
     C                   Z-ADD     XXCHK         SCCHK#
     C     CHKKEY        CHAIN     PCR01                              60
     C  N60              Z-ADD     CRAMT         SCAMT
     C                   MOVE      *BLANKS       SCEMSG
     C                   Z-ADD     *ZERO         SCCAMT
     C                   UPDATE    SCR02S
     C                   ELSE
     C                   GOTO      TAGG
     C                   END
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKD
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C*
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02C'      SCR02T
     C     Q2RECF        CABEQ     'SCR02S'      SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  TEST THE ACTION CODE FOR 1 OR 2.
     C*
     C     SCCODE        IFNE      '1'
     C     SCCODE        ANDNE     '2'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCODE        COMP      '2'                                080607
     C   06              MOVE      OP(1)         SCOPTN
     C   06              MOVE      'AP'          APPKEY            2
     C   07              MOVE      OP(2)         SCOPTN
     C   07              MOVE      'PR'          APPKEY
     C*
     C*  VALIDATE THE COMPANY/FACILITY RECORD.
     C*
     C     CFKEY         CHAIN     PCF01                              21
     C     *IN21         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     CFNAME        SCCNAM
     C*
     C* INITIALIZE SUBFILE
     C*
     C                   MOVE      '1'           *IN02
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C                   MOVE      '0'           *IN02
     C                   Z-ADD     *ZERO         RRN2
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02   '    Q2RECF
     C                   GOTO      STRT
     C***
     C*  PROCESS SCREEN 2 - TRANSACTION ENTRY
     C***
     C     SCR02T        TAG
     C*
     C*  PROCESS ALL RECORDS IN SUBFILE
     C*
     C                   Z-ADD     *ZERO         SFRNO             5 0
     C                   MOVE      *ZEROS        ERRTOT            5 0
     C     TAGRC         TAG
     C***                  READCSCR02S                   50
     C                   ADD       1             SFRNO
     C     SFRNO         CHAIN     SCR02S                             50
     C     *IN50         IFEQ      '0'
     C     SCCHK#        IFEQ      *ZERO
     C     SCEMSG        IFGT      *BLANKS
     C                   MOVE      *BLANKS       SCEMSG
     C                   END
     C                   GOTO      ERRT
     C                   END
     C*
     C                   MOVE      *BLANKS       SCEMSG
     C                   MOVE      *ZEROS        SCCAMT
     C     CHKKEY        CHAIN     PCR01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     MS(1)         SCEMSG
     C                   ADD       1             ERRTOT
     C                   GOTO      ERRT
     C                   END
     C*
     C     CRAMT         IFNE      SCAMT
     C                   MOVEL     MS(2)         SCEMSG
     C                   Z-ADD     CRAMT         SCCAMT
     C                   ADD       1             ERRTOT
     C                   GOTO      ERRT
     C                   END
     C     ERRT          TAG
     C                   MOVE      '0'           *IN39
     C                   UPDATE    SCR02S
     C                   GOTO      TAGRC
     C                   END
     C*
     C*  ON ENTER REDISPLAY SCREEN FOR ERRORS
     C*
     C     ERRTOT        IFGT      *ZERO
     C                   MOVE      '1'           *IN29
     C                   MOVEL     'SCR02 '      Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  NO ERRORS, UPDATE STATUS FIELDS
     C*
     C                   Z-ADD     *ZERO         SFRNO             5 0
     C                   MOVE      *ZEROS        ERRTOT            5 0
     C     TAGUP         TAG
     C                   ADD       1             SFRNO
     C     SFRNO         CHAIN     SCR02S                             50
     C     *IN50         IFEQ      '0'
     C     SCCHK#        IFEQ      *ZERO
     C                   GOTO      TAGUP
     C                   END
     C*
     C     CHKKEY        CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      SCRECD        CRCLR
     C                   UPDATE    PCR01
     C                   END
     C                   GOTO      TAGUP
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
**  TABLE OP OPTION SELECTED.
 1. Accounts Payable
 2. Payroll
**  ERROR MESSEAGES
Check number not found.
Amount does not match
