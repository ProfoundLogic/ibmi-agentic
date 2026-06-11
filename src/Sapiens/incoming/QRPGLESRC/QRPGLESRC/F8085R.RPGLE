     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8085R - HES Billing File Selection Program                  *
     H*                                                               *
     H*  Called from: F8084C1 - HES Billing File Submitter CL         *
     H*  Calls to   : F8086C  - HES Billing File Transfer Program 1   *
     H*             : F8087C  - HES Billing File Transfer Program 2   *
     H*                                                               *
     H*****************************************************************
     H*  Created on 11/03/2009 by C:227 for Project 135MMX            *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  12/05/05  C: 79  SUPPORT: 50173                              *
     H*                   CALL F8087C WITH CO#                        *
     H*                                                               *
     H*****************************************************************
     FFXH0L     IF   E           K DISK
     F                                     RENAME(DOCDTL:DBRCDF)
     FFAH9L1    IF A E           K DISK
     F                                     INFDS(DBFDS)
     FFAH9L2    IF   E           K DISK
     F                                     RENAME(PH901:PH902)
     F*

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  KKMBR#                 1     10
     D  YYJUL                  2      8  0
     D  B                      1      1
     D  YYYEAR                 2      5  0
     D  YYDAYS                 6      8  0
     D  SEQ#                   9     10  0
     D*
     D                 DS
     D  H9MBR#                 1     10
     D  YYJUL2                 2      8  0
     D  XXSEQ#                 9     10  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FOLD           60
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTJUL         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*JUL'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     H9KY1         KLIST
     C                   KFLD                    DOCNAM
     C*
     C     H9KY2         KLIST
     C                   KFLD                    KKMBR#
     C*================================================================
     C*  MAIN PROCESSING
     C*================================================================
     C                   EXSR      MAINSR
     C*
     C                   SETON                                        LR
     C*================================================================
     C* BLDMBR - Build Member Number
     C*================================================================
     C     BLDMBR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        YYJUL2
     C                   Z-ADD     *ZEROS        XXSEQ#
     C*
     C                   MOVEL(P)  'B'           B
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTJUL
     C                   MOVEL     SRDTO         YYJULI            5 0
     C*
     C                   Z-ADD     DSDATE        YYYEAR
     C                   Z-ADD     YYJULI        YYDAYS
     C*
     C                   Z-ADD     99            SEQ#
     C*
     C     H9KY2         SETGT     PH902
     C                   READP     PH902                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     YYJUL         ANDEQ     YYJUL2
     C     XXSEQ#        IFEQ      99
     C                   Z-ADD     0             SEQ#
     C                   ELSE
     C     XXSEQ#        ADD       1             SEQ#
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     0             SEQ#
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* GETWRT - Get or Write the transaction record
     C*================================================================
     C     GETWRT        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C     H9KY1         CHAIN     PH901                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   EXSR      BLDMBR
     C*
     C                   CLEAR                   PH901
     C*
     C                   MOVEL(P)  DOCNAM        H9DCID
     C*
     C                   MOVEL(P)  KKMBR#        H9MBR#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H9ULDT
     C                   Z-ADD     DSTIME        H9ULTM
     C                   Z-ADD     *ZEROS        H9CVDT
     C                   Z-ADD     *ZEROS        H9CVTM
     C                   Z-ADD     *ZEROS        H9PRDT
     C                   Z-ADD     *ZEROS        H9PRTM
     C                   MOVE      'A'           H9STAT
     C*
     C                   MOVEL     Q1PGM         H9PGM
     C                   MOVEL     Q1USER        H9USER
     C                   Z-ADD     H9ULDT        H9DT
     C                   Z-ADD     DSTIME        H9TIME
     C*
     C                   MOVEL     Q1PGM         H9IPGM
     C                   MOVEL     Q1USER        H9IUSR
     C                   Z-ADD     H9ULDT        H9IDT
     C                   Z-ADD     DSTIME        H9ITIM
     C*
     C                   WRITE     PH901
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MAINSR - Main processing
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   READ      DBRCDF                                 99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      GETWRT
     C*
     C     H9PRDT        IFEQ      *ZEROS
     C     H9CVDT        IFEQ      *ZEROS
     C                   MOVEL     'Y'           ##COPY
     C                   ELSE
     C                   MOVEL     'N'           ##COPY
     C                   ENDIF
     C*
     C                   CALL      'F8086C'
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      @@FOLD        ##FOLD           60
     C                   PARM      H9DCID        ##DCID           12
     C                   PARM      H9MBR#        ##MBR#           10
     C                   PARM                    ##COPY            1
     C*
     C                   ELSE
     C*
     C                   CALL      'F8087C'
     C                   PARM                    @@CO#
     C                   PARM      H9DCID        ##DCID           12
     C*
     C                   ENDIF
     C*
     C                   READ      DBRCDF                                 99
     C                   ENDDO
     C*
     C                   ENDSR
