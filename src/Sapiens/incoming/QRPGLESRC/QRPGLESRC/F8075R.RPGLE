     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8075R - Upload ANCILLARY Bills - Selection                  *
     H*                                                               *
     H*  Called from: F8070C - Create Ancillary documents             *
     H*  Calls to   : F8076C - Upload Ancillary bills - Copy to FAC0P *
     H*               F8077C - Ancillary Medical Copy/Delete Document *
     H*****************************************************************
     H*  Created on 04/05/2011 by c:198 for Project 45MHZ             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FFXC0L     IF   E           K DISK
     F                                     RENAME(DOCDTL:DBRCDF)
     FFAC9L1    IF A E           K DISK
     F                                     INFDS(DBFDS)
     FFAC9L2    IF   E           K DISK
     F                                     RENAME(PC901:PC902)
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
     D  C9MBR#                 1     10
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
     C     C9KY1         KLIST
     C                   KFLD                    DOCNAM
     C*
     C     C9KY2         KLIST
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
     C     C9KY2         SETGT     PC902
     C                   READP     PC902                                  99
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
     C     C9KY1         CHAIN     PC901                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   EXSR      BLDMBR
     C*
     C                   CLEAR                   PC901
     C*
     C                   MOVEL(P)  DOCNAM        C9DCID
     C*
     C                   MOVEL(P)  KKMBR#        C9MBR#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C9ULDT
     C                   Z-ADD     DSTIME        C9ULTM
     C                   Z-ADD     *ZEROS        C9CVDT
     C                   Z-ADD     *ZEROS        C9CVTM
     C                   Z-ADD     *ZEROS        C9PRDT
     C                   Z-ADD     *ZEROS        C9PRTM
     C                   MOVE      'A'           C9STAT
     C*
     C                   MOVEL     Q1PGM         C9PGM
     C                   MOVEL     Q1USER        C9USER
     C                   Z-ADD     C9ULDT        C9DT
     C                   Z-ADD     DSTIME        C9TIME
     C*
     C                   MOVEL     Q1PGM         C9IPGM
     C                   MOVEL     Q1USER        C9IUSR
     C                   Z-ADD     C9ULDT        C9IDT
     C                   Z-ADD     DSTIME        C9ITIM
     C*
     C                   WRITE     PC901
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
     C     2             SUBST     DOCNAM        XXPREF            2
     C*
     C     XXPREF        IFEQ      'AM'
     C*
     C                   EXSR      GETWRT
     C*
     C     C9PRDT        IFEQ      *ZEROS
     C*
     C     C9CVDT        IFEQ      *ZEROS
     C                   MOVEL     'Y'           ##COPY
     C                   ELSE
     C                   MOVEL     'N'           ##COPY
     C                   ENDIF                                                  CVDT = 0
     C*
     C                   MOVE      *BLANKS       ##FOLD
     C                   Z-ADD     0             ##RRN
     C                   MOVE      *BLANKS       ##DCID
     C                   MOVE      *BLANKS       ##MBR#
     C*
     C                   CALL      'F8076C'
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      @@FOLD        ##FOLD           60
     C                   PARM      C9DCID        ##DCID           12
     C                   PARM      C9MBR#        ##MBR#           10
     C                   PARM                    ##COPY            1
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       ##DCID
     C*
     C                   CALL      'F8077C'
     C                   PARM                    @@CO#
     C                   PARM      C9DCID        ##DCID           12
     C*
     C                   ENDIF                                                  PRDT = 0
     C*
     C                   ENDIF                                                  PREFIX IS AM
     C*
     C                   READ      DBRCDF                                 99
     C                   ENDDO
     C*
     C                   ENDSR
