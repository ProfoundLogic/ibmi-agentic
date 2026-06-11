     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8076R - Upload Ancillary Bills - Transfer Program 1
     H*
     H*****************************************************************
     H*  Description - Updates conversion date/time in FAC9P since    *
     H*                CPYFRMPCD succeeded.                           *
     H*                                                               *
     H*  Called from: F8076C - Upload Ancillary Bills - CPYFRMPCD     *
     H*  Calls to   : F8077R - Upload Ancillary Bills - Trans Pgm 2   *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/05/2011 by c:198 for Project 45MHZ             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FFAC0L     UF   E             DISK
     FFAC1P     O    E             DISK
     FFAC9P     UF   E             DISK
     F*
      /COPY SCOPYWW01D
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D* Program constants.
     D*================================================================
     D COMAPI          C                   CONST('FA007R')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D DETAIL        E DS                  EXTNAME(FAC1P)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@DCID           12
     C                   PARM                    @@MBR#           10
     C                   PARM                    @@COPY            1
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   Z-ADD     0             XXCNT
     C*
     C                   CALL      COMAPI        COMPL                  99
     C*
     C     @@COPY        IFEQ      'Y'
     C                   READ      PC001                                  99
     C     *IN99         IFEQ      *OFF
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      WRITER
     C                   MOVEL     'Y'           C0PRFL
     C                   UPDATE    PC001
     C                   READ      PC001                                  99
     C                   ENDDO
     C                   ENDIF
     C                   EXSR      UPDAT9
     C                   ENDIF
     C*
     C* UPDATE WCMC FILES
     C*
     C                   CALL      'F8077R'
     C                   PARM      @@DCID        ##DCID           12
     C                   PARM      @@RRN         ##RRN             9 0
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT9 - Update Ancillary Bill Transmission File (FAN9P).
     C*================================================================
     C     UPDAT9        BEGSR
     C*
     C     @@RRN         CHAIN     PC901                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     C9CVDT        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  Q1USER        C9USER
     C                   MOVEL(P)  Q1PGM         C9PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C9CVDT
     C                   MOVEL     SRDTO         C9DT
     C*
     C                   Z-ADD     DSTIME        C9CVTM
     C*
     C                   ENDIF                                                  CVDT = 0
     C*
     C                   Z-ADD     XXCNT         C9CNT
     C*
     C                   UPDATE    PC901
     C*
     C                   Z-ADD     0             XXCNT
     C*
     C                   ENDIF                                                  *IN99 = '0'
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Ancillary Bill Process File (FAC1P).
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   PC101
     C                   MOVEL     C0DATA        DETAIL
     C                   MOVEL(P)  @@DCID        C1DCID
     C                   MOVEL(P)  @@MBR#        C1MBR#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1ULDT
     C                   ADD       1             XXCNT             9 0
     C                   WRITE     PC101
     C*
     C                   ENDSR
