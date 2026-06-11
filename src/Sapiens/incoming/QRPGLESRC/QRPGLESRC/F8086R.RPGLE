     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8086R - HES Billing File Transfer Program 1
     H*
     H*****************************************************************
     H*  Description - Updates conversion date/time in FAH9P since    *
     H*                CPYFRMPCD succeeded.                           *
     H*                                                               *
     H*  Called from: F8086C - HES Billing File Copy Document to FAH0P*
     H*  Calls to   :                                                 *
     H*                                                               *
     H*****************************************************************
     H*  Created on 11/03/2009 by C:227 for Project 135MMX            *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*  11/17/05  C: 79  Project: 135MET                             *
     H*                   Additional change for remittance file proc  *
     H*                                                               *
     H*****************************************************************
     FFAH0L     UF   E             DISK
     FFAH1P     O    E             DISK
     FFAH9P     UF   E             DISK
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
     D HEADER        E DS                  EXTNAME(FAH1P)

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
     C                   CALL      COMAPI        COMPL                  99
     C*
     C                   Z-ADD     0             CNT               8 0
     C     @@COPY        IFEQ      'Y'
     C                   READ      PH001                                  99
     C     *IN99         IFEQ      *OFF
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      WRITER
     C                   MOVEL     'Y'           H0PRFL
     C                   UPDATE    PH001
     C                   READ      PH001                                  99
     C                   ENDDO
     C                   ENDIF
     C                   EXSR      UPDAT9
     C                   ENDIF
     C*
     C* UPDATE WCMC FILES
     C*
     C                   CALL      'F8087R'
     C                   PARM      @@DCID        ##DCID           12
     C                   PARM      @@RRN         ##RRN             9 0
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT9 - Update HES Billing Transaction File (FAH9P).
     C*================================================================
     C     UPDAT9        BEGSR
     C*
     C     @@RRN         CHAIN     PH901                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     H9CVDT        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  Q1USER        H9USER
     C                   MOVEL(P)  Q1PGM         H9PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H9CVDT
     C                   MOVEL     SRDTO         H9DT
     C*
     C                   Z-ADD     DSTIME        H9CVTM
     C                   Z-ADD     CNT           H9RECL
     C                   MOVE      HDCNT         H9HDCT
     C                   ENDIF
     C*
     C                   UPDATE    PH901
     C                   ENDIF
     C*
     C                   Z-ADD     0             CNT
     C                   MOVE      *ALL'0'       HDCNT
     C                   ENDSR
     C*================================================================
     C* WRITER - HES Billing Header File (FAH1P).
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   MOVEL     H0DATA        TEST              1
     C*
     C                   SELECT
     C     TEST          WHENEQ    'H'
     C     5             SUBST     H0DATA:21     HDCNT             5
     C     TEST          WHENEQ    'T'
     C     5             SUBST     H0DATA:2      TOTLD             5
     C                   OTHER
     C                   ADD       1             CNT
     C                   CLEAR                   PH101
     C                   MOVEL     H0DATA        HEADER
     C                   MOVEL(P)  @@DCID        H1DCID
     C                   MOVEL(P)  @@MBR#        H1MBR#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1ULDT
     C*** END ADD *** 11/17/05 ****************************************
     C                   WRITE     PH101
     C                   ENDSL
     C*
     C                   ENDSR
