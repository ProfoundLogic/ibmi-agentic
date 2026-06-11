     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKSTR - STATE CONTROL EDIT PROGRAM
     H*
     H*****************************************************************
     H* DESCRIPITION:
     H*
     H* CHKSTR is called by various applications to verify: if state
     H* is installed, if state requires a tape, or if dental or
     H* pharmacy is installed.
     H*
     H* State is passed in to edit, return '1' found, '0' not found.
     H*****************************************************************
     H* Compile-time Arrays

     D ST              S              5    DIM(1) CTDATA PERRCD(1)              STATE
     D*
     D                 DS
     D  XXSTAT                 1      5
     D  XXST                   1      2
     D  XXFLAG                 3      5

     C*
     C                   MOVEL     *ZEROS        QQSTRT
     C                   MOVEL     *BLANKS       QQFLRT
     C     *ENTRY        PLIST
     C                   PARM                    QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C*
     C                   DO        1             T                 2 0
     C                   MOVEL     ST(T)         XXSTAT
     C     QQSTT         IFEQ      XXST
     C                   MOVE      1             QQSTRT
     C                   MOVE      XXFLAG        QQFLRT
     C                   ENDIF
     C                   ENDDO
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
** ST - ARRAY OF STATES
