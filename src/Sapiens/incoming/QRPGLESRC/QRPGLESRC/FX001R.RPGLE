     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FX001R - Fix the DARTS for transferred claims
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   This program fixes the DARTS records for claims which have  *
     H*   been transferred.                                           *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:15                                              *
     H* CREATED: 01/04/99               Support: 15655                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*****************************************************************
     F*
     FWACTP     IF   E             DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  QFKEY                  1     40
     D  QFCO#                  1      3  0
     D  QFFND                  4      6  0
     D  QFFYR                  7      9  0
     D  QFCASE                10     16  0
     D                 DS
     D  QTKEY                  1     40
     D  QTCO#                  1      3  0
     D  QTFND                  4      6  0
     D  QTFYR                  7      9  0
     D  QTCASE                10     16  0

     C*
     C                   TIME                    DSDTTM
     C**************************************************************************
     C*  Loop through claim transfer records.
     C*
     C                   READ      PCT01                                  50
     C     *IN50         DOWEQ     *OFF
     C*
     C*  Process claim if the key changed.
     C*
     C     CTFYR         IFNE      CTXFYR
     C     CTCASE        ANDEQ     CTXCAS
     C*
     C                   MOVE      CTCO#         QFCO#
     C                   MOVE      CTFND         QFFND
     C                   MOVE      CTXFYR        QFFYR
     C                   MOVE      CTXCAS        QFCASE
     C                   MOVE      CTCO#         QTCO#
     C                   MOVE      CTFND         QTFND
     C                   MOVE      CTFYR         QTFYR
     C                   MOVE      CTCASE        QTCASE
     C                   MOVEL(P)  'PDS'         QQQUE
     C*
     C                   CALL      'SC900C'
     C                   PARM                    QFKEY
     C                   PARM                    QTKEY
     C                   PARM                    QQQUE            10
     C                   PARM      'D'           QQMODE            1
     C                   PARM      ' '           QQRTCD            1
     C*
     C                   ENDIF
     C*
     C                   READ      PCT01                                  50
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C**************************************************************
