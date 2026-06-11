     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IVD01R  - WORKFLOW PROGRAM CALL                          *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*  PROGRAMMER: 02                                               *
     H*  CREATED:     9/09/99             PROJECT:                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*****************************************************************
     H*  FILE USGE:                                                  *
     H*****************************************************************
     F*

     D*
     D*
     D HEAD            C                   CONST('Accessing Visual Da-
     D                                     rts')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C                   PARM                    DEKEY            40
     C                   PARM                    DECOM1          256
     C                   PARM                    DECOM2           19
     C                   PARM                    QQPGM            10
     C                   PARM                    DERTCD            1
     C                   PARM                    ERR               1
     C*
     C                   MOVEL     QQPGM         ##PGM
     C     ##PGM         IFEQ      'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C                   ENDIF
     C     ##PGM         IFEQ      'W0050C01'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   ENDIF
     C*
     C     ##PGM         IFEQ      'W0070C'
     C                   MOVEL(P)  'W0070R'      ##PGM
     C                   ENDIF                                                  ##PGM=W0070C
     C*
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGM            10
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           ERR               1
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       DEPGM             8
     C                   MOVEL     QQPGM         DEPGM
     C**** USE IN-DERTCD*  MOVE '1'       DERTCD
     C*
     C                   CALL      DEPGM                                37
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD            1
     C*
     C     *IN37         IFEQ      '1'
     C                   MOVE      '2'           ERR
     C                   GOTO      TERM
     C                   END
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
