     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTAPPFL - CMD TO EXECUTE TO CONTAIN 1 BYTE FLAG
     FMMOPL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXEXCL                 1    230
     D  OPEXCL                 1    230
     D  XXAPFL               230    230
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  READ MENU OPTION FILE
     C*
     C                   READ      POP01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     OPMYN         IFEQ      'Y'
     C     OPAPP         IFEQ      'LFS'
     C     OPAPP         OREQ      'TFS'
     C                   MOVE      'F'           XXAPFL
     C                   UPDATE    POP01
     C                   ELSE
     C     OPAPP         IFEQ      'LWC'
     C     OPAPP         OREQ      'TWC'
     C     OPAPP         OREQ      'LQT'
     C     OPAPP         OREQ      'MWC'
     C     OPAPP         OREQ      'LGL'
     C     OPAPP         OREQ      'LCK'
     C     OPAPP         OREQ      'LRI'
     C                   MOVE      'W'           XXAPFL
     C                   UPDATE    POP01
     C                   ELSE
     C     OPAPP         IFEQ      'LEM'
     C     OPAPP         OREQ      'TEM'
     C                   MOVE      'E'           XXAPFL
     C                   UPDATE    POP01
     C                   ELSE
     C     OPAPP         IFEQ      'LSC'
     C     OPAPP         OREQ      'MNU'
     C                   MOVE      'S'           XXAPFL
     C                   UPDATE    POP01
     C                   ELSE
     C     OPAPP         IFEQ      'LRC'
     C                   MOVE      'R'           XXAPFL
     C                   UPDATE    POP01
     C                   END                                                    END LRC
     C                   END                                                    END LSC
     C                   END                                                    END LEM
     C                   END                                                    END LWC
     C                   END                                                    END LFS
     C*
     C                   END                                                    END OPMYN
     C*
     C                   READ      POP01                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
