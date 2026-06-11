     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F****
     FCLATCHL   UF   E           K DISK
     F                                     RENAME(PCM01:PATCH)
     F*

     D*
     D*
     D** MAKE DEFAULT IFS DIRECTORY HERE
     D***
     D*
     D                 DS
     D  @@KEY                  1     40
     D  @@CO#                  1      3
     D  @@FND                  4      6
     D  @@FYR                  7      9
     D  @@CASE                10     16
     D  @@FILL                17     40

     I*
     ***

     C     DARTS         KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     ****
     C*
     C                   READ      PATCH                                  60
     ****
     C     *IN60         DOWEQ     *OFF
     C     CMSTAT        ANDNE     'P'
     C     CMSTAT        ANDNE     'X'
     *****
     C                   MOVE      CMSEQ#        QQSEQ#            5
     ****
     C                   CALL      'CVTIIX1'
     C                   PARM                    CMKEY
     C                   PARM                    QQSEQ#
     C                   PARM      *BLANKS       QQPTH           255
     C                   PARM      *BLANK        @@RC              1
     **
     C                   MOVEL     @@RC          CMSTAT                          IF ERR, THEN UNCN I
     C                   MOVEL     QQPTH         CMUNCN                             ERRDESC
     C**
     C                   UPDATE    PATCH
     C*
     C                   READ      PATCH                                  60
     C                   ENDDO
     C*
     C*
     C                   MOVE      *ON           *INLR
