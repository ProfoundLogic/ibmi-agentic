     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     F****
     FSXDEL1    UF   E           K DISK
     F                                     RENAME(PDE01:PATCH)
     FWDF2L     IF   E           K DISK

     D**
     D*
     D** MAKE DEFAULT IFS DIRECTORY HERE
     D***
     D*
     D                 DS
     D  @@KEY                  1     40
     D  @@CO#                  1      3
     D  @@FND                  4      6
     D  @@EMP                  7     15
     D  @@DIV                 16     20
     D  @@FILL                17     40
     D*
     D*
     D                 DS
     D  ##KEY                  1     40
     D  ##CO#                  1      3
     D  ##FND                  4      6
     D  ##FYR                  7      9
     D  ##CASE                10     16
     D  ##FILL                17     40

     I*
     ***

     C     DARTS         KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     ***
     C     F2K           KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     ****
     C*
     C                   READ      PATCH                                  60
     ****
     C     *IN60         DOWEQ     *OFF
     **
     C                   MOVEL     CMKEY         @@KEY
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   MOVE      @@FND         XXFND             3 0
     C*
     C*
     C     F2K           CHAIN     PF201                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVE      F2CAR#        CMNCCI
     C                   ELSE
     C                   MOVE      *ZEROS        CMNCCI
     C                   ENDIF
     *****
     C                   UPDATE    PATCH
     C*
     C                   READ      PATCH                                  60
     C                   ENDDO
     C*
     C*
     C                   MOVE      *ON           *INLR
