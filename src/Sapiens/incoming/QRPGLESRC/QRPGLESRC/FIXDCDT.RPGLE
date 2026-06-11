     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     FIXDCDT                                       **
     H* DESCRIPTION: Add century to the Diary completion and       **
     H*              Maintenance dates tn SDCMP if it is not       **
     H*              included.               .                     **
     H* CREATED BY:  C:128  PROJ:         DATE:  07/11/01          **
     H***************************************************************
     FSDCMP     UF   E             DISK
      *

     D                 DS
     D  CMDCDT                 1      7  0
     D  DCCENT                 1      1  0
     D  DCYEAR                 2      3  0
     D  DCMON                  4      5  0
     D  DCDAY                  6      7  0
     D                 DS
     D  CMDT                   1      7  0
     D  DTCENT                 1      1  0
     D  DTYEAR                 2      3  0
     D  DTMON                  4      5  0
     D  DTDAY                  6      7  0
      *

     C*** Read the Darts remark file.
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     CMDCDT        IFGT      *ZERO
     C     DCYEAR        IFLT      02
     C                   Z-ADD     1             DCCENT
     C                   ENDIF
     C                   ENDIF
      *
     C     CMDT          IFGT      *ZERO
     C     DTYEAR        IFLT      02
     C                   Z-ADD     1             DTCENT
     C                   ENDIF
     C                   ENDIF
      *
     C                   UPDATE    PCM01
      *
     C                   READ      PCM01                                  60
     C                   ENDDO                                                  ENDDOW *IN60
     C*
     C*** End the program
     C*
     C                   MOVE      '1'           *INLR
