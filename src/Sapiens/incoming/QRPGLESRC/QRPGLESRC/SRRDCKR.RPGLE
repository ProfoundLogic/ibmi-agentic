     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     FSRRDCKF   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @READS            7 0
     C                   PARM                    @SEL              3 0
     C                   PARM                    @CONT             1
     C*
     C     *IN70         DOUEQ     *OFF
     C                   SETOFF                                       70
     C                   EXFMT     SCR01
     C                   MOVEA     '00000000'    *IN(71)
     C     *INKL         IFEQ      *OFF
     C                   EXSR      ERRCHK
     C                   ELSE
     C                   MOVE      '1'           ANSWER
     C                   ENDIF
     C                   ENDDO
     C                   MOVE      ANSWER        @CONT
     C                   SETON                                        LR
     C*
     C     ERRCHK        BEGSR
     C     ANSWER        IFLT      '1'
     C     ANSWER        ORGT      '5'
     C                   SETON                                        7071
     C                   ENDIF
     C                   ENDSR
