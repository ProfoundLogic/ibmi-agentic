     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*
      * SND MSG
     F*
     FIMIQL     IF   E           K DISK
     F*

     C     IQKY          KLIST
     C                   KFLD                    ##QID
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ##QID            10
     C                   PARM                    ##MSG1           64
     C                   PARM                    ##MSG2           64
     C                   PARM                    ##MSG3           64
     C                   PARM                    ##MSG4           64
     C                   PARM                    ##MSG5           64
     C                   PARM                    ##MSG6           64
     C                   PARM                    ##MSG7           64
     C                   PARM                    ##MSG8           64
     C                   PARM                    ##BRKM            1
     C                   PARM                    @$RTN             1
     C*****************************************
     C*
     C     IQKY          CHAIN     PIQ01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     IQMSGQ        ##MSGQ
     C                   ELSE
     C                   MOVEL     'QSYSOPR'     ##MSGQ
     C                   ENDIF
     C*
     C*
     C                   CALL      'ISSNDMC'
     C                   PARM                    ##MSGQ           10
     C                   PARM                    ##MSG1           64
     C                   PARM                    ##MSG2           64
     C                   PARM                    ##MSG3           64
     C                   PARM                    ##MSG4           64
     C                   PARM                    ##MSG5           64
     C                   PARM                    ##MSG6           64
     C                   PARM                    ##MSG7           64
     C                   PARM                    ##MSG8           64
     C                   PARM                    ##BRKM            1
     C                   PARM                    @$RTN             1
     C*
     C                   RETURN
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
