     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     FFILE      UF   F    2        DISK

     D*
     D OUT             DS
     D  X1CO#                  1      2P 0

     I*
     IFILE      AA  01
     I                             P    1    2 0XXCO#

     C*
     C     *ENTRY        PLIST
     C                   PARM                    FRCO#             3
     C                   PARM                    TOCO#             3
     C*
     C                   MOVE      FRCO#         XFCO#             3 0
     C                   MOVE      TOCO#         X1CO#
     C*
     C                   READ      FILE                                   LR
     C     *INLR         DOWEQ     '0'
     C*
     C     XXCO#         IFEQ      XFCO#
     C                   UPDATE    FILE          OUT
     C                   ENDIF
     C*
     C                   READ      FILE                                   LR
     C                   ENDDO
     C*
     C                   RETURN
