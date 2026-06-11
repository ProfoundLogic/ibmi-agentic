     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA900R - API to DETERMINE FUND TYPE
     H*****************************************************************
     H*  Description:
     H*
     H*    ACCESS WCMS FUND AUXILIARY FILE TO GET FUND TYPE
     H*    Pass above values back to calling program.
     H*
     H*  Called From:
     H*    Program
     H*
     H*  Calls To:
     H*
     H*    None
     H*
     H*  Created:  10/23/97  C:53  Project:  MC145
     H*
     H*****************************************************************
     H*  Revisions:
     H*
     H*
     H*****************************************************************
     H*  Indicators:
     H*
     H*    60 - General File Access
     H*****************************************************************
     H*
     FWDF2L     IF   E           K DISK
     F*

     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQTYPE            2
     C*
     C     KKKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     *LIKE         DEFINE    F2CO#         KKCO#
     C     *LIKE         DEFINE    F2FND         KKFND
     C*
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      *BLANKS       QQTYPE
     C*****************************************************************
      *   ADD CALL TO FND/CO# CONVERSION
      *
      * GET GRP#
      *
     C                   MOVEL     *BLANKS       QQWCF#
      *
     C                   CALL      'FA700R'
     C                   PARM                    @@RTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQWCF#            3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C*
     C                   MOVEL     QQWCF#        KKFND
     C*****************************************************************
     C*
     C     KKKEY         CHAIN     PF201                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F2TYPE        QQTYPE
     C                   ELSE
     C                   MOVE      'F'           QQRTCD                          FAIL
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C                   RETURN
