     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4600R1 - REPORT ORDERING GET OUTQ
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWMROP     IF   E             DISK
     F************************************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I************************************************************

     C     *ENTRY        PLIST
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQROVL           10
     C                   PARM                    QQSPD#            1
     C                   PARM                    QQCRLB           10
     C                   PARM                    QQRTCD            1
     C*
     C     1             CHAIN     PRO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ROOUTQ        QQOUTQ
     C                   MOVEL     ROROVL        QQROVL
     C                   MOVEL     ROSPD#        QQSPD#
     C                   MOVEL     ROCRLB        QQCRLB
     C                   ELSE
     C                   MOVEL     'QPRINT'      QQOUTQ
     C                   MOVEL     *BLANKS       QQROVL
     C                   MOVEL     '1'           QQSPD#
     C                   MOVEL     '1'           QQRTCD
     C                   MOVEL     'QGPL    '    QQCRLB
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
