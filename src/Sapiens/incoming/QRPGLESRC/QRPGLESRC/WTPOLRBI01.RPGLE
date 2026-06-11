     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve payroll information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLQTE
     h*  Calls to:
     h*
     h*  Created on 06/27/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     D QCA             S             55    DIM(2) CTDATA PERRCD(1)
     C     *ENTRY        PLIST
     C                   Parm                    inRrn
     C                   Parm                    outJSON
     C                   Parm                    rtnCode
     C*  OVERRIDE DATABASE FILE WMEPL TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5

     C                   Call      'WTPOLQTE06'
     C                   Parm                    inRrn             9 0
     C                   Parm                    outJSON       32767
     C                   Parm                    rtnCode           1
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C                   Return
**  QCAEXEC TABLE
OVRDBF FILE(WMEPP) TOFILE(WAEPP)                                                 1
DLTOVR FILE(WMEPP)                                                               2
