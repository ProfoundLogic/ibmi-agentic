     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE DQAMTR FOR RECORDS PRIOR TO DEDUC CHANGE INSTALL
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWMCMP     IF   E             DISK

     C*----------------------------------------------------------------
     C*  WCMC Installed determination
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1
     C                   PARM                    ##FBIF            1
     C                   PARM                    ##FL01            1
     C*
     C     ##FBIF        IFEQ      'Y'
     C     ##FL01        ANDEQ     'Y'
     C*
     C*  Start of main loop
     C*
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C*
     C                   CALL      'FA030R'
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      CMNRSE        ##CMNU            3            Nurse
     C                   PARM      *BLANKS       ##CMSU            3            Unavailable
     C                   PARM      CMCLEX        ##CLAD            3            Claim examiner
     C                   PARM      CMSUPR        ##CLSU            3            Supervisor
     C                   PARM      CMFEAD        ##CLFA            3            FEAD
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*
