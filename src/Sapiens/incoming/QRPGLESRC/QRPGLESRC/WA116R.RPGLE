     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - Determine if Group allows Medical Payments on Controvert  WCMC
     H*       Claim                                                     WCMC
     H*
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA116R                                            *
     H*    CREATED: 01/07/03  C:79 PROJ: MC255                        *
     H*    DESCRIPTION: API to determine if Group allows Medical      *
     H*                 Payments on Controvert Claim                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*    05/07/07  C:191  Project: WC1438                           *
     H*                     - Add one more parameter to return claim  *
     H*                       summary flag                            *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     FWMCXL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C* Parameter Lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                           I
     C                   PARM                    @@FND                           I
     C                   PARM                    @@ALMP            1             I/O
     C                   PARM                    @@RTCD            1             I/O
     C*** BEG ADD ****** 05/07/07 *********************************************
     C                   PARM                    @@CDSM            1             O
     C*** END ADD ****** 05/07/07 *********************************************
     C*****************************************************************
     C* Key  Lists
     C*****************************************************************
     C*
     C* Claim DARTS Control File Logical
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    CXCO#         KKCO#
     C     *LIKE         DEFINE    CXFND         KKFND
     C     *LIKE         DEFINE    CXCO#         @@CO#
     C     *LIKE         DEFINE    CXFND         @@FND
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FND         KKFND
     C                   MOVE      *BLANKS       @@ALMP
     C*
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C*** BEG ADD ****** 05/07/07 *********************************************
     C     Q1PRMS        IFLE      4
     C*** END ADD ****** 05/07/07 *********************************************
     C     CXRT01        IFEQ      'M'
     C     CXRT02        OREQ      'M'
     C     CXRT03        OREQ      'M'
     C     CXRT04        OREQ      'M'
     C*
     C                   MOVE      'Y'           @@ALMP
     C*
     C                   ENDIF                                                  CXRT01 IFEQ M
     C*** BEG ADD ****** 05/07/07 *********************************************
     C                   ENDIF                                                  Q1PRMS <= 4
     C* Add process to return claim summary flag
     C     Q1PRMS        IFGE      5
     C                   MOVE      CXCDSM        @@CDSM
     C                   ENDIF                                                  Q1PRMS >= 5
     C*** END ADD ****** 05/07/07 *********************************************
     C*
     C                   ELSE
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
