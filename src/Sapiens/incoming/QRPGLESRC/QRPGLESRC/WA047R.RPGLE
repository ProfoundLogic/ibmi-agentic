     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  WA047R - Find out if EOMB has any payments.                  *
     H*                                                               *
     H*  Called from: F0350R                                          *
     H*                                                               *
     H*  Created: 06/01/01  C:62   Project: FS227                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  00/00/00 C:00 Project: xxxxxx                                *
     H*                                                               *
     H*  10/07/03 C:62 Project: FS267                                 *
     H*                Let's give the option to go out to recon       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     F*** BEGIN CHG *** 10/07/03 **************************************
     F**********WDPALA  IF  E           K        DISK
     FWDPALG    IF   E           K DISK
     F*** END   CHG *** 10/07/03 **************************************
     F*

     D*** BEGIN ADD *** 10/07/03 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END   ADD *** 10/07/03 **************************************
     I*

     C*----------------------------------------------------------------
     C*  Key list for the Payment File
     C*
     C     PAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 10/07/03 **************************************
     C*
     C     PAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     C*** END   ADD *** 10/07/03 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@EOB#            9
     C                   PARM                    @@RTCD            1
     C*** BEGIN ADD *** 10/07/03 **************************************
     C                   PARM                    @@RCN             3
     C*** END   ADD *** 10/07/03 **************************************
     C*
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PAEOB#        KKEOB#
     C*** BEGIN ADD *** 10/07/03 **************************************
     C     *LIKE         DEFINE    PAEOBS        KKEOBS
     C*** END   ADD *** 10/07/03 **************************************
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@EOB#        KKEOB#
     C*** BEGIN ADD *** 10/07/03 **************************************
     C                   SETOFF                                       16
     C     Q1PRMS        IFGT      6
     C                   SETON                                        16
     C                   CLEAR                   KKEOBS
     C                   MOVE      @@RCN         KKEOBS
     C                   ENDIF
     C*** END   ADD *** 10/07/03 **************************************
     C*
     C*  See if there is a Payment
     C*
     C*** BEGIN ADD *** 10/07/03 **************************************
     C     *IN16         IFEQ      *ON
     C     PAKY2         CHAIN     PPA01                              99
     C                   ELSE
     C*** END   ADD *** 10/07/03 **************************************
     C     PAKY          CHAIN     PPA01                              99
     C*** BEGIN ADD *** 10/07/03 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/07/03 **************************************
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     'F'           @@RTCD
     C                   ENDIF
     C*
     C*  End of pgm
     C*
     C                   MOVE      *ON           *INLR
     C*
