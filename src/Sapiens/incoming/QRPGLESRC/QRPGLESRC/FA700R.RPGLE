     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA700R - API TO RETREIVE SUB/GROUP TRANSLATION
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Using @@WCF#, access MC FUND, SUB#                           *
     H*    Using @@FND & @@SUB#, access WC TRANSLATION FUND #           *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    VARIOUS PROGRAMS                                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  09/17/97  C:53  Project:  MC145                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*  02/21/03  C:62 PROJECT 126MBD                                  *
     H*            COMMUNICATE FAILURE IF @@WCF# = '000'.               *
     H*                                                                 *
     H*  04/09/98  47  87MEA                                            *
     H*            USE SELEC STMT TO GO INTO ONLY ONE CONDITION.        *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H*******************************************************************
     F*
     FFMFNL1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@WCF#            3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C*
     C     FNKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNWCF#        KKWCF#
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKFND
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@WCF#        KKWCF#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C*
     C                   SELECT
     C*
     C*** X-LATE FROM WC FUND #
     C*
     C     @@WCF#        WHENNE    *BLANKS
     C*
     C*** BEGIN ADD *** 02/21/03 **************************************
     C     @@WCF#        IFNE      '000'
     C*** END   ADD *** 02/21/03 **************************************
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FNSUB#        @@SUB#
     C                   MOVE      FNFND         @@FND
     C                   MOVE      ' '           @@RTCD
     C                   ELSE
     C                   MOVE      'F'           @@RTCD
     C                   ENDIF
     C*** BEGIN ADD *** 02/21/03 **************************************
     C                   ELSE
     C                   MOVE      'F'           @@RTCD
     C                   ENDIF
     C*** END   ADD *** 02/21/03 **************************************
     C*
     C*
     C*** X-LATE FROM MC FND/SUB#
     C*
     C     @@SUB#        WHENNE    *BLANKS
     C     @@FND         ANDNE     *BLANKS
     C*
     C     FNKY2         CHAIN     PFN02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FNWCF#        @@WCF#
     C                   MOVE      ' '           @@RTCD
     C                   ELSE
     C                   MOVE      'F'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
