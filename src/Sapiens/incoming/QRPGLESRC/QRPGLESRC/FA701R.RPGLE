     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA701R - API TO CHECK IF THIS IS AN INSIDE ACCOUNT
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Using @@CO#, @@SUB#, @@FND RETURN THE THE OUTSID ACCOUNT     *
     H*    DETERMINATION (Y/N)                                          *
     H*                                                                 *
     H*    RETURN CODES: 'N' = NO TRANSLATION                           *
     H*                  'F' = FAILED CHAIN ON FUND AUX FILE            *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    VARIOUS PROGRAMS                                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  12/16/97  C:53  Project:  MC145                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*
     H*  04/21/00 C:53 PROJECT: MC189
     H*                ADD TRANSLATION FOR FUND # IF CALLED FROM WCMC
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H*******************************************************************
     F*
     FFMF1L     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@OUTS            1
     C*
     C*** KEY  LIST FOR FUND/GROUP AUXILIARY FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     *LIKE         DEFINE    F1CO#         KKCO#
     C     *LIKE         DEFINE    F1SUB#        KKSUB#
     C     *LIKE         DEFINE    F1FND         KKFND
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C*
     C*** BEG ADD *** 04/21/00 ********************************
     C*
     C*** CALL API FOR SUB OR GROUP XLATE IF CALLED FROM WCMS
     C*
     C     @@SUB#        IFEQ      *BLANKS
     C     @@SUB#        OREQ      *ZEROS
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@CO#         ##CO#             3
     C                   PARM      @@FND         ##WCF#            3
     C                   PARM      *BLANKS       ##SUB#            5
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     ##RTCD        IFEQ      ' '
     C                   MOVE      ##SUB#        KKSUB#
     C                   MOVE      ##FND         KKFND
     C*** ##RTCD = 'F'
     C                   ELSE
     C*
     C*** SET @@RTCD = 'N' FOR NO FUND RECORD.
     C*
     C                   MOVE      'N'           @@RTCD
     C                   GOTO      EOJ
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 04/21/00 ********************************
     C*
     C*** SET FLAG TO YES IF NOT A WCMS GROUP - OUTSIDE ACCOUNT
     C*
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ' '           @@RTCD
     C*
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      'Y'           @@OUTS
     C                   ELSE
     C                   MOVE      'N'           @@OUTS
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      'F'           @@RTCD
     C                   ENDIF
     C*
     C     EOJ           TAG                                                    ADD 04/21/00
     C                   RETURN
