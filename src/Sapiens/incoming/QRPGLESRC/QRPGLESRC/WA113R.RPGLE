     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE WA113R - Returns cancelled date of employer RISK#
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program gets the risk number from WCMS Employer    **
     H**     Master File.                                            **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     VARIOUS                                                 **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   12/08/98      PROJECT: 87MHO                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H*  03/22/00 C:62 PROJ:87MIW
     H*                ADD PARM FOR RISK# EXTENSION
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      99                General usage                        **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      WMEML  (PEM01): WCMS Employer Master (CO#/WCF#/EMP/DIV)**
     H*****************************************************************
     H*
     FWMEML     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*          ------------------------------------------------------
     C*          Key List - WCMS Employer Master
     C*          ------------------------------------------------------
     C     EMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C*
     C*          ------------------------------------------------------
     C*          Entry PARM List
     C*          ------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@RSK#           15
     C*** BEGIN ADD *** 03/22/00 **************************************
     C                   PARM                    @@EXT             3 0
     C*** END   ADD *** 03/22/00 **************************************
     C*
     C*          ------------------------------------------------------
     C*          M A I N L I N E
     C*          ------------------------------------------------------
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EMSEMP        @@RSK#
     C*** BEGIN ADD *** 03/22/00 **************************************
     C     Q1PRMS        IFGT      5
     C*
     C                   Z-ADD     EMBEXT        @@EXT
     C*
     C                   ENDIF
     C*** END   ADD *** 03/22/00 **************************************
     C                   ENDIF                                                  *IN99 = Off
     C*
     C*          End of program
     C*          ------------------------------------------------------
     C                   RETURN
