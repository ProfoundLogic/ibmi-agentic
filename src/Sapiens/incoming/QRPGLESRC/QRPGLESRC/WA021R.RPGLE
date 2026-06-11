     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE WA021R - API V3 - GET REGION AND DESCRIPTION
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program gets the region number from WCMS employer  **
     H**     master file, then the description from the region file. **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     VARIOUS                                                 **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H*    PROGRAM: WA021R                                            *
     H*    CREATED: 10/20/98  C:62                                    *
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
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
     H**      WMRGL  (PRG01): Region file (CO#/WCF#/RGN#)            **
     H*****************************************************************
     H*
     FWMEML     IF   E           K DISK
     FWMREL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     EMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C*
     C     REKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    EMRGN#
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@RGN#            3 0
     C                   PARM                    @@DESC           30
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     REKY          CHAIN     PRE01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     RERGN#        @@RGN#
     C                   MOVEL(P)  REDESC        @@DESC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   RETURN
