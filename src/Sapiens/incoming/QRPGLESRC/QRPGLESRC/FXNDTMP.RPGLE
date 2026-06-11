     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FXNDTMP - POPULATE TMSYS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program populates a new field, TMSYS, with data    **
     H**     from FAEOP                                              **
     H**   CALLED FROM:                                              **
     H**     Command Line                                            **
     H**   CALLS TO:                                                 **
     H**     None                                                    **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   09/30/99      PROJECT: MC176.                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      50      General usage                                  **
     H**      51      General usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      NDTMP    (PTM01): Time/Materials (T/M) Detail          **
     H**      FAEOLG   (PEO01): EOMB Transaction File                **
     H**                        (CO/SUB/FND/FYR/CASE/EOB#)           **
     H*****************************************************************
     H*
     FFAEOLG    IF   E           K DISK
     FNDTMP     UF   E           K DISK

     D*
     D* ---------------------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* ---------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* ---------------------------------------------------------------
     D* SYSTEM DATE AND TIME
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*          PEOLG - EOMB Transaction File
     C     EOKY          KLIST
     C                   KFLD                    TMCO#
     C                   KFLD                    TMSUB#
     C                   KFLD                    TMFND
     C                   KFLD                    TMFYR
     C                   KFLD                    TMCASE
     C                   KFLD                    TMEOB#
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          GET TIME
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C*
     C*          Read all records from NDTMP
     C*          ------------------------------------------------------
     C                   READ      PTM01                                  50
     C     *IN50         DOWEQ     *OFF
     C     EOKY          CHAIN     PEO01                              51
     C     *IN51         IFEQ      *OFF
     C                   MOVE      EOSYS         TMSYS
     C                   UPDATE    PTM01
     C                   ENDIF                                                  *IN51 = Off
     C*
     C*             Read next record
     C*             ---------------------------------------------------
     C                   READ      PTM01                                  50
     C                   ENDDO                                                  *IN50 = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
      /EJECT
