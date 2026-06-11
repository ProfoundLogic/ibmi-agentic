     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  SC851RA- FOLDER EXCLUSION FILE RETRIEVE (OPTICAL)            *
     H*  This program receives THE FOLDERS TO EXCLUDE FROM OPTICAL    *
     H*     PURGE :   12/16/97
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*  60  General CHAIN/READE                                     **
     H*****************************************************************
     H*  FILE USAGE:                                                 **
     H*****************************************************************
     FSMFEP     IF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFLR            40
     C                   PARM                    QQRTCD            1
     C*
     C                   READ      SMFEP                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     *BLANK        QQRTCD
     C                   MOVE      FEFLR         QQFLR
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   MOVE      *BLANKS       QQFLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   RETURN
     C***  End the Program
     C     TERM          TAG
     C                   RETURN
