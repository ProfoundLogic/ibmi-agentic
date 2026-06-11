     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE SRGETNM - RETREIVE KEY NAME SUB-PROGRAM
     H*
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
     FSMKDL     IF   E           K DISK
     FSDCML4    IF   E           K DISK
     FWXNMP     O    E             DISK
      *
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C                   PARM                    QQUSER           10
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *
     C*
     C*   KEY LIST FOR THE KEY DEFINITION FILE
     C*
     C     KDKEY         KLIST
     C                   KFLD                    QQPGM
     C*================================================================
     C*
     C     QQUSER        SETLL     PCM01
     C     QQUSER        READE     PCM01                                  67
     C     *IN67         DOWEQ     *OFF
     C*
     C*** INITIALIZE THE PARMS FOR PROGRAM CALL
     C*
     C                   MOVEL     *BLANKS       QQKEY
     C                   MOVEL     *BLANKS       QQPGM
     C                   MOVEL     *BLANKS       QQNAME
     C*
     C                   MOVEL     CMKEY         QQKEY            79
     C                   MOVEL     CMPNAM        QQPGM            10
     C                   MOVEL     *BLANKS       QQNAME           79
     C*
     C*
      * *** GET THE KEY/NAME
     C*
     C***
     C     KDKEY         CHAIN     PKD01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        QQPGM
     C     KDKEY         CHAIN     PKD01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      ENDPGM
     C                   END
     C*
     C                   END
     C*
     C* If conversion program name <> blanks; Then call program
     C* to pass back name in requested form
     C*  KDCVTP HOLDS THE CONVERSION PROGRAM NAME (I.E. CR001R...)
     C     KDCVTP        IFNE      *BLANKS
     C                   MOVE      *BLANKS       Q1PGM            10
     C                   MOVE      *BLANKS       Q1KEY            79
     C                   MOVE      *BLANKS       Q1NAME           79
     C                   MOVEL     QQPGM         Q1PGM
     C                   MOVEL     QQKEY         Q1KEY
     C                   CALL      KDCVTP
     C                   PARM                    Q1PGM
     C                   PARM                    Q1KEY
     C                   PARM                    Q1NAME
     C                   MOVE      *BLANKS       NMKEY
     C                   MOVEL     Q1KEY         NMKEY
     C                   Z-ADD     CMSEQ#        NMSEQ#
     C                   MOVE      *BLANKS       NMNAME
     C                   MOVEL     Q1NAME        NMNAME
     C                   WRITE     PNM01
     C                   END                                                    END KDCVTP NE *BLANK
     C     QQUSER        READE     PCM01                                  67
     C                   ENDDO
     C                   EXSR      ENDPGM
     C*---------------------------------------------------------------
     C     ENDPGM        BEGSR
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSR
