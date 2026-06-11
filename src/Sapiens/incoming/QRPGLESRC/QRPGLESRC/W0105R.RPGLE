     H* (c) 2010 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H*****************************************************************
     H/TITLE W0105R  - RECORD IN USE MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL REMOVE RECORDS FROM          *
     H*                FILE WXRIP                                     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R
     H*    W0051R21
     H*    W8002C
     H*                                                               *
     H*  CREATED: 11/30/10  PROGRAMMER: C:180     SUPPORT: 74221      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0105F    CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWXRIL     UF   E           K DISK    USROPN
     FWXRIL1    UF   E           K DISK    RENAME(PRI01:PRI02) USROPN
     FWXRIL2    UF   E           K DISK    RENAME(PRI01:PRI03) USROPN
     FSMCOL     IF   E           K DISK
     FWMUPL     If   E           K DISK
    
      * STAND ALONE

     D @@RRN           S              9  0 INZ
     D @@FILE          S             10    INZ
     D @@CO            S              3  0 INZ
     D @@FND           S              3  0 INZ
     D @@EMP           S              9  0 INZ
     D @@MODE          S              1    INZ
     D ##DATA          S             80    INZ
     D ##LENG          S              3  0 INZ

      * DATA STRUCTURES

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C     *ENTRY        PLIST
     C                   PARM                    QQMODE            1
     C                   PARM                    QQRRN             9
     C                   PARM                    QQFILE           10
     C                   PARM                    QQCO              3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP             9

     C     RIUKY         KLIST
     C                   KFLD                    @@FILE
     C                   KFLD                    @@RRN

     C     RIUKY2        KLIST
     C                   KFLD                    @@CO
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP

     C     RIUKY3        KLIST
     C                   KFLD                    SCJOB
     C                   KFLD                    SCJOBN
     C                   KFLD                    SCUSER

     C     UPKY          KLIST
     C                   KFLD                    Q1USER

     C                   SELECT
     C                   WHEN      @@MODE = '@'
     C                   EXSR      DSPMOD
     C                   WHEN      @@MODE = 'B'
     C                   EXSR      PGMMOD
     C                   ENDSL

     C                   EVAL      *INLR =  *ON
      *
      *---------------------------------------------------------------------
      * DISPLAY MODE SUBROUTINE
      *---------------------------------------------------------------------
     C     DSPMOD        BEGSR

     C                   OPEN      W0105F
     C                   OPEN      WXRIL2

     C     UPKY          CHAIN     PUP01
     C     UPCO#         CHAIN     PCO01
     C                   IF        %FOUND
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL     ##DATA        SCCONM
     C                   ENDIF


     C                   DOU       *INKC
     C                   EXFMT     SCR01
     C   KC              LEAVE
     C                   Exsr      Validate

     C                   IF        *IN50 = *OFF
     C                   DOU       *IN71
     C     RIUKY3        DELETE    PRI03                              71
     C                   ENDDO
     C                   EXFMT     WDWMSG
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C
     C                   ENDDO

     C                   CLOSE     W0105F
     C                   CLOSE     WXRIL2
     C                   ENDSR
      *---------------------------------------------------------------------
      * VALIDATE SUBROUTINE
      *---------------------------------------------------------------------
     C     VALIDATE      BEGSR
     C     RIUKY3        CHAIN     PRI03                              50
     C                   ENDSR
      *---------------------------------------------------------------------
      * PGM MODE SUBROUTINE
      *---------------------------------------------------------------------
     C     PGMMOD        BEGSR
     C                   IF        @@RRN > *ZEROS
     C                   OPEN      WXRIL
     C     RIUKY         CHAIN     PRI01                              60
     C  N60              DELETE    PRI01
     C                   CLOSE     WXRIL
     C                   ELSE

     C                   MOVE      QQCO          @@CO
     C                   MOVE      QQFND         @@FND
     C                   MOVE      QQEMP         @@EMP

     C                   OPEN      WXRIL1
     C                   DOU       *IN71
     C     RIUKY2        DELETE    PRI02                              71
     C                   ENDDO
     C                   CLOSE     WXRIL1
     C                   ENDIF
     C                   ENDSR

     C*---------------------------------------------------------------------
     C* INITIALIZATION SUBROUTINE
     C*---------------------------------------------------------------------
     C     *INZSR        BEGSR
     C                   MOVE      QQMODE        @@MODE

     C                   IF        QQRRN > *BLANKS
     C                   MOVE      QQRRN         @@RRN
     C                   ENDIF
     C
     C                   MOVE      QQFILE        @@FILE


     C                   ENDSR
