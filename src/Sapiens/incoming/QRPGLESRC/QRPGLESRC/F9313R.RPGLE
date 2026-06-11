     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F9312R - CM SERVICES SECONDARY REPORT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     LISTS PERSONS WHO ARE RESPONSIBLE FOR SECONDARY CASE LEVEL*
     H*                                                               *
     H*  CALLED FROM: F9311C - OVERRIDE - CM SERVICES REPORT          *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*  CREATED: 11/03/99        PROGRAMMER: 62       PROJECT: MC175 *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*  SMCOL    PCO01: CO                                   Company *
     H*  SMORL    POR01: CO/PGM                              Override *
     H*  SMUPL    PUP01: USER                               User Parm *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       Read main File                                      *
     H*  98       Last record processing                              *
     H*  97       Print Group Information                             *
     H*  20       TO FILE                                             *
     H*  19       TO REPORT                                           *
     H*  18       IF THERE IS NO HITS                                 *
     H*  17       Is the case status 'C'                              *
     H*  16       What Case Master File to Use                        *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*****************************************************************
     F*
     F* CASE MASTER - ALPHA
     FFMCMLM    IF   E           K DISK
     F*
     F* CASE MASTER - NUMERIC
     FFMCMLN    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F* CASE SERVICE FILE
     FFAMSL10   IF   E           K DISK
     FFXR6P     O    E             DISK

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*****************************************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*   KEY LISTS
     C*****************************************************************
     C*
     C*   Company
     C*
     C     COKY          KLIST
     C                   KFLD                    @@CO#
     C*
     C*  CASE SERVICE FILE
     C*
     C     MSKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     MSKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    @@STAF
     C*
     C*  Output Routing
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@STAF            3
     C                   PARM                    @@CSLD            1
     C                   PARM                    @@SRT1            2
     C                   PARM                    @@SRT2            1
     C                   PARM                    @@TYPE            1
     C                   PARM                    @@MODE            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C*   Initialize
     C*****************************************************************
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           MEETPR            1            PRINTED
     C*
     C*  PGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*   What File to Read
     C*
     C                   EXSR      RDCOND
     C*
     C*  Read 1st record
     C*
     C  N16              EXSR      READ1
     C   16              EXSR      READ2
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C   21MSKY          CHAIN     PMS01                              99
     C  N21MSKY2         CHAIN     PMS01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRITER
     C*
     C   21MSKY          READE     PMS01                                  99
     C  N21MSKY2         READE     PMS01                                  99
     C*
     C                   ENDDO
     C*
     C*  Read next record
     C*
     C  N16              EXSR      READN1
     C   16              EXSR      READN2
     C*
     C                   ENDDO                                                  WRK RECORD
     C*
     C*  End of Processing
     C*
     C     END           TAG
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C     @@STAF        IFEQ      *BLANKS
     C*
     C                   SETON                                        21
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       21
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* RDCOND - CONDITION OF WHICH LOGICAL TO READ
     C*****************************************************************
     CSR   RDCOND        BEGSR
     C*
     C     @@SRT2        IFEQ      'A'
     C*
     C                   SETOFF                                       16
     C*
     C                   ELSE
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  RDCOND
     C*****************************************************************
     C* READ1 - Read first record from file
     C*****************************************************************
     CSR   READ1         BEGSR
     C*
     C     *LOVAL        SETLL     PCM01
     C                   READ      PCM01                                  99
     C*
     C                   ENDSR                                                  READ1
     C*****************************************************************
     C* READ2 - Read first record from file
     C*****************************************************************
     CSR   READ2         BEGSR
     C*
     C     *LOVAL        SETLL     PCM02
     C                   READ      PCM02                                  99
     C*
     C                   ENDSR                                                  READ2
     C*****************************************************************
     C* READN1 - Read next record from file
     C*****************************************************************
     CSR   READN1        BEGSR
     C*
     C                   READ      PCM01                                  99
     C*
     C                   ENDSR                                                  READN1
     C*****************************************************************
     C* READN2 - Read next record from file
     C*****************************************************************
     CSR   READN2        BEGSR
     C*
     C                   READ      PCM02                                  99
     C*
     C                   ENDSR                                                  READN2
     C*****************************************************************
     C* WRITER - WRITE FXR2P
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PR601
     C*
     C                   Z-ADD     CMCO#         R6CO#
     C                   Z-ADD     CMSUB#        R6SUB#
     C                   Z-ADD     CMFND         R6FND
     C                   Z-ADD     CMEMP#        R6EMP#
     C                   Z-ADD     CMDIV         R6DIV
     C                   Z-ADD     CMFYR         R6FYR
     C                   Z-ADD     CMCASE        R6CASE
     C                   MOVEL(P)  MSMSCD        R6MSCD
     C                   Z-ADD     MSSEQ#        R6SEQ#
     C                   MOVEL(P)  CMLSN         R6LSN
     C                   MOVEL(P)  CMFSN         R6FSN
     C                   MOVEL(P)  MSSTAF        R6STAF
     C*
     C                   MOVEL(P)  'A'           R6STAT
     C                   MOVEL(P)  Q1USER        R6USER
     C                   MOVEL(P)  Q1PGM         R6PGM
     C                   MOVEL(P)  Q1USER        R6IUSR
     C                   MOVEL(P)  Q1PGM         R6IPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        R6TIME
     C                   Z-ADD     DSTIME        R6ITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R6DT
     C                   MOVEL     SRDTO         R6IDT
     C*
     C                   WRITE     PR601
     C*
     C                   FEOD      FXR6P
     C*
     C                   ENDSR                                                  WRITER
     C*****************************************************************
