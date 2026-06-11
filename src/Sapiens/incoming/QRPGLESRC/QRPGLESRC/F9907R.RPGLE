     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE F9906R - CASE MANAGEMENT SERVICES REPORT WORKFILE BUILD
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     BUILDS WORKFILE FOR CASE SERVICE REPORT.                  *
     H*                                                               *
     H*  CALLED FROM: F9906C-OVERRIDE - CASE MANAGEMENT STATUS REPORT *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*  CREATED: 01/04/01        PROGRAMMER: 62       PROJECT: MC174 *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/13  C:277  Project: MC329 Phase 2                       *
     H*                  Get Description from ICD10 if ICD10 is active*
     H*                                                               *
     H* 11/07/07   C:180    SUPPORT 60228 - Fix printing of case info.
     H*                     Add new fields to file FXR8P.
     H*
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  95       Read main File                                      *
     H*                                                               *
     H*****************************************************************
     F*
     F* CASE MASTER - ALPHA
     FFMCMLH    IF   E           K DISK
     F*
     F* CASE MASTER - NUMERIC
     FFMCMLJ    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F* MEDICAL SERVICE TRANSACTION FILE
     FFAMSL4    IF   E           K DISK
     F* CASE HISTORY REPORT FILE
     FFXR8P     O    E             DISK
     F*** Beg Add *** 08/05/13 ****************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ****************************************

     D CD              S              4    DIM(99) ASCEND
     D CS              S              5  0 DIM(99)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D*****************************************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Beg Add *** 08/05/13 ****************************************
     D kkicd10         s                   like(iccode)
     D*** End Add *** 08/05/13 ****************************************

     C*****************************************************************
     C*   KEY LISTS
     C*****************************************************************
     C*
     C*  MEDICAL SERVICES TRANSACTION FILE
     C*
     C     MSKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKMSCD
     C*
     C     MSKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE 10 MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    R8lost
     C*** End Add *** 08/05/13 ***************************************
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    MSMSCD        KKMSCD
     C     *LIKE         DEFINE    MSDT          XXDATE
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@SVCD            4
     C                   PARM                    @@STAT            1
     C                   PARM                    @@FRDT            7
     C                   PARM                    @@TODT            7
     C                   PARM                    @@SORT            1
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
     C*****************************************************************
     C*   Initialize
     C*****************************************************************
     C                   TIME                    DSDTTM
     C*
     C*  PGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*  Read 1st record
     C*
     C                   EXSR      READ2
     C                   EXSR      READ1
     C*
     C     *IN95         DOWEQ     '0'
     C     *IN96         DOWEQ     '0'
     C*
     C*  If subset criteria met
     C*
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
     C*
     C     MEETSS        IFEQ      *ON
     C*
     C*  Write RECORD
     C*
     C                   EXSR      WRITER
     C*
     C                   ENDIF                                                  MEETSS
     C*
     C*  Read next record
     C*
     C                   EXSR      RNXT1
     C                   ENDDO                                                  WRK RECORD
     C*
     C                   EXSR      RNXT2
     C                   ENDDO                                                  WRK RECORD
     C*
     C*  End of Processing
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      @@FRDT        YYFRDT            7 0
     C                   MOVE      @@TODT        YYTODT            7 0
     C                   MOVEL(P)  @@SVCD        KKMSCD
     C*
     C     @@SVCD        IFEQ      *BLANK
     C                   SETON                                        24
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* READ1 - Read first record from file
     C*****************************************************************
     CSR   READ1         BEGSR
     C*
     C  N24MSKY          CHAIN     PMS01                              96
     C   24MSKY1         CHAIN     PMS01                              96
     C*
     C                   ENDSR                                                  READ1
     C*****************************************************************
     C* READ2 - Read first record from file
     C*****************************************************************
     CSR   READ2         BEGSR
     C*
     C     @@SORT        IFEQ      'A'
     C     *LOVAL        SETLL     PCM01
     C                   READ      PCM01                                  95
     C                   ELSE
     C     *LOVAL        SETLL     PCM02
     C                   READ      PCM02                                  95
     C                   ENDIF
     C*
     C                   ENDSR                                                  READ2
     C*****************************************************************
     C* RNXT1  - Read next record from file
     C*****************************************************************
     CSR   RNXT1         BEGSR
     C*
     C  N24MSKY          READE     PMS01                                  96
     C   24MSKY1         READE     PMS01                                  96
     C*
     C                   ENDSR                                                  RNXT1
     C*****************************************************************
     C* RNXT2  - Read next record from file
     C*****************************************************************
     CSR   RNXT2         BEGSR
     C*
     C     @@SORT        IFEQ      'A'
     C                   READ      PCM01                                  95
     C                   ELSE
     C                   READ      PCM02                                  95
     C                   ENDIF
     C*
     C                   EXSR      READ1
     C*
     C                   ENDSR                                                  RNXT2
     C*****************************************************************
     C* SUBSET - Check if member of records processed subset
     C*****************************************************************
     CSR   SUBSET        BEGSR
     C*
     C*  MEETSS = *OFF, if not to be processed
     C*
     C*
     C*  SERVICE CODE MUST MATCH IF ENTERED
     C*
     C     @@SVCD        IFNE      *BLANKS
     C     @@SVCD        ANDNE     MSMSCD
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*
     C*  DATE MUST FALL WITHIN RANGE FOR STATUS ENTERED
     C*
     C                   SELECT
     C*
     C     @@STAT        WHENEQ    'O'
     C     YYFRDT        IFNE      *ZEROS
     C     MSBGDT        IFLT      YYFRDT
     C     MSBGDT        ORGT      YYTODT
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C                   ELSE
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     MSBGDT        IFGT      XXDATE
     C     MSBGDT        ORLE      XXDATE
     C     MSENDT        ANDNE     *ZEROS
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     @@STAT        WHENEQ    'C'
     C     MSENDT        IFNE      *ZEROS
     C     MSENDT        IFLT      YYFRDT
     C     MSENDT        ORGT      YYTODT
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*
     C     @@STAT        WHENEQ    ' '
     C     MSBGDT        IFLT      YYFRDT
     C     MSBGDT        ORGT      YYTODT
     C     MSENDT        IFNE      *ZEROS
     C     MSENDT        IFLT      YYFRDT
     C     MSENDT        ORGT      YYTODT
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE FXR1P
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PR801
     C*
     C                   Z-ADD     CMCO#         R8CO#
     C                   Z-ADD     CMSUB#        R8SUB#
     C                   Z-ADD     CMFND         R8FND
     C                   Z-ADD     CMEMP#        R8EMP#
     C                   Z-ADD     CMDIV         R8DIV
     C                   MOVEL(P)  MSMSCD        R8MSCD
     C                   Z-ADD     CMFYR         R8FYR
     C                   Z-ADD     CMCASE        R8CASE
     C                   MOVEL(P)  CMLSN         R8LSN
     C                   MOVEL(P)  CMFSN         R8FSN
     C*
     C                   MOVEL(P)  'A'           R8STAT
     C                   MOVEL(P)  Q1USER        R8USER
     C                   MOVEL(P)  Q1PGM         R8PGM
     C                   MOVEL(P)  Q1USER        R8IUSR
     C                   MOVEL(P)  Q1PGM         R8IPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        R8TIME
     C                   Z-ADD     DSTIME        R8ITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R8DT
     C                   MOVEL     SRDTO         R8IDT
     C*
      ****** BEGIN ADD **************** 11/07/07 **********************
     C                   MOVE      MSICD9        R8ICD9
     C                   MOVE      MSLOST        R8LOST
     C                   MOVE      MSSTAF        R8STAF
     C                   MOVE      MSBGDT        R8BGDT
     C                   MOVE      MSENDT        R8ENDT
     C                   MOVE      MSSTS         R8MSST
     C                   MOVE      MSSEQ#        R8SEQ#
     C                   WRITE     PR801
      ****** END ADD   **************** 11/07/07 **********************
     C****** Beg Add *** 08/05/13 ************************************
      /Free
       If MSicdind ='Y';
          R8icd10 = MSicd1001;
       Endif;
      /End-free
     C****** End Add *** 08/05/13 ************************************
     C*
     C                   FEOD      FXR8P
     C*
     C                   ENDSR                                                  WRITER
