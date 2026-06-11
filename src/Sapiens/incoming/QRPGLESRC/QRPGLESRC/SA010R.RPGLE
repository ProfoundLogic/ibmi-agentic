     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*********************************************************************
     H* Revisions:
     H*
     H* 08/27/08  C:33  Project: SY147
     H*                 Retrieve RRN for SDCLP.
     H*********************************************************************
     FSDCLP     O  A E             DISK
     F                                     RENAME(PCL01:DBRCDF)
     F***  Begin Add  ***  08/27/08  **************************************
     F                                     INFDS(DBFDS)
     F***  End   Add  ***  08/27/08  **************************************
     FSMUPL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D***  Begin Add  ***  08/27/08  **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0

     I***  End   Add  ***  08/27/08  **************************************
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE
     C                   PARM                    @@RSYS            8
     C                   PARM                    @@RMK
     C                   PARM                    @@PROJ
     C                   PARM                    @@SEQ#
     C***  Begin Add  ***  08/27/08  **************************************
     C                   PARM                    @@RRN             9 0
     C***  End   Add  ***  08/27/08  **************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CLMODE        @@MODE
     C     *LIKE         DEFINE    CLRMK         @@RMK
     C     *LIKE         DEFINE    CLPROJ        @@PROJ
     C     *LIKE         DEFINE    CLSEQ#        @@SEQ#
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        CLIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLIDT
     C                   MOVEL     DSTIME        CLITIM
     C                   WRITE     DBRCDF
     C                   FEOD      SDCLP
     C*
     C***  Begin Add  ***  08/27/08  **************************************
     C     Q1PRMS        IFGE      6
     C                   Z-ADD     DBRRN         @@RRN
     C                   ENDIF
     C***  End   Add  ***  08/27/08  **************************************
     C*
     C                   SETON                                        LR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     @@MODE        CLMODE
     C                   MOVEL(P)  @@RSYS        CLRSYS
     C                   MOVEL     @@RMK         CLRMK
     C                   MOVEL     @@PROJ        CLPROJ
     C                   MOVEL     @@SEQ#        CLSEQ#
     C     Q1USER        CHAIN     PUP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     UPUSNM        CLSOUN
     C                   ENDIF
     C                   ENDSR
