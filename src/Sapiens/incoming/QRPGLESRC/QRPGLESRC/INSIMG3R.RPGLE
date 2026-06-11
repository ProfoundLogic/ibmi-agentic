     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSMUPL     UF A E           K DISK
     FSMSLL2    UF A E           K DISK
     F*
     FIAUSL     IF   E           K DISK
      * EXCEPTION SPECIFICATIONS * * *

     D V1              S              1    DIM(29) CTDATA PERRCD(29)
     D V2              S              1    DIM(40) CTDATA PERRCD(40)
      *****************************************************************
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D ISSFCD         UDS
     D  X#WSID                 1      2
     D*
     D*
     F*
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D ERSMUP          C                   CONST('ERROR "*DEFAULT" rc-
     D                                     d not found for SMU-
     D                                     PP.  Enter Dft Rcd.-
     D                                      Call INSIMG3R again-
     D                                      to Create Users.')
     D*
     D ERSMSL          C                   CONST('ERROR "*DEFAULT" rc-
     D                                     d not found for SMS-
     D                                     LP.  Enter Dft Rcd.-
     D                                      Call INSIMG3R again-
     D                                      to Create Users.')
     D*
     D QSD             C                   CONST('SUBDIREC.001')

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C*   KEY LIST FOR THE USER PROFILE  FILE
     C*
     C     CTKEY         KLIST
     C                   KFLD                    XXUSPR
     C*
     C*
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXUSPR
     C*
     C*
     C     UPKY2         KLIST
     C                   KFLD                    XXDFTU           10
     C*
     C     SLKY1         KLIST
     C                   KFLD                    XXUSPR
     C*
     C*
     C     SLKY2         KLIST
     C                   KFLD                    XXDFTU
     C*
     C*
     C     *DTAARA       DEFINE                  ISSFCD
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQSYSN            8
     C**
     C                   TIME                    DSDTTM
     C                   MOVEL     '*DEFAULT'    XXDFTU
      *
     C                   UNLOCK    ISSFCD
     C*
     C                   READ      QLIDOBJD                               60
     C     *IN60         DOWEQ     *OFF
      * *******  CTKEY     SETLLQLIDOBJD             60
      *** FOR EACH USER PROFILE, CHECK EXISTENCE OF UP RCD
      *** IF NOT FOUND, 1) GET DEFAULT UP RCD, AND THEN WRITE NEW UP RCD
      *** $$$ IF NO DEFAULT SNDMSG, AND END PGM
     C*
     C                   MOVEL     ODOBNM        XXUSPR           10
     C                   MOVEL     ODOBTX        XXDESC           80
     C******* GET OUTQ INFO FOR NEW USER PARM    RECORD
     C                   CALL      'SRRTVUSR'
     C                   PARM      XXUSPR        QQUSPR           10
     C                   PARM                    QQTEXT           50
     C                   PARM                    QQOUTQ           10
     C                   PARM      *BLANK        Q1RTCD            1
      *
      *
     C     UPKY1         CHAIN     PUP01                              61
     C     *IN61         IFEQ      *OFF
     C                   ELSE
     C*** NO HIT *** NEED TO CREATE NEW RECORD
     C     UPKY2         CHAIN     PUP01                              61
     C     *IN61         IFEQ      *OFF
     C**** CREATE UP RECORD WITH NEW USERID, OUTQ, AND TEXT,
     C*
     C                   MOVEL     XXUSPR        UPUSR
     C                   MOVEL     QQTEXT        UPUSNM
     C                   MOVEL     QQTEXT        UPTITL
     C                   MOVEL     QQOUTQ        UPOUTQ
     C*     THE WSID TYPE PGM TO CONCAT "#" WITH 2 BYTE UNIQUE VALUE
     C*
     C     TRYAGN        TAG
     C     *LOCK         IN        ISSFCD
     C*
     C     '#'           CAT       X#WSID        UPSTAF
     C****                 MOVELX#WSID    WSWSID
     C*
     C**** INCREMENT THE WSID DTAARA FOR NEXT NUMBER...
     C*
     C                   EXSR      INCRM
     C****
     C                   OUT       ISSFCD
     C*
     C** INIT MAINT FIELDS AND LAST MAINT FIELDS HERE!
     C                   MOVE      'A'           UPSTAT
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     Q1USER        UPIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPIDT
     C                   WRITE     PUP01
     C                   ELSE
     C*** NO DEFAULT RCD FOUND... ERROR!! SEND MESSAGE TO CALLER!
     C** BUILD MESSAGE TO SEND
     C*
     C                   CALL      'ISSNDMSG'
     C                   PARM      ERSMUP        XXMSGD          200
     C                   PARM      Q1USER        XXTOUS           10
     C                   PARM      *BLANK        XXRNC             1
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C                   ENDIF
     **
      *
      *** CHECK EXISTENCE OF THE SL RCD, IF NOT FOUND 1) DET DEFAULT SL
      *** RCD, THEN 2) WRITE NEW SL RECORD
      *** $$$ IF NO DEFAULT SND MSG AND ENDPGM
      *
      *
     C     SLKY1         CHAIN     PSL01                              61
     C     *IN61         IFEQ      *OFF
     C                   ELSE
     C*** NO HIT *** NEED TO CREATE NEW RECORD
     C     SLKY2         CHAIN     PSL01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C                   MOVEL     XXUSPR        SLUSRP
     C                   MOVE      *BLANKS       SLFOLD
     C                   MOVEL     XXUSPR        SLFOLD
     C                   MOVEL     QQTEXT        SLUDSC
     C*
     C** INIT MAINT FIELDS AND LAST MAINT FIELDS HERE!
     C                   MOVE      'A'           SLSTAT
     C                   MOVEL     Q1PGM         SLPGM
     C                   MOVEL     Q1USER        SLIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SLDT
     C                   WRITE     PSL01
     C                   ELSE
     C*** NO DEFAULT RCD FOUND... ERROR!! SEND MESSAGE TO CALLER!
     C** BUILD MESSAGE TO SEND
     C*
     C                   CALL      'ISSNDMSG'
     C                   PARM      ERSMSL        XXMSGD          200
     C                   PARM      Q1USER        XXTOUS           10
     C                   PARM      *BLANK        XXRNC             1
     C*
\    C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C                   ENDIF
     **
     C*** OTHER PROCESS' FOR USER PROFILE?
      ***
     C                   CALL      'INSIMG7C'
     C                   PARM                    QQUSPR           10
     C                   PARM                    QQTEXT           50
     C                   PARM                    QQSYSN            8
     C                   PARM      *BLANK        Q1RTCD            1
     C*
     C                   READ      QLIDOBJD                               60
     C                   ENDDO
      *
      *
      *
     C     TERM          TAG
      **
      *
     C                   MOVE      *ON           *INLR
     C*
     C*
     C     INCRM         BEGSR
     C*
     C                   MOVEA     '00'          *IN(65)
     C                   Z-ADD     1             X                 2 0
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEL     X#WSID        A                 1
     C                   MOVE      X#WSID        B                 1
     C     A             LOOKUP    V1(Y)                                  65
     C     B             LOOKUP    V2(X)                                  66
     C     *IN66         IFEQ      *ON
     C     X             IFLT      40
     C                   ADD       1             X
     C                   MOVEL     V2(X)         B
     C     A             CAT       B             X#WSID
     C                   GOTO      DONE1
     C                   ELSE
     C*** MUST CHANGE A, AND PUT B AT 1ST VALUE
     C                   Z-ADD     1             X
     C     *IN65         IFEQ      *ON
     C     Y             IFLT      29
     C                   ADD       1             Y
     C                   MOVEL     V1(Y)         A
     C                   MOVEL     V2(X)         B
     C     A             CAT       B             X#WSID
     C                   GOTO      DONE1
     C*
     C                   ELSE
     C***    *** ERRROR
     C                   ENDIF
     C                   ELSE
     C***    *** ERROR
     C                   ENDIF
     C***
     C                   ENDIF
     C                   ELSE
     C***    *** ERROR
     C                   ENDIF
     C*
     C     DONE1         ENDSR
**    VALUES
ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$
**    VALUES
0123456789@#$&ABCDEFGHIJKLMNOPQRSTUVWXYZ
