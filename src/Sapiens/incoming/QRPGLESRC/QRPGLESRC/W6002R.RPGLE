     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W6002R - DAILY BATCH PROCESS SUBMIT
     H*****************************************************************
     H*  MODIFICATIONS:                                               *
     H*                                                               *
     H*   JJJ 1-16-91 - BASE ENHANCEMENT TO SEND A VERIFICATION       *
     H*                 MESSAGE IF ENTERED DATE DOES NOT EQUAL THE    *
     H*                 CURRENT SYSTEM DATE                           *
     H*                                                               *
     H*  09/08/94 C:15  PROJECT WC361                                 *
     H*                 CHANGED THE TEXT ON SCR02.                    *
     H*****************************************************************
     FW6002F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWZDBP     UF A E             DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D MG              S             60    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DLYA                   1     80
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *DTAARA       DEFINE    WDAILYA       DLYA
     C*
     C     *LIKE         DEFINE    CODT          XXDSDT
     C     *LIKE         DEFINE    CODT          XXPRDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 06/  /98 ************************************
     C******************** MOVE Q1USER    SCUSER
      **** END   DEL **** 06/  /98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      COCO#         SCCO#
     C*
     C*   CHECK TO SEE WHETHER THE DAILY PROCESS IS ALREADY EXECUTING
     C*
     C     *LOCK         IN        DLYA                                 80
     C     *IN80         IFEQ      '1'
     C                   EXFMT     SCR02
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C*   GET THE DAILY BATCH RECORD
     C*
     C     1             CHAIN     PDB01                              60
     C     *IN60         IFEQ      '0'
      *
     C                   Z-ADD     DBSDT         SCDATE
      *
     C                   MOVE      'N'           SCMNTH
     C                   MOVE      'N'           SCYEAR
     C*
     C     DBTAG         IFNE      *BLANK
     C                   MOVE      '1'           *IN90
      **** BEGIN DEL **** 06/  /98 ************************************
     C*******************  MOVE DBUSR     SCUSR
      **** END   DEL **** 06/  /98 ************************************
     C                   MOVE      DBJOB         SCJOB
      *
     C                   Z-ADD     DBSDT         SRDTI
      **** BEGIN CHG **** 06/23/98 ************************************
     C******************** CALL 'SRCVTDAT'DTMDY1
     C                   CALL      'SRCVTDAT'    DATCHK
      **** END   CHG **** 06/23/98 ************************************
     C                   MOVEL     SRDTO         SCSDT
      *
     C                   MOVE      DBSTM         SCSTM
     C                   MOVE      DBTAG         SCTAG
     C                   MOVE      DBCO#         SCRCO
     C                   MOVE      DBMNTH        SCMNTH
     C                   MOVE      DBYEAR        SCYEAR
     C                   END
     C*
     C                   ELSE
     C*
     C*                    Z-ADDDSDATE    SRDTI
     C*                    CALL 'SRCVTDAT'DTMDY8
     C*                    MOVELSRDTO     SCDATE
      *
     C                   Z-ADD     DSDATE        SCDATE
      *
     C                   MOVE      'N'           SCMNTH
     C                   MOVE      'N'           SCYEAR
     C*
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN09         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   CHECK FOR A VALID DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   MONTHLY FLAG MUST BE YES, NO
     C*
     C     SCMNTH        IFNE      'Y'
     C     SCMNTH        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   YEARLY FLAG MUST BE YES, NO
     C*
     C     SCYEAR        IFNE      'Y'
     C     SCYEAR        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  SEND A VERIFICATION MESSAGE IF ENTERED DATE DOES NOT EQUAL
     C*  CURRENT SYSTEM DATE AND WE ARE NOT RE-SUBMITTING AN ABNORMAL
     C*  TERMINATION
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT
     C*
     C     SCMSG1        IFEQ      *BLANKS
     C     XXPRDT        ANDNE     XXDSDT
     C     *IN90         ANDNE     '1'
     C                   MOVEL     MG(1)         SCMSG1
     C                   MOVEL     MG(2)         SCMSG2
     C                   GOTO      STRT
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, WRITE THE RECORD
     C*
     C*
     C*                    Z-ADDSCDATE    SRDTI
     C*                    CALL 'SRCVTDAT'DTMDY1
     C*                    MOVELSRDTO     DBSDT
      *
     C                   Z-ADD     SCDATE        DBSDT
      *
     C                   MOVE      DSTIME        DBSTM
     C                   MOVE      SCMNTH        DBMNTH
     C                   MOVE      SCYEAR        DBYEAR
     C                   MOVE      Q1USER        DBUSR
     C                   MOVE      Q1JOB         DBJOB
     C                   MOVE      COCO#         DBCO#
     C     *IN90         IFEQ      '1'
     C                   MOVE      'Y'           DBRTRT
     C                   MOVE      SCRCO         DBCO#
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PDB01
     C                   ELSE
     C                   WRITE     PDB01
     C                   END
     C*
     C*  SUBMIT THE JOB
     C*
     C                   CALL      'W6002C'
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE MG
      Date entered is not equal to current system date.
    Change date if necessary and press ENTER to continue.
