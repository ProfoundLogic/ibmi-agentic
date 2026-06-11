     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W6002R1 - GROUP BATCH PROCESS SUBMIT
     H****************************************************************
     H*  PROGRAM DESCRIPTION:                                        *
     H*          THIS PROGRAM SUBMITS THE GROUP CHECK BATCH PROCESS. *
     H*          THE GROUP BATCH PROCESS IS SEPARATE FROM THE DAILY  *
     H*          BATCH PROCESS BUT CAN NOT BE RUN AT THE SAME TIME.  *
     H*                                                               *
     H*****************************************************************
     H*  MODIFICATIONS:                                               *
     H*                                                               *
     H*   01/03/12 C:279 PROJECT: SY266 REDMINE: 1928                 *
     H*                  Display Pop-Up window for job submitted.     *
     H*                                                              *
     H*  09/08/94  C:15  PROJECT: WC361                              *
     H****************************************************************
     FW6002F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWZD3P     UF A E             DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D*
     D MG              S             60    DIM(2) CTDATA PERRCD(1)
     D*
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

      ****** Beg Add ***** 01/03/12 ******************************************
      /copy wprGenPop
      ****** End Add ***** 01/03/12 ******************************************
     C*
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
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
     C     *DTAARA       DEFINE    WDAILYA       DLYA
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
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
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      COCO#         SCCO#
     C*
     C*  CHECK TO SEE WHETHER THE GROUP PROCESS IS ALREADY EXECUTING
     C*
     C     *LOCK         IN        DLYA                                 80
     C     *IN80         IFEQ      '1'
     C                   EXFMT     SCR02
     C                   GOTO      TERM
     C                   END
     C*
     C*  GET THE GROUP BATCH RECORD
     C*
     C     1             CHAIN     PD301                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      D3SDT         SCDATE
     C*
     C     D3TAG         IFNE      *BLANK
     C                   MOVE      '1'           *IN90
     C                   MOVE      D3USR         SCUSR
     C                   MOVE      D3JOB         SCJOB
      *
     C*** BEG CHG *** 08/23/00 ****************************
     C***********          Z-ADDD3SDT     SRDTI
     C***********          CALL 'SRCVTDAT'DTMDY1
     C***********          MOVELSRDTO     SCSDT
     C                   Z-ADD     D3SDT         SCSDT
     C*** BEG CHG *** 08/23/00 ****************************
      *
     C                   MOVE      D3STM         SCSTM
     C                   MOVE      D3TAG         SCTAG
     C                   MOVE      D3CO#         SCRCO
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      DSDATE        SCDATE
     C*
     C                   END
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  PROCESS HELP REQUESTS
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
     C*  PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
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
     C                   MOVE      SCDATE        D3SDT
     C                   MOVE      DSTIME        D3STM
     C                   MOVE      Q1USER        D3USR
     C                   MOVE      Q1JOB         D3JOB
     C                   MOVE      COCO#         D3CO#
     C     *IN90         IFEQ      '1'
     C                   MOVE      'Y'           D3RTRT
     C                   MOVE      SCRCO         D3CO#
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PD301
     C                   ELSE
     C                   WRITE     PD301
     C                   END
     C*
     C*  SUBMIT THE GROUP BATCH PROCESS
     C*
     C                   CALL      'W6002C1'
     C****** Beg Add ***** 01/03/12 ******************************************
      /IF DEFINED (WEB)
     C                   CallP     PopOneBtn('Batch Job submitted successfully'
     C                             :'Information')
      /ENDIF
     C****** Beg Add ***** 01/03/12 ******************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE MG
      Date entered is not equal to current system date.
    Change date if necessary and press ENTER to continue.
