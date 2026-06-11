     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F7061RFX - CASE TRANSFER FIX PGM CASE OR SS#/ACDT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     FF7061FFX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CASE MASTER FILE (FMCML1 - PCM01)
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C*  CASE MASTER FILE (FMCMLA - PCM02)
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     *LIKE         DEFINE    CMDT          XXACDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPCO#         SCFCO#
     C                   MOVE      UPSUB#        SCFSUB
     C                   MOVE      UPFND         SCFFND
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C**************************************************************************
     C     CMDKEY        TAG
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*   F3 = EXIT (SCR01/SCR02)     *
     C*********************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*********************************
     C*   F12 = PREVIOUS (SCR02)      *
     C*********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C                   Z-ADD     *ZEROS        SCTCO#
     C                   Z-ADD     *ZEROS        SCTSUB
     C                   Z-ADD     *ZEROS        SCTFND
     C                   Z-ADD     *ZEROS        SCTFYR
     C                   Z-ADD     *ZEROS        SCTCAS
     C                   MOVE      *BLANKS       SCTSS#
     C                   Z-ADD     *ZEROS        SCTACD
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C************************************
     C*   *IN10 - PROCESS HELP REQUESTS  *
     C************************************
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END CMDKEYS
     C*******************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* BOTH TYPES OF TRANSFERS CANNOT BE ENTERED
     C*
     C     SCFCO#        IFGT      *ZEROS
     C     SCFSUB        ORGT      *ZEROS
     C     SCFFND        ORGT      *ZEROS
     C     SCFFYR        ORGT      *ZEROS
     C     SCFCAS        ORGT      *ZEROS
     C     SCFSS#        IFNE      *BLANKS
     C     SCFACD        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCFSS#        IFGT      *BLANKS
     C     SCFACD        ORGT      *ZEROS
     C     SCFCO#        IFNE      *ZEROS
     C     SCFSUB        ANDNE     *ZEROS
     C     SCFFND        ANDNE     *ZEROS
     C     SCFFYR        ANDNE     *ZEROS
     C     SCFCAS        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN28
     C                   MOVE      '0'           *IN29
     C     SCFCO#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN28
     C                   END
     C     SCFSS#        IFNE      *BLANKS
     C                   MOVE      '1'           *IN29
     C                   END
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  PROCESS SECOND SCREEN - SCR02
     C*
     C     SCR02T        TAG
     C*
     C* BOTH TYPES OF TRANSFERS CANNOT BE ENTERED
     C*
     C     SCTCO#        IFGT      *ZEROS
     C     SCTSUB        ORGT      *ZEROS
     C     SCTFND        ORGT      *ZEROS
     C     SCTFYR        ORGT      *ZEROS
     C     SCTCAS        ORGT      *ZEROS
     C     SCTSS#        IFNE      *BLANKS
     C     SCTACD        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCTSS#        IFGT      *BLANKS
     C     SCTACD        ORGT      *ZEROS
     C     SCTCO#        IFNE      *ZEROS
     C     SCTSUB        ANDNE     *ZEROS
     C     SCTFND        ANDNE     *ZEROS
     C     SCTFYR        ANDNE     *ZEROS
     C     SCTCAS        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* THE TO TRANSFER MUST BE SAME AS FROM TRANSFER
     C*
     C     SCFCO#        IFNE      *ZEROS
     C     SCTSS#        IFNE      *BLANKS
     C     SCTACD        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCFSS#        IFNE      *BLANKS
     C     SCTCO#        IFNE      *ZEROS
     C     SCTSUB        ANDNE     *ZEROS
     C     SCTFND        ANDNE     *ZEROS
     C     SCTFYR        ANDNE     *ZEROS
     C     SCTCAS        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCFCO#        IFEQ      *ZEROS
     C                   MOVE      SCFSS#        XXSS#
     C*
     C                   Z-ADD     SCFACD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C     CMKEY2        CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMCO#         QQFCO#            3
     C                   MOVE      CMSUB#        QQFSUB            5
     C                   MOVE      CMFND         QQFFND            3
     C                   MOVE      CMFYR         QQFFYR            3
     C                   MOVE      CMCASE        QQFCAS            7
     C                   MOVE      SCFSS#        QQFSS#            9
     C                   MOVE      SCFACD        QQFACD            8
     C                   END
     C                   ELSE
     C                   MOVE      SCFCO#        XXCO#
     C                   MOVE      SCFSUB        XXSUB#
     C                   MOVE      SCFFND        XXFND
     C                   MOVE      SCFFYR        XXFYR
     C                   MOVE      SCFCAS        XXCASE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         QQFSS#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFACD            8 0
     C*
     C                   MOVE      XXFACD        QQFACD
     C                   MOVE      SCFCO#        QQFCO#
     C                   MOVE      SCFSUB        QQFSUB
     C                   MOVE      SCFFND        QQFFND
     C                   MOVE      SCFFYR        QQFFYR
     C                   MOVE      SCFCAS        QQFCAS
     C                   END
     C                   END
     C*
     C     SCTCO#        IFEQ      *ZEROS
     C                   MOVE      SCTSS#        XXSS#
     C*
     C                   Z-ADD     SCTACD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C     CMKEY2        CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMCO#         QQTCO#            3
     C                   MOVE      CMSUB#        QQTSUB            5
     C                   MOVE      CMFND         QQTFND            3
     C                   MOVE      CMFYR         QQTFYR            3
     C                   MOVE      CMCASE        QQTCAS            7
     C                   MOVE      SCTSS#        QQTSS#            9
     C                   MOVE      SCTACD        QQTACD            8
     C                   END
     C                   ELSE
     C                   MOVE      SCTCO#        XXCO#
     C                   MOVE      SCTSUB        XXSUB#
     C                   MOVE      SCTFND        XXFND
     C                   MOVE      SCTFYR        XXFYR
     C                   MOVE      SCTCAS        XXCASE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         QQTSS#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTACD            8 0
     C*
     C                   MOVE      XXTACD        QQTACD
     C                   MOVE      SCTCO#        QQTCO#
     C                   MOVE      SCTSUB        QQTSUB
     C                   MOVE      SCTFND        QQTFND
     C                   MOVE      SCTFYR        QQTFYR
     C                   MOVE      SCTCAS        QQTCAS
     C                   END
     C                   END
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         QQDATE            8
     C*
     C                   CALL      'F7060RFX'
     C                   PARM                    QQFCO#
     C                   PARM                    QQFSUB
     C                   PARM                    QQFFND
     C                   PARM                    QQFFYR
     C                   PARM                    QQFCAS
     C                   PARM                    QQFSS#
     C                   PARM                    QQFACD
     C                   PARM                    QQTCO#
     C                   PARM                    QQTSUB
     C                   PARM                    QQTFND
     C                   PARM                    QQTFYR
     C                   PARM                    QQTCAS
     C                   PARM                    QQTSS#
     C                   PARM                    QQTACD
     C                   PARM                    QQDATE
     C                   PARM      'D'           QQMODE            1
     C*
     C                   Z-ADD     *ZEROS        SCFCO#
     C                   Z-ADD     *ZEROS        SCTCO#
     C                   Z-ADD     *ZEROS        SCFSUB
     C                   Z-ADD     *ZEROS        SCTSUB
     C                   Z-ADD     *ZEROS        SCFFND
     C                   Z-ADD     *ZEROS        SCTFND
     C                   Z-ADD     *ZEROS        SCFFYR
     C                   Z-ADD     *ZEROS        SCTFYR
     C                   Z-ADD     *ZEROS        SCFCAS
     C                   Z-ADD     *ZEROS        SCTCAS
     C                   MOVE      *BLANKS       SCFSS#
     C                   MOVE      *BLANKS       SCTSS#
     C                   Z-ADD     *ZEROS        SCFACD
     C                   Z-ADD     *ZEROS        SCTACD
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
