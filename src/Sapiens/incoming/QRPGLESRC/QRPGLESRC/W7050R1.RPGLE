     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7050R1 - PRINT ENDORSEMENTS
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM ALLOWS THE USER TO SUBMIT A JOB TO PRINT        *
     H*  ENDORSEMENTS ALREADY ENTERED THROUGH ENDORSEMENT MAINT.      *
     H*  CHANGES INCLUDE:                                             *
     H*  NAME CHANGE (DBA NAME/EMPLOYER NAME)                         *
     H*  ADDRESS CHANGE (PHYSICAL/MAILING)                            *
     H*  DELETION (ENTITY/LOCATION)                                   *
     H*  ADDITION OF NAME INSURED                                     *
     H*  ADDITION OF LOCATION                                         *
     H*  OTHER (FEDERAL ID#, INCREASED LIMIT, ADDITION OF OFFICER,    *
     H*        CERTIFICATE OF INSURANCE, NOMINAL/OWNERSHIP)           *
     H*                                                               *
     H*  CALLED BY: MENU                                              *
     H*                                                               *
     H*  CALLS TO:  W7050R2 - ENTITY/LOCATION MAINTENANCE             *
     H*                                                               *
     H*  CREATED: 04/03/95        PROGRAMMER: 11       PROJECT: WC410 *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 06/13/96 C:33 PROJECT WC420                                  *
     H*               ADD USER PARM OUTQ TO PARM OF CALL TO W7050R1  *
     H*                                                              *
     H****************************************************************
     FW7050F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE FUND FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  KEY LIST FOR SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
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
     C                   TIME                    DSDTTM
     C*
     C*  CANNOT EXECUTE THIS PGM IF CLIENT NOT ENROLLED IN "WCPOLS"
     C*
     C                   MOVEL     'WC'          ##KEY
     C     SYKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      '1'
     C     *IN60         OREQ      '0'
     C     WCFL03        ANDNE     'Y'                                          NOT ENROLLED,
     C                   GOTO      TERM                                         EXIT THE PGM
     C                   END
     C*
     C     STRT01        TAG
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   Load the Record Format Info For First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     *IN11         IFEQ      '1'
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKA
     C*
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C                   GOTO      STRT01
     C                   END                                                    END *INKM
     C*
     C                   END                                                    END CMDKEYS
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPFND         QQFND             3
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W7050C1'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
