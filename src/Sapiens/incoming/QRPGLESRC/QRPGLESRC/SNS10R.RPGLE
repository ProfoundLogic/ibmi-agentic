     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: SNS10R                                            *
     H*    DESCRIPTION: PROMPT FOR NON-SIS REFERENCE#                 *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      SC010R - DARTS                                           *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  SCR01:                                                       *
     H*  25     INVALID INPUT                                         *
     H*  26     MCR0503 - FIRST TIME FLAG MUST BE 'Y' OR 'N'          *
     H*****************************************************************
     FSNS10F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FNONSISRF  IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D PGMTTL          C                   CONST('NON-SIS Reference Im-
     D                                     age Document Lookup')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the NON-SIS File
     C*
     C     SISKY         KLIST
     C                   KFLD                    KKNSIS           20
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
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
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     PGMTTL        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C******************************************************************
     C* Process All Command Keys
     C************************************
     C* F3 = EXIT                        *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C****************************
     C* CHOOSE PROCESSING        *
     C****************************
     C     SCR01T        TAG
     C*
     C                   MOVEL     SCNSIS        KKNSIS
     C     SISKY         CHAIN     NONSISR                            60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C* Call DARTS and pass in Ref# & Key Name
     C*
     C                   CALL      'SC010R'
     C                   PARM      SCNSIS        XXKEY            40
     C                   PARM      'NONSIS'      XXPNAM           10
     C*
     C                   CLEAR                   SCNSIS
     C*
     C                   GOTO      STRT
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
