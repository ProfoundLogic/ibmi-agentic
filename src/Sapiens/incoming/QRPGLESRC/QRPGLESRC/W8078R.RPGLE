     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8078R -  PROMPT FOR EDI ACKNOWLEDGMENT PROCESSING
     H*
     H*****************************************************************
     H*
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  05/30/18 C:180 PROJECT WC2515
     H*                 Add Release 3.1 processing.
     H*
     H*  06/17/08 C:180 SUPPORT 63841
     H*                 Flag SUBMIT job for R3 processing when
     H*                 applicable.
     H*
     H*  12/07/07 C:53  PROJECT: WC1177
     H*                 R3 ACKNOWLEDGMENT PROCESSING.
     H*
     H*  06/27/05 C:53  SUPPORT: WC129/4774
     H*                 INIT TIME AND DATE FIELDS TO BLANKS IN ORDER
     H*                 TO PERFORM THE SEARCH CORRECTLY IF NOTHING IS
     H*                 ENTERED.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 08/10/99  C:118  PROJECT:  WC468                              *
     H*                  Made V3 chgs to display file.  Added search  *
     H*                  on Jurisdiction state.                       *
     H*                                                               *
     H*****************************************************************
     FW8078F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FWXA1L     IF   E           K DISK
     F                                     IGNORE(PA101)
     F*** BEG ADD *** 12/07/07 **********************************
     FWMIXL     IF   E           K DISK
     F*** END ADD *** 12/07/07 **********************************

     D*________________________________________________________________
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*________________________________________________________________
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*         KEY LIST FOR THE ACKNOWLEDGMENT HEADER FILE
     C*
     C     HAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XCOTDT
     C                   KFLD                    XCOTTM
     C*
     C*** BEG ADD *** 12/07/07 **********************************
     C*
     C***        WMIXL - EDI Claims Control Maintenance File
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    KKTSET
     C*
     C*** END ADD *** 12/07/07 **********************************
     C*
     C*   ENTRY PARM LIST
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    HAOTDT        XCOTDT
     C     *LIKE         DEFINE    HAOTTM        XCOTTM
     C                   MOVE      *ZEROS        XXDATE            8 0
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         UPCO#
     C                   END
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C***** BEGIN DEL **** 08/10/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/10/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  INITIALIZE STATE AND ORIGINAL TRANSMISSION DATE/TIME
     C*
     C                   MOVE      *BLANK        SCJST
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SCOTDT
     C                   MOVE      DSTIME        SCOTTM
     C*
     C*** BEG ADD *** 06/27/05 ****************************
     C                   MOVE      *BLANK        SCOTDT
     C                   MOVE      *BLANK        SCOTTM
     C*** END ADD *** 06/27/05 ****************************
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
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 05 = SEARCH HEADERS
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C***** BEGIN ADD **** 08/10/99 ***********************************
     C*  JURISDICTION STATE TABLE SEARCH
     C*
     C     HFLD          IFEQ      'SCJST'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'JRST'        QQTBID            4
     C                   PARM      *BLANK        QQKEY            10
     C                   PARM      ' '           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   ENDIF                                                  QQRTCD='A'
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='B'
     C*
     C     QQKEY         IFEQ      *BLANKS
     C                   GOTO      STRT
     C                   ENDIF                                                  QQKEY=BLKS
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCJST
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='*'
     C*
     C                   ENDIF                                                  HFLD=SCJST
     C*
     C***** END   ADD **** 08/10/99 ***********************************
     C*
     C*     THE JURISDICTION STATE MUST BE ENTERED AND VALID, BECAUSE
     C*     THIS SEARCH ONLY DISPLAYS A LIST FOR ONE JURS STATE AT A TIME.
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCJST         QQJST             2
     C                   MOVE      SCOTDT        QQDATE            8
     C                   MOVE      SCOTTM        QQOTTM            6
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'W0878R'
     C                   PARM                    QQCO#
     C                   PARM                    QQJST
     C                   PARM                    QQDATE
     C                   PARM                    QQOTTM
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQJST         CABEQ     ' '           STRT
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQDATE        SCOTDT
     C                   MOVE      QQOTTM        SCOTTM
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
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
     C***** BEGIN DEL **** 08/10/99 ***********************************
     C*********            WRITESCR99
     C***** END   DEL **** 08/10/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*     CF13
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 08/10/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/10/99 ***********************************
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*
     C*   VALIDATE JURISDICTION STATE IS IN TABLE FILE
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   VALIDATE THE TRANSMISSION DATE
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE THE HEADER RECORD.
     C*
     C                   MOVE      SCOTDT        XCDAT4            4            YEAR
     C                   MOVEL     XCDAT4        XCOTDT
     C                   MOVEL     SCOTDT        XCDAT4                          M-D
     C                   MOVE      XCDAT4        XCOTDT
     C                   MOVE      SCOTTM        XCOTTM
     C     HAKY          CHAIN     PHA01                              11
     C     *IN11         IFEQ      *ON
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** BEG ADD *** 12/07/07 **********************************
     C*
     C                   MOVE      *BLANKS       KKTSET            3
     C     IXKY          CHAIN     PIX01                              60
     C*
     C*** DUE TO ROES SENDING R3 ACK'S/RAK1'S IN THE R1 FORMAT,
     C*** WE WILL NEED TO OVERRIDE TO INSURE THE CORRECT PROCESSING
     C*** PROGRAM IS CALLED.
     C*
      ***** BEGIN CHANGE **** 06/17/08 *******************************
      *
     C****       HATSET    IFEQ 'AK1'
     C*****      HATSET    OREQ 'AR1'
     C*****                MOVE '1'       IXRLS
     C****                 ENDIF
      *
     C                   SELECT
      *
     C     HATSET        WHENEQ    'AK1'
     C     HATSET        OREQ      'AR1'
     C                   MOVE      '1'           IXRLS
      *
     C     HATSET        WHENEQ    'AKC'
     C                   MOVE      '3'           IXRLS
      *
      *** Begin Add *** 05/30/18 *************************************
     C     HATSET        WHENEQ    'ARC'
     C                   MOVE      '3'           IXRLS
      *** End   Add *** 05/30/18 *************************************
     C                   OTHER
     C                   GOTO      TERM
     C                   ENDSL
      ***** END CHANGE ****** 06/17/08 *******************************
     C*
     C*** END ADD *** 12/07/07 **********************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVEL     SCJST         QQJST
     C                   MOVE      XCOTDT        QQOTDT            8
     C                   MOVE      SCOTTM        QQOTTM
     C                   CALL      'W8078C'
     C                   PARM                    QQJST
     C                   PARM                    QQOTDT
     C                   PARM                    QQOTTM
     C                   PARM                    QQCO#
     C                   PARM                    UPOUTQ
     C*** BEG ADD *** 12/07/07 **********************************
     C                   PARM      IXRLS         QQRLS             1
     C*** END ADD *** 12/07/07 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
