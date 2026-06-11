     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8073R -  PROMPT FOR EDI SUBMISSION PROCESSING
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE                        *
     H*                    C:58  PROJECT: WC468                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  03/24/10 C:232 Project: 139WNN
     H*                 Added one parameter QQSFID in call of W8073C.
     H*
     H*  09/03/09 C:53  Project: WC1839
     H*                 Add EDI Format Identifier processing
     H*
     H*  07/24/06 C:53  PROJECT: WC1434
     H*                 ROES ASP INTEGRATION (RAK1)
     H*
     H*  02/11/04 C:53  Support: WC055/41160
     H*                 Added edit for an entry on the time to prevent
     H*                 errors in the acknowledgement process.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  07/15/99  C:118  PROJECT:  WC468                             *
     H*                   Made V3 changes to display file.  Added F10=*
     H*                   Search on Jurisdiction state.               *
     H*****************************************************************
     FW8073F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMM2L     UF A E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** Begin add *** 09/03/09 ******************************************
     FWTGTL     IF   E           K DISK
     F*** END add *** 09/03/09 **************************************  ****

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
     C*   KEY LIST FOR THE TRANSMISSION MANAGEMENT FILE
     C*
     C     M2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXTSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C*** BEG ADD *** 09/03/09 **************************************  ****
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END ADD *** 09/03/09 **************************************  ****
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
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    M2OTDT        XXOTDT
     C     *LIKE         DEFINE    M2OTTM        XXOTTM
     C     *LIKE         DEFINE    M2TSET        XXTSET
     C*** BEG ADD *** 09/03/09 **************************************  ****
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*** END ADD *** 09/03/09 **************************************  ****
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
     C***** BEGIN DEL **** 07/15/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 07/15/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  INITIALIZE STATE AND EXTRACT DATE
     C*
     C                   MOVE      *BLANK        SCJST
     C                   MOVE      *ZEROS        SCOTDT
     C                   MOVE      DSTIME        SCOTTM
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
     C*   COMMAND 10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C***** BEGIN ADD **** 07/15/99 ***********************************
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
     C*
     C*
     C***** END   ADD **** 07/15/99 ***********************************
     C*     THE JURISDICTION STATE MUST BE ENTERED AND VALID, BECAUSE
     C*     THIS SEARCH ONLY DISPLAYS A LIST FOR ONE JURS STATE AT A TIME.
     C*
     C*** Begin add *** 09/03/09 ******************************************
     C     HFLD          IFEQ      'SCOTDT'
     C     HFLD          OREQ      'SCOTTM'
     C*** End   add *** 09/03/09 ******************************************
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
     C                   MOVE      SCOTDT        QQOTDT            8
     C                   MOVE      SCOTTM        QQOTTM            6
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'W0873R'
     C                   PARM                    QQCO#
     C                   PARM                    QQJST
     C                   PARM                    QQOTDT
     C                   PARM                    QQOTTM
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN41
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQJST         CABEQ     ' '           STRT
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQJST         SCJST
     C                   MOVEL     QQOTDT        SCOTDT
     C                   MOVE      QQOTTM        SCOTTM
     C***** BEGIN ADD **** 07/15/99 ***********************************
     C                   MOVE      *ON           *IN42
     C                   MOVE      *OFF          *IN41
     C***** END   ADD **** 07/15/99 ***********************************
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/03/09 ******************************************
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCEDIF'
     C****************     MOVEL##DATA    QQPRDS 40
     C                   MOVEL(P)  'EDIF'        KKTBID
     C                   MOVE(P)   *BLANK        KKKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    KKTBID
     C                   PARM                    KKKEY
     C                   PARM                    QQRTCD            1
     C                   MOVE      *ON           *IN43                          POS CUSR
     C                   MOVE(P)   *BLANKS       SCEDFD
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  KKKEY         SCEDIF
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCEDFD
     C                   MOVE      *OFF          *IN43                          POS CUSR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** End   add *** 09/03/09 ******************************************
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
     C***** BEGIN DEL **** 07/15/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 07/15/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*     CF13
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 07/15/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 07/15/99 ***********************************
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C**  VALIDATE TEST FLAG
     C*
     C     SCTSTF        IFNE      *BLANKS
     C     SCTSTF        ANDNE     'Y'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C**  VALIDATE JURISDICTION STATE IS IN TABLE FILE
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C**  VALIDATE THE TRANSMISSION DATE
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOTDT
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEG ADD *** 02/11/04 ****************************
     C*
     C**  TRANSMISSION MUST HAVE AN ENTRY
     C*
     C     SCOTTM        IFEQ      0
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*** END ADD *** 02/11/04 ****************************
     C*
     C**  VALIDATE TRANSACTION SET ID.
     C*
     C     SCTSET        IFNE      '1'
     C     SCTSET        ANDNE     'A'
     C     SCTSET        ANDNE     'B'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEG ADD *** 09/03/09 **************************************  ****
     C*
     C*  The EDI Format ID Must be in the table file if entered.
     C*  If not entered, check the fund file to see if it is required.
     C*
     C     SCEDIF        IFNE      *BLANKS
     C                   MOVEL(P)  'EDIF'        KKTBID
     C                   MOVEL(P)  SCEDIF        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCEDFD
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCEDFD
     C                   SETON                                          27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  If not entered, check the fund file to see if it is required.
     C                   ELSE
     C*
     C* CALL API TO CHECK THE FUND.
     C*
     C                   CALL      'WA075R'
     C                   PARM                    XXCO#
     C                   PARM      000           XXFND             3 0
     C                   PARM      *BLANK        QQRTCD
     C     QQRTCD        IFEQ      'F'                                          CODE FOUND
     C                   SETON                                          28      ENTRY REQ'D
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 09/03/09 **************************************  ****
     C*
     C*   WRITE/UPDATE THE SUBMISSION MANAGEMENT RECORD.
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXOTDT
     C                   MOVE      SCOTTM        XXOTTM
     C     SCTSET        IFEQ      'B'                                          148 PROCESSING
     C     SCTSET        OREQ      '1'
     C                   MOVEL     '148'         XXTSET
     C     M2KY          CHAIN     PM201                              11
     C     *IN11         IFEQ      *OFF
     C                   MOVE      'R'           M2STAT
     C                   MOVE      SCTSTF        M2TSTF
     C*PUIMOD
     C*                  Z-ADD     UDATE         SRDTI
     C                   EVAL      SRDTI= %Int(%Char(%Date():*MDY0))
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2LTDT
     C                   TIME                    M2LTTM
     C     M2OTDT        IFEQ      *ZEROS
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2OTDT
     C                   MOVE      SCOTTM        M2OTTM
     C                   ENDIF
     C                   UPDATE    PM201
     C                   ELSE
     C                   MOVE      XXCO#         M2CO#
     C                   MOVE      SCJST         M2JST
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2OTDT
     C                   MOVE      SCOTTM        M2OTTM
     C                   MOVE      XXTSET        M2TSET
     C                   MOVE      *ZEROS        M2MTCD
     C                   MOVE      *ZEROS        M2ARCV
     C                   MOVE      *ZEROS        M2AENT
     C                   MOVEL     Q1USER        M2IUSR
     C                   MOVEL     Q1PGM         M2IPGM
     C                   MOVE      M2OTDT        M2IDT
     C                   MOVE      M2OTTM        M2ITM
     C                   MOVE      *ZEROS        M2148T
     C                   MOVE      *ZEROS        M2A49T
     C                   MOVE      *ZEROS        M2148S
     C                   MOVE      *ZEROS        M2A49S
     C                   MOVE      M2OTDT        M2LTDT
     C                   MOVE      M2OTTM        M2LTTM
     C                   MOVE      'A'           M2STAT
     C                   MOVE      SCTSTF        M2TSTF
     C*** BEG ADD *** 09/03/09 **************************************  ****
     C                   MOVE      SCEDIF        M2EDIF
     C*** END ADD *** 09/03/09 **************************************  ****
     C                   WRITE     PM201
     C                   ENDIF
     C*
     C                   ENDIF                                                  148 PROCESSING
     C*
     C     SCTSET        IFEQ      'B'                                          A49 PROCESSING
     C     SCTSET        OREQ      'A'
     C                   MOVEL     'A49'         XXTSET
     C*
     C*** BEG ADD *** 07/24/06 **************************************
     C     SCTSET        IFEQ      'B'
     C     SCOTTM        ADD       1             XXOTTM
     C                   ENDIF
     C*** END ADD *** 07/24/06 **************************************
     C*
     C     M2KY          CHAIN     PM201                              11
     C     *IN11         IFEQ      *OFF
     C                   MOVE      'R'           M2STAT
     C                   MOVE      SCTSTF        M2TSTF
     C*PUIMOD
     C*                  Z-ADD     UDATE         SRDTI
     C                   EVAL      SRDTI= %Int(%Char(%Date():*MDY0))
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2LTDT
     C                   TIME                    M2LTTM
     C*** BEG ADD *** 07/24/06 **************************************
     C                   ADD       1             M2LTTM
     C*** END ADD *** 07/24/06 **************************************
     C     M2OTDT        IFEQ      *ZEROS
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2OTDT
     C*** BEG ADD *** 07/24/06 **************************************
     C     SCTSET        IFEQ      'B'
     C     SCOTTM        ADD       1             M2OTTM
     C                   ELSE
     C*** END ADD *** 07/24/06 **************************************
     C                   MOVE      SCOTTM        M2OTTM
     C*** BEG ADD *** 07/24/06 **************************************
     C                   ENDIF
     C*** END ADD *** 07/24/06 **************************************
     C                   ENDIF
     C                   UPDATE    PM201
     C                   ELSE
     C                   MOVE      XXCO#         M2CO#
     C                   MOVE      SCJST         M2JST
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2OTDT
     C*** BEG ADD *** 07/24/06 **************************************
     C     SCTSET        IFEQ      'B'
     C     SCOTTM        ADD       1             M2OTTM
     C                   ELSE
     C*** END ADD *** 07/24/06 **************************************
     C                   MOVE      SCOTTM        M2OTTM
     C*** BEG ADD *** 07/24/06 **************************************
     C                   ENDIF
     C*** END ADD *** 07/24/06 **************************************
     C                   MOVE      XXTSET        M2TSET
     C                   MOVE      *ZEROS        M2MTCD
     C                   MOVE      *ZEROS        M2ARCV
     C                   MOVE      *ZEROS        M2AENT
     C                   MOVEL     Q1USER        M2IUSR
     C                   MOVEL     Q1PGM         M2IPGM
     C                   MOVE      M2OTDT        M2IDT
     C                   MOVE      M2OTTM        M2ITM
     C                   MOVE      *ZEROS        M2148T
     C                   MOVE      *ZEROS        M2A49T
     C                   MOVE      *ZEROS        M2148S
     C                   MOVE      *ZEROS        M2A49S
     C                   MOVE      M2OTDT        M2LTDT
     C                   MOVE      M2OTTM        M2LTTM
     C                   MOVE      'A'           M2STAT
     C                   MOVE      SCTSTF        M2TSTF
     C*** BEG ADD *** 09/03/09 **************************************  ****
     C                   MOVE      SCEDIF        M2EDIF
     C*** END ADD *** 09/03/09 **************************************  ****
     C                   WRITE     PM201
     C                   ENDIF
     C*
     C                   ENDIF                                                  A49 PROCESSING
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVEL     SCJST         QQJST
     C                   MOVE      SCOTDT        QQOTDT
     C                   MOVE      SCOTTM        QQOTTM
     C                   CALL      'W8073C'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQRTCD
     C                   PARM                    QQJST
     C                   PARM                    SCTSET
     C                   PARM                    QQOTDT
     C                   PARM                    QQOTTM
     C                   PARM                    QQCO#
     C                   PARM                    UPOUTQ
     C*** BEG ADD *** 09/03/09 **************************************  ****
     C                   PARM                    SCEDIF
     C*** END ADD *** 09/03/09 **************************************  ****
     C*** BEG ADD *** 03/24/10 **************************************  ****
     C                   PARM      *BLANKS       QQSFID            2
     C*** END ADD *** 03/24/10 **************************************  ****
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
