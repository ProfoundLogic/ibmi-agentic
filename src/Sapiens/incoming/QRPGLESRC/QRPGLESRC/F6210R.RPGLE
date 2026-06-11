     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F6210R -  PROMPT FOR EDI SUBMISSION PROCESSING
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE                        *
     H*                    C:58  PROJECT: WC468                       *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/11/09 C:232 PROJECT: SY184 PRISS#12                       *
     H*                 CURSOR SHOULD POSITION TO ORIGINAL TRANSMISSION
     H*                 DATE AFTER SEARCH ON STATE FIELD.             *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     FF6210F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFEMFL     UF A E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFELGL3    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE TRANSMISSION MANAGEMENT FILE
     C*
     C     MFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    SCTSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    SCOTTM
     C*
     C*   KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  Key list for the 837 Log History file.
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
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
     C     *LIKE         DEFINE    MFOTDT        XXOTDT
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    LGFRDT        XXFRDT
     C     *LIKE         DEFINE    LGTODT        XXTODT
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         UPCO#
     C                   ENDIF                                                  *IN60=1
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       CONAME
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
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
     C                   MOVE      '837'         SCTSET
     C                   MOVE      *ZEROS        SCOTDT
     C                   MOVE      DSTIME        SCOTTM
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF=SCR01
     C*** BEG ADD *** 08/11/09 **********************************
     C                   MOVE      '0'           *IN81
     C                   MOVE      '0'           *IN82
     C*** END ADD *** 08/11/09 **********************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=1
     C*
     C*   F10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C*     Search for Jurisdiction State
     C*
     C     HFLD          WHENEQ    'SCJST'
     C                   CALL      'SY600R'
     C                   PARM      '  '          QQJST
     C                   PARM      ' '           QQRTCD
     C*
     C*** BEG ADD *** 08/11/09 **********************************
     C     SCJST         IFEQ      '  '
     C     QQJST         ANDEQ     '  '
     C                   MOVE      '1'           *IN81
     C                   ENDIF
     C                   MOVE      '1'           *IN82
     C*** END ADD *** 08/11/09 **********************************
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQJST         CABEQ     ' '           STRT
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQJST         SCJST
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD=*
     C*
     C*     Search on original transmission date
     C*
     C     HFLD          WHENEQ    'SCOTDT'
     C     HFLD          OREQ      'SCOTTM'
     C*
     C*     The jurisdiction state and trans set ID must be entered
     C*     to use this search.
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=1
     C*
     C     SCTSET        CABEQ     *BLANKS       STRT                     25
     C                   MOVEL(P)  'TSET'        XXTBID
     C                   MOVEL(P)  SCTSET        XXKEY
     C     GTKY          CHAIN     PGT01                              26
     C     *IN26         CABEQ     *ON           STRT
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCOTDT        QQOTDT
     C                   MOVE      SCOTTM        QQOTTM
     C                   CALL      'F6213R'
     C                   PARM                    QQCO#             3
     C                   PARM      SCJST         QQJST             2
     C                   PARM      SCTSET        QQTSET            3
     C                   PARM                    QQOTDT            8
     C                   PARM                    QQOTTM            6
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQJST         CABEQ     ' '           STRT
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     QQOTDT        SCOTDT
     C                   MOVE      QQOTTM        SCOTTM
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C*     Search on transaction set ID
     C*
     C     HFLD          WHENEQ    'SCTSET'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           @@CALL            1
     C                   PARM      *BLANK        @@KC              1
     C                   PARM      *BLANK        @@KL              1
     C                   PARM      *ZERO         @@RRN             9 0
     C                   PARM      'TSET'        XXTBID
     C*
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN     FTGTP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTSET
     C                   ENDIF                                                  *IN60=OFF
     C                   ENDIF                                                  @@RRN<>0
     C*
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ=1
     C*
     C*  F13=CHANGE USER PARMS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM=1
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
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10=1
     C*
     C                   ENDIF                                                  *IN11=1
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
     C                   ENDIF                                                  SCTSTF<>BLK
     C*
     C**  VALIDATE JURISDICTION STATE IS IN TABLE FILE
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C**  VALIDATE THE TRANSMISSION DATE
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOTDT
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR<>0
     C*
     C**  VALIDATE TRANSACTION SET ID.
     C*
     C                   MOVEL(P)  'TSET'        XXTBID
     C                   MOVEL(P)  SCTSET        XXKEY
     C     GTKY          CHAIN     PGT01                              26
     C     *IN26         CABEQ     *ON           STRT
     C*
     C*   WRITE/UPDATE THE SUBMISSION MANAGEMENT RECORD.
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXOTDT
     C     MFKY          CHAIN     PMF01                              11
     C     *IN11         IFEQ      *OFF
     C*
     C                   MOVE      'R'           MFSTAT
     C                   MOVE      SCTSTF        MFTSTF
     C*PUIMOD
     C*                  Z-ADD     UDATE         SRDTI
     C                   EVAL      SRDTI = %Int(%Char(%Date():*MDY0))
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MFLTDT
     C                   TIME                    MFLTTM
     C     MFOTDT        IFEQ      *ZEROS
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MFOTDT
     C                   MOVE      SCOTTM        MFOTTM
     C                   ENDIF                                                  MFOTDT=0
     C                   UPDATE    PMF01
     C*
     C                   ELSE
     C*
     C                   MOVE      XXCO#         MFCO#
     C                   MOVE      SCJST         MFJST
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MFOTDT
     C                   MOVE      SCOTTM        MFOTTM
     C                   MOVE      SCTSET        MFTSET
     C                   MOVE      *ZEROS        MFARCV
     C                   MOVE      *ZEROS        MFAENT
     C                   EXSR      DATRNG
     C                   MOVE      XXFRDT        MFRFDT
     C                   MOVE      XXTODT        MFRTDT
     C                   MOVEL     Q1USER        MFIUSR
     C                   MOVEL     Q1PGM         MFIPGM
     C                   MOVE      MFOTDT        MFIDT
     C                   MOVE      MFOTTM        MFITIM
     C                   MOVE      MFOTDT        MFLTDT
     C                   MOVE      MFOTTM        MFLTTM
     C                   MOVE      'A'           MFSTAT
     C                   MOVE      SCTSTF        MFTSTF
     C                   WRITE     PMF01
     C                   ENDIF                                                  *IN11=0FF
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCOTDT        QQOTDT
     C                   MOVE      SCOTTM        QQOTTM
     C                   CALL      'F6210C'
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    SCJST
     C                   PARM                    SCTSET
     C                   PARM                    QQOTDT
     C                   PARM                    QQOTTM
     C                   PARM                    UPOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*----------------------------------------------------------------
     C*  DATRNG - This subroutine determines the reporting period
     C*    (date range) of this transmission.
     C*----------------------------------------------------------------
     C*
     C     DATRNG        BEGSR
     C*
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             XXTODT
     C*
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     LGSTAT        IFNE      'H'
     C     LGSTS         ANDEQ     'EX'
     C*
     C     XXFRDT        IFEQ      0
     C     LGFRDT        ORLT      XXFRDT
     C     LGFRDT        ANDNE     0
     C                   Z-ADD     LGFRDT        XXFRDT
     C                   ENDIF
     C*
     C     LGTODT        IFGT      XXTODT
     C                   Z-ADD     LGTODT        XXTODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
