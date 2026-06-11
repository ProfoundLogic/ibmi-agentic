     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F6200R -  PROMPT FOR LOG EXTRACTION PROCESSING
     H*
     H*****************************************************************
     H*                                                               *
     H* Called from:  menu                                            *
     H* Calss to:     F6200C                                          *
     H*                                                               *
     H* Created: 06/10/99    C:15     Project: KY010                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/22/00 C:15  Support: WC065/24044                           *
     H*                Added code to prevent F10 from acting like the *
     H*                Enter key.                                     *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     FF6200F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFEMEL     UF A E           K DISK
     FFELGL     UF   E           K DISK
     FFEDDL     UF   E           K DISK
     FFEDHL     UF   E           K DISK
     FFEDPL     UF   E           K DISK
     FFEDUL     UF   E           K DISK
     FFEEDL     UF   E           K DISK
     FFEEHL     UF   E           K DISK
     FFEEPL     UF   E           K DISK
     FFEEUL     UF   E           K DISK
     FFEE1L     UF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)

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
     C*   KEY LIST FOR THE USER PARAMETER FILE
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
     C*   KEY LIST FOR THE SUBMISSION MANAGEMENT FILE
     C*
     C     MEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    SCTSET
     C*
     C*   KEY LIST FOR THE LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    SCTSET
     C*
     C*   KEY LIST FOR THE 837 EXTRACT FILES
     C*
     C     EEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGEOB#
     C                   KFLD                    LGRCN
     C                   KFLD                    LGLSEQ
     C*
     C*   KEY LIST FOR THE SYSTEM TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE EXTRACT ERROR FILE
     C*
     C     E1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    SCTSET
     C                   KFLD                    XXETYP
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    MEEXDT        XXEXDT
     C     *LIKE         DEFINE    E1ETYP        XXETYP
     C                   MOVE      'E'           XXETYP
     C*
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      '1'
     C                   Z-ADD     *ZERO         UPCO#
     C                   END
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       CONAME
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      Q1USER        SCUSER
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
     C                   MOVE      *BLANKS       SCJST
     C                   MOVE      '837'         SCTSET
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
     C*   Convert all dates to MMDDCCYY.
     C*
     C                   EXSR      DATSBR
     C***********************
     C*   F3 = EXIT         *
     C***********************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C***********************
     C*   F10=SEARCH        *
     C***********************
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C*  Jurisdiction state search
     C*
     C     HFLD          WHENEQ    'SCJST'
     C                   CALL      'SY600R'
     C                   PARM      '  '          ##JST             2
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        CABEQ     'A'           TERM
     C     ##RTCD        CABEQ     'B'           STRT
     C     ##JST         CABEQ     '  '          STRT
     C*
     C     ##RTCD        IFEQ      '*'
     C                   MOVEL(P)  ##JST         SCJST
     C                   SETON                                        30        pos cursor
     C                   GOTO      STRT
     C                   ENDIF                                                  ##RTCD = '*'
     C*
     C* Transaction Set ID search
     C*
     C     HFLD          WHENEQ    'SCTSET'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'TSET'        ##TBID            4
     C*
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT                     31
     C     ##RRN         CABEQ     *ZEROS        STRT                     31
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     FTGTP                              99
     C     *IN99         CABEQ     *ON           STRT                     31
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCTSET
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN <> *ZEROS
     C*
     C                   ENDSL                                                            '
     C*
     C*** BEGIN ADD *** 08/22/00 **************************************
     C                   GOTO      STRT
     C*** END ADD ***** 08/22/00 **************************************
     C                   ENDIF                                                  *INKJ = '1'
     C**************************
     C*  Process help request  *
     C**************************
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   GOTO      STRT
     C                   END
     C**********************************
     C*  F13=Change user parameters    *
     C**********************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C                   GOTO      STRT01
     C                   END
     C*
     C*** BEGIN ADD *** 08/22/00 **************************************
     C                   GOTO      STRT
     C*** END ADD ***** 08/22/00 **************************************
     C                   END
     C*================================================================
     C*   PROCESS REQUEST SCREEN    (ERROR CHEKCING)
     C*
     C     SCR01T        TAG
     C*
     C*  Validate the jurisdiction state
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     20
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  Validate the extract date
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  Validate the transaction set ID.
     C*
     C                   MOVEL(P)  'TSET'        XXTBID
     C                   MOVEL(P)  SCTSET        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         CABEQ     *ON           STRT                     25
     C*
     C*   An extract cannot be re-submitted if the data has been downloaded
     C*   or an extract is already in progress.
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C     MEKY          CHAIN(N)  PME01                              99
     C     *IN99         IFEQ      *OFF
     C     MEOTDT        IFNE      0
     C     MESTAT        OREQ      'P'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Create or update the extract management record.
     C*
     C     MEKY          CHAIN     PME01                              99
     C     *IN99         IFEQ      '0'
     C*
     C*   Reset the Log History records for this extract date and
     C*     delete the corresponding extract files.
     C*
     C     LGKY          CHAIN     PLG01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C     LGSTS         IFEQ      'EX'
     C                   MOVE      'EC'          LGSTS
     C                   MOVE      *ZEROS        LGEXDT
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    PLG01
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PED01                              99
     C                   END
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PEH01                              99
     C                   END
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PEP01                              99
     C                   END
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PEU01                              99
     C                   END
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PDD01                              99
     C                   END
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PDH01                              99
     C                   END
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PDP01                              99
     C                   END
     C*
     C     *IN99         DOUEQ     *ON
     C     EEKY          DELETE    PDU01                              99
     C                   END
     C*
     C                   ENDIF
     C*
     C     LGKY          READE     PLG01                                  99
     C                   ENDDO
     C*
     C                   MOVE      'P'           MESTAT
     C                   MOVEL     Q1USER        MEUSER
     C                   MOVEL     Q1PGM         MEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        METIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MEDT
     C                   UPDATE    PME01
     C*
     C                   ELSE
     C*
     C                   MOVEL     XXCO#         MECO#
     C                   MOVEL     SCJST         MEJST
     C                   Z-ADD     XXEXDT        MEEXDT
     C                   MOVE      SCTSET        METSET
     C                   Z-ADD     0             MEOTDT
     C                   Z-ADD     0             MEOTTM
     C                   Z-ADD     0             MEMTCD
     C                   Z-ADD     0             MEARCV
     C                   Z-ADD     0             MEAENT
     C                   Z-ADD     0             ME837T
     C                   Z-ADD     0             ME837S
     C                   MOVE      'P'           MESTAT
     C                   MOVEL     Q1USER        MEUSER
     C                   MOVEL     Q1PGM         MEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        METIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MEDT
     C                   MOVEL     Q1USER        MEIUSR
     C                   MOVEL     Q1PGM         MEIPGM
     C                   Z-ADD     DSTIME        MEITIM
     C                   MOVEL     MEDT          MEIDT
     C                   WRITE     PME01
     C*
     C                   ENDIF
     C*
     C*   Clear the Extract Error File records based on CO#/JST/TSET/ETYP.
     C*
     C     *IN99         DOUEQ     *ON
     C     E1KY          DELETE    PE101                              99
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCEXDT        QQEXDT
     C                   CALL      'F6200C'
     C                   PARM                    QQCO#             3
     C                   PARM                    SCJST
     C                   PARM                    QQEXDT            8
     C                   PARM                    SCTSET
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*================================================================
     C* DATSBR - Date check subrountine
     C*  This subroutine is used to put all screen dates in the correct
     C*  format so the edit code will work correct.
     C*================================================================
     C     DATSBR        BEGSR
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   ENDSR
