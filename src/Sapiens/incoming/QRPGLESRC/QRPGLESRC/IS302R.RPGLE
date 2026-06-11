     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS302R - WORK WITH ACTION TYPES                          *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF FAX ROUTING ACTION ENTRIES *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTION         *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMRTL   (PRT01): TSID  - VIEW 1                            *
     H*    IMRTL1  (PRT01): DESC  - VIEW 2                            *
     H*****************************************************************
     FIS302F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F*
     FIMRTAL    UF   E           K DISK

     D AC              S             12    DIM(6) CTDATA PERRCD(1)
     D AD              S             40    DIM(6) ALT(AC)
     D*
     D NOLNK           C                   CONST('No action entries ')
     D                 DS
     D  SCDESC                 1     40
     D  XXIMGQ                23     32
     D  XXOUTQ                23     32
     D  XXMSGQ                 8     17
     D  XXKEY                 16     40
     D  XXIADR                16     40
     D  XXRCVP                13     22
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C     RTKY          KLIST
     C                   KFLD                    XXTSID
     C*
     C*
     C     RTKY1         KLIST
     C                   KFLD                    XXTSID
     C                   KFLD                    XXKSEQ            5 0
      **
     C*
     C*
     C     RTKYD         KLIST
     C                   KFLD                    XXTSID
     C                   KFLD                    SCKSEQ
      **
     C     *ENTRY        PLIST
     C                   PARM                    QQTSID           20
     C                   PARM                    QQDESC           40
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQTSID        XXTSID           20
     C                   MOVEL     QQDESC        SCDSC
     C                   MOVEL     QQTSID        SCTSID
     C*
      /Free
       scPrDs = 'Work with Routing Actions';
      /End-Free
     C*
      *------------------------*
     C*
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     RTKY          SETLL     IMRTAL                             60
     C*
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CA
     C                   ENDIF
     C                   MOVE      '0'           *IN01
     C*
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
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
     C                   MOVE      *OFF          *IN10
     C                   END                                                    END *IN10
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   MOVE      #@$2A         RRN2A
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C****************************************
     C*   *INKC -  EXIT                      *
     C****************************************
     C     *INKC         IFEQ      '1'                                          EXIT
     C                   MOVEL     'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P
     C*
     C****************************************
     C*   *INKF -  1st Display window to select action type *
      *            then display the ADD Screen (Call a Program)
      *
     C****************************************
     C     *INKF         IFEQ      '1'                                          ROLLUP
     C*
     C                   CALL      'IS303R'
     C                   PARM      *BLANK        Q#SEL             1
     C                   PARM      *BLANK        Q#RTC             1
     C*
     C     Q#RTC         CABEQ     'A'           TERM
     C     Q#RTC         CABEQ     'B'           TOP
     C*
     C     Q#RTC         IFEQ      *BLANK
     C     Q#SEL         ANDNE     *BLANK
     C     'IS303R'      CAT       Q#SEL         PN                7
     C                   CALL      PN
     C                   PARM                    SCTSID
     C                   PARM      *ZEROS        XXKSQ#            5 0              0 FOR CREATE
     C                   PARM                    SCDSC
     C                   PARM      *BLANK        Q#RTC
     C*
     C     Q#RTC         CABEQ     'A'           TERM
     C     Q#RTC         CABEQ     'B'           TOP
     C*
     C                   ENDIF
     C*
     C                   GOTO      TOP
     C                   END                                                    END *INKF        P
     C*
     C*
     C*
     C****************************************
     C*   *INKL -  Cancel                    *
     C****************************************
     C     *INKL         IFEQ      '1'                                          EXIT
     C                   MOVEL     'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C                   READC     SCR01SA                                50
     C     *IN50         IFEQ      '0'
     C*
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '2'
     C     SCSEL         ANDNE     '4'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01SA
     C*
     C                   GOTO      STRT
     C                   ELSE                                                   USER BLANK OUT SELCT
     C     SCSEL         IFEQ      ' '
     C                   UPDATE    SCR01SA
     C                   GOTO      CKSEL
     C                   END                                                    END EQ ' '
     C                   END                                                    END EQ '1'
     C*
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** Edit Entry
     C     SCSEL         IFEQ      '2'
      *
     C                   MOVE      ' '           QQRTCD
     C*
     C     SC#           IFNE      *BLANKS
     C     'IS303R'      CAT       SC#           PN                7
     C                   CALL      PN
     C                   PARM                    SCTSID
     C                   PARM      SCKSEQ        XXKSQ#            5 0              0 FOR CREATE
     C                   PARM                    SCDSC
     C                   PARM      *BLANK        Q#RTC
     C*
     C     Q#RTC         CABEQ     'A'           TERM
     C     Q#RTC         CABEQ     'B'           TOP
     C                   ENDIF
     C*
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C                   UPDATE    SCR01SA
     C*
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  ELSE EQ '2'
      * * * Remove Entry
     C     SCSEL         IFEQ      '4'
     C***
     C***
     C     RTKYD         DELETE    IMRTAL                             60
     C                   MOVE      ' '           QQRTCD
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C                   UPDATE    SCR01SA
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  ELSE EQ '2'
     C****
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE                                                   ELSE 0 *IN50
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN50
     C                   GOTO      STRT
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2A             5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01  '     Q2RECF
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANK        SCSEL
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C     RTKY          READE     IMRTAL                                 60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2A
     C                   MOVEL     RTACTC        SCACTC
     C*** Determine the Description for this entry and move to SCDESC
     C                   MOVE      *BLANKS       SC#
     C                   Z-ADD     1             C                 3 0
     C     SCACTC        LOOKUP    AC(C)                                  66
     C     *IN66         IFEQ      *ON
     C*** Fill in the Needed fields
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     AD(C)         SCDESC
     C                   MOVE      C             SC#
     C     C             IFEQ      1
     C                   MOVEL     RTIMGQ        XXIMGQ
     C                   ENDIF
     C     C             IFEQ      2
     C                   MOVEL     RTOUTQ        XXOUTQ
     C                   ENDIF
     C     C             IFEQ      3
     C                   MOVEL     RTMSGQ        XXMSGQ
     C                   ENDIF
     C     C             IFEQ      4
     C                   MOVEL     RTKEY         XXKEY
     C                   ENDIF
     C     C             IFEQ      5
     C     RTIADR        IFNE      *BLANKS
     C                   MOVEL     RTIADR        XXIADR
     C                   ELSE
     C                   MOVEL     RTMALU        XXIADR
     C                   ENDIF
     C                   ENDIF
     C     C             IFEQ      6
     C                   MOVEL     RTRCVP        XXRCVP
     C                   ENDIF
      *
      *
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     '*NO Desc'    SCDESC
     C                   ENDIF
     C*
     C     RTIFXP        IFNE      *BLANKS
     C                   MOVEL     RTIFXP        SCIFXP
     C                   ELSE
     C                   MOVEL     '*ALL    '    SCIFXP
     C                   ENDIF
     C                   MOVE      RTKSEQ        SCKSEQ
     C*
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2A         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCACTC
     C                   MOVE      *BLANK        SCIFXP
     C                   MOVE      *ZEROS        SCKSEQ
     C                   MOVEL     NOLNK         SCDESC
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2A         #@$2A
     C*
     C*
     C                   ENDSR
**    AC/AD
*TSI-QUEUE  Place received fax in xxxxxxxxxx
*TSI-PRINT  Print received fax on xxxxxxxxxx
*TSI-NOTIFY Notify xxxxxxxxxx when fax is received.
*TSI-INDEX  Index faxes to xxxxxxxxxxxxxxxxxxxxxxxxx
*TSI-EMAIL  Email faxes to xxxxxxxx
*TSI-RCVPGM Run program xxxxxxxxxx for received fax.
