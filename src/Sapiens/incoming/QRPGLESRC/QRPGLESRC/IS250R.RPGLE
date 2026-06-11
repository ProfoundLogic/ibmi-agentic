     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS250R - WORK WITH OUTGOING FAX ENTRIES                  *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF OUTGOING FAX ENTRIES      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H***************************************************************
     H*  REVISIONS:                                                **
     H*                                                            **
     H*  10/17/11 C:277 Project: SY195
     H*                 QDLS migration to IFS
     H*                 Add two parameters with command
     H*
     H*  07/13/00 C:128 PROJECT MC087                              **
     H*                 ON SELECTION OF FAX DETAIL WRONG RECORD    **
     H*                 COULD BE DISPLAYED. WITH THE ADVENT OF THE **
     H*                 RESEND FEATURE THE KEY TO THE LOGICAL FILE **
     H*                 BECAME NON UNIQUE. ADDED LOGICAL FILE WITH **
     H*                 A DEEPER KEY.                              **
     H*****************************************************************
     H*  RETRN CODES
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTION         *
     H*  21    RECORD LOCKED BY OTHER USER                            *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*****************************************************************
     FIS250F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F                                     SFILE(SCR01SB:RRN2B)
     F                                     SFILE(SCR01SC:RRN2C)
     F*
     FIAFTL1    IF   E           K DISK
     FSDCMLE    IF   E           K DISK
      ********BGN CHG 07/13/00 ***************************************
     F******IAFTL   UF  E           K        DISK
     F******            PFT01                             KRENAMEPFT02
     FIAFTL0    UF   E           K DISK
     F                                     RENAME(PFT01:PFT02)
      ********END CHG 07/13/00 ***************************************
     FIMDOL1    IF   E           K DISK

     D*
     D DN              S             75    DIM(5)                               DOCN's
     D NOLNK           C                   CONST('No entries available')
      *
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('By user:')
     D MID             C                   CONST('&QUEUE  ID')
      *
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('REMOVED ')
     D DHBR            C                   CONST('has been removed')
     D DTOQ            C                   CONST('from Queue:')
     D DDL1            C                   CONST('and marked DELETED')
     D DDL2            C                   CONST('from the system. ')
     D FFRM            C                   CONST('Fax From:')
      *
     D CCOMP           C                   CONST('Complete')
     D CERR            C                   CONST('Error   ')
     D CSND            C                   CONST('Sending ')
     D CRSND           C                   CONST('Resend  ')
     D CDSC            C                   CONST('NO DESCRIPTION AVAIL')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      /Free
       scPrDs = 'Work with Outgoing Fax Entries';
      /End-Free
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C     FTKY          KLIST
     C                   KFLD                    Q@USER
     C                   KFLD                    ##DATE            7 0
     C                   KFLD                    SCTIME
     C*
     C*
     C     FTKY1         KLIST
     C                   KFLD                    Q@USER
     C*
     C     FTKY2         KLIST
     C                   KFLD                    SCJOBN
     C                   KFLD                    SCUSR
     C                   KFLD                    SCJOB#
     C                   KFLD                    SCFILN
     C                   KFLD                    SCFIL#
      ********BGN CHG 07/13/00 ***************************************
     C                   KFLD                    SHFCDT
     C                   KFLD                    SHFCTM
      ********END CHG 07/13/00 ***************************************
     C*
     C*
     C     CMKY          KLIST
     C                   KFLD                    SCKEY
     C                   KFLD                    SCSEQ#
     C*
     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8               MDY(YY)
     C                   PARM      *BLANKS       SRTFMT            8               MDYY
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD   '    SRFFMT            8            YYMMDD
     C                   PARM      '*MDYY   '    SRTFMT            8            MMDDYY
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD   '    SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     PL0191        PLIST
     C                   PARM                    SCID             10
     C                   PARM                    @@RSET            1
     C                   PARM                    QQRTC             1
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    $$RTN             1
     C*
     C*** IF $$RTN = 'A' THEN THIS IS FOR ALL USERS, AND SHOW A LIST
     C*** TO BE SELECTED FROM , ELSE JUST DO FOR THE CALLING USER.
     C*
     C     $$RTN         IFEQ      'A'
     C*
     C     @@RSET        IFEQ      *BLANK
     C                   MOVEL     'Y'           @@RSET
     C                   ELSE
     C                   MOVEL     'N'           @@RSET
     C                   ENDIF
     C*
     C                   CALL      'IS191C'      PL0191
     C*
     C                   MOVEL     SCID          Q@USER
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCID          OREQ      *BLANK
     C                   MOVEL     Q1USER        Q@USER           10
     C                   END
     C                   ELSE
     C                   MOVEL     Q1USER        Q@USER
     C                   ENDIF
     C*
     C*** USE TODAYS DATE  0 TIME FOR START OF DISPLAY ***
     C*
     C     RSTRT         TAG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *BLANKS       SCDATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       ##DATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCTIME                         REPOSITION KEY
     C*
     C                   MOVE      DSDATE        SCDATE
     C                   MOVEL     Q@USER        SCUSRN
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C*** CVT THE SCDATE VALUE ***
     C*
     C******
     C*** DO DATE CONVERT TO CHANGE INTO AN 8DIGIT DATE EVEN IF 6 WAS KEYED
     C******
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      *ON
     C                   ENDIF
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTCYMD
     C                   MOVEL     SRDTO         ##DATE
     C*
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCPDT
     C*
     C*
     C*
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C     FTKY          SETLL     PFT01                              60
     C     @VIEW         WHENEQ    'B'
     C     FTKY          SETLL     PFT01                              60
     C     @VIEW         WHENEQ    'C'
     C     FTKY          SETLL     PFT01                              60
     C                   ENDSL
     C*
     C*
     C                   MOVE      *BLANKS       SCDATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       ##DATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCTIME                         REPOSITION KEY
     C*
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      *ON           *IN04
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   WRITE     SCR01CA
     C     @VIEW         WHENEQ    'B'
     C                   WRITE     SCR01CB
     C     @VIEW         WHENEQ    'C'
     C                   WRITE     SCR01CC
     C                   ENDSL
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CA
     C                   ENDIF
     C     @VIEW         WHENEQ    'B'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CB
     C                   ENDIF
     C     @VIEW         WHENEQ    'C'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CC'
     C     Q2RECF        OREQ      'SCR01SC'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CC
     C                   ENDIF
     C                   ENDSL
     C*
     C                   MOVE      *OFF          *IN01
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      *ON
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C     *IN10         IFEQ      *ON
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
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      *ON                                          ROLLUP
     C     *IN32         CABEQ     *ON           STRT                           NO ROLL IF EOF
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   MOVE      #@$2A         RRN2A
     C     @VIEW         WHENEQ    'B'
     C                   MOVE      #@$2B         RRN2B
     C     @VIEW         WHENEQ    'C'
     C                   MOVE      #@$2C         RRN2C
     C                   ENDSL
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C****************************************
     C*   *INKC -  EXIT                      *
     C****************************************
     C     *INKC         IFEQ      *ON                                          EXIT
     C                   MOVEL     'A'           QQRTCD            1
     C                   GOTO      TERM
     C                   ENDIF                                                  END  *INKC       P
     C*
     C****************************************
     C*   *INKE - REFRESH                    *
     C****************************************
     C     *INKE         IFEQ      *ON
     C                   MOVE      SCPDT         SCDATE
     C                   GOTO      TOP
     C                   ENDIF                                                  END *INKE        P
     C*
     C*
     C****************************************
     C*   *INKF -                            *
     C****************************************
     C     *INKF         IFEQ      *ON
     C*
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  END *INKF        P
     C*
     C*
     C****************************************
     C*   *INKK -  CHANGE VIEW               *
     C****************************************
     C     *INKK         IFEQ      *ON                                          ROLLUP
     C                   MOVE      SCPDT         SCDATE
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   MOVEL     'B'           @VIEW
     C     @VIEW         WHENEQ    'B'
     C                   MOVEL     'C'           @VIEW
     C     @VIEW         WHENEQ    'C'
     C                   MOVEL     'A'           @VIEW
     C                   ENDSL
     C*
     C                   GOTO      TOP
     C                   ENDIF                                                  END *INKK        P
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCDATE        IFNE      *ZEROS
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   READC     SCR01SA                                50
     C     @VIEW         WHENEQ    'B'
     C                   READC     SCR01SB                                50
     C     @VIEW         WHENEQ    'C'
     C                   READC     SCR01SC                                50
     C                   ENDSL
     C     *IN50         IFEQ      '0'
     C*
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     '2'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '4'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   UPDATE    SCR01SC
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C                   ELSE                                                   USER BLANK OUT SELCT
     C     SCSEL         IFEQ      ' '
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   UPDATE    SCR01SC
     C                   ENDSL
     C                   GOTO      CKSEL
     C                   END                                                    END EQ ' '
     C                   END                                                    END EQ '1'
     C*
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C     SCSEL         IFEQ      '1'
     C*
     C*** DISPLAY DETAILS
     C*
     C                   MOVE      ' '           QQRTCD
     C                   MOVEL     *BLANKS       SCSEL
     C***
     C                   CALL      'IS250R1'
     C                   PARM      SCJOBN        QQJOBN           10            SPFL JOB NAM
     C                   PARM      SCUSRN        QQUSRN           10
     C                   PARM      SCJOB#        QQJOB#            6            SPLF JOB NUM
     C                   PARM      SCFILN        QQFILN           10            SPLF NAME  4
     C                   PARM      SCFIL#        QQFIL#            3            SPLF NUMBER
     C                   PARM      *BLANK        QQRCOD            1
      ********BGN CHG 07/13/00 ***************************************
     C                   PARM      SHFCDT        QQFCDT            7 0          DATE
     C                   PARM      SHFCTM        QQFCTM            6 0          TIME
      ********END CHG 07/13/00 ***************************************
     C*
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   UPDATE    SCR01SC
     C                   ENDSL
     C*
     C                   MOVE      *OFF          *IN33
     C*
     C                   ENDIF                                                  ELSE EQ '1'
     C*
     C*
     C*
     C     SCSEL         IFEQ      '2'
     C*** RE-SEND   ***
     C                   MOVE      ' '           QQRTCD
     C                   MOVEL     *BLANKS       SCSEL
     C***
     C                   SELECT
     C     SCFL02        WHENEQ    'O'
     C                   MOVEL     'O'           Q1RCD             1
     C     SCFL02        WHENEQ    'S'
     C                   MOVEL     'S'           Q1RCD             1
     C     SCFL02        WHENEQ    'F'
     C                   MOVEL     'F'           Q1RCD             1
     C                   OTHER
     C                   MOVEL     ' '           Q1RCD             1
     C                   ENDSL
     C                   MOVEL     '*R'          Q1CVRI            2
     C                   MOVE      SCSEQ#        X4SEQ             5
     C*
     C     SCFL01        IFEQ      'D'
     C     CMKY          CHAIN     PCM01                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     CMFOLD        X4FOLD           80
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     CMUNCN        X4UNCN          255
     C                   MOVE      CMIFSP        X4IFSP            1
     C*** End   Add *** 10/17/11 **************************************
     C                   ENDIF
     C                   ELSE
     C     SCFLR         CAT(P)    '/':0         X4FOLD
     C                   CAT(P)    SCSFLR:0      X4FOLD
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     *BLANKS       X4UNCN          255
     C                   MOVE      *BLANKS       X4IFSP            1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C                   ENDIF
     C*** IF TYPE IS "S" THEN VERIFY THE SPOOLED FILE EXISTS OTHERWISE
     C*** DISPLAY A MESSAGE THAT THIS FILE IS NO LONGER AVAIL. TO BE
     C*** RE-FAXED.
     C     Q1RCD         IFEQ      'S'
     C***
      ***???***
     C** CALL PGM TO VERIFY THE SPOOLED FILE EXISTS
     C** IF NOT ERROR
     C                   CALL      'SVALSPL'
     C                   PARM                    SCJOBN
     C                   PARM                    SCJOB#
     C                   PARM                    SCUSR
     C                   PARM                    SCFILN
     C                   PARM                    SCFIL#
     C                   PARM                    BBRTC             1
     C                   MOVE      BBRTC         *IN22
     C*
     C     *IN22         CABEQ     '1'           UPD2
     C*
     C                   ENDIF
     C*
     C                   CALL      'IS203R'
     C                   PARM      SCRECP        Q1RECP           10            SPFL JOB NAM
     C                   PARM      SCKEY         Q1KEY            40
     C                   PARM      X4SEQ         Q1SEQ             5            SPLF JOB NUM
     C                   PARM      SCDCID        Q1DCID           12            SPLF NAME  4
     C                   PARM      X4FOLD        Q1FOLD           80            SPLF NUMBER
     C                   PARM                    Q1RCD
     C                   PARM      SCQID         Q1QID            10
     C                   PARM                    Q1CVRI            2
     C                   PARM                    SCJOB#
     C                   PARM                    SCUSR
     C                   PARM                    SCJOBN
     C                   PARM                    SCFILN
     C                   PARM                    SCFIL#
      ********BGN CHG 07/13/00 ***************************************
     C                   PARM                    SHFCDT
     C                   PARM                    SHFCTM
      ********END CHG 07/13/00 ***************************************
     C*
     C     UPD2          TAG
     C*
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   UPDATE    SCR01SC
     C                   ENDSL
     C*
     C                   MOVE      *OFF          *IN33
     C*
     C*
     C     *IN22         CABEQ     '1'           STRT
     C*
     C                   ENDIF                                                  END  EQ '2'
     C*
     C*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     SCSEL         IFEQ      '4'
     C*** REMOVE ******
     C                   MOVE      ' '           QQRTCD
     C                   MOVE      ' '           ER21
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C     SCFAXC        IFEQ      CCOMP
     C     SCFAXC        OREQ      CERR
     C*
     C     FTKY2         CHAIN     PFT02                              68
     C     *IN68         IFEQ      *OFF
     C                   MOVEL     'I'           FTSTAT
     C                   UPDATE    PFT02
     C                   ENDIF
     C                   ELSE
     C*** CAN'T REMOVE UNTIL IN COMPLETE OR ERROR STATUS
      ***???***
     C                   MOVE      *ON           *IN21
     C                   MOVE      '1'           ER21              1
     C*
     C                   ENDIF
     C*
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   UPDATE    SCR01SC
     C                   ENDSL
     C*
     C                   MOVE      *OFF          *IN33
     C*
     C     ER21          CABEQ     '1'           STRT
     C*
     C                   ENDIF                                                  END  EQ '4'
     C*
     C*
     C     SCSEL         IFEQ      '5'
     C*** GET THE ***
     C                   SELECT
     C     SCFL02        WHENEQ    'O'                                          OFFICE DOC
     C*
     C                   MOVEL     *BLANKS       X4FL05
     C     SCFL01        IFEQ      'D'
     C     CMKY          CHAIN     PCM01                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     CMFOLD        X4FOLD           80
     C                   MOVEL     CMFL05        X4FL05            1
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     CMUNCN        X4UNCN          255
     C                   MOVE      CMIFSP        X4IFSP            1
     C*** End   Add *** 10/17/11 **************************************
     C                   ENDIF
     C                   ELSE
     C     SCFLR         CAT(P)    '/':0         X4FOLD
     C                   CAT(P)    SCSFLR:0      X4FOLD
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     *BLANKS       X4UNCN          255
     C                   MOVE      *BLANKS       X4IFSP            1
     C*** End   Add *** 10/17/11 **************************************
     C                   ENDIF
     C*
     C                   MOVEL     SCDCID        X5DCID           12
     C                   MOVEL     X4FOLD        X5FOLD           80
     C                   MOVE      SCSEQ#        X5SQ#             5
     C                   MOVEL     SCKEY         X5KY             40
     C                   MOVEL     *BLANK        QQRTCD            1
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     X4UNCN        X5UNCN          255
     C                   MOVEL     X4IFSP        X5IFSP            1
     C*** End   Add *** 10/17/11 **************************************
     C                   CALL      'SDSPDOC'
     C                   PARM                    X5DCID           12
     C                   PARM                    X5FOLD
     C                   PARM                    X5KY
     C                   PARM                    X5SQ#
     C                   PARM                    X4FL05
     C                   PARM                    Q5RTCD            1
     C                   PARM      *BLANK        Q5MS              1
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM                    X5UNCN
     C                   PARM                    X5IFSP
     C*** End   Add *** 10/17/11 **************************************
     C*
     C     Q5RTCD        IFEQ      '1'
     C                   MOVE      '1'           ERR               1
     C                   END
     C     Q5RTCD        IFEQ      '2'
     C                   MOVE      '1'           ERR               1
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           ERR               1
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           ERR               1
     C                   END
     C*
     C*
     C     SCFL02        WHENEQ    'F'                                          FAX DOC
     C*
     C*
     C     SCFL01        IFEQ      'D'
     C     CMKY          CHAIN     PCM01                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     CMFOLD        X4FOLD           80
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     CMUNCN        X4UNCN          255
     C                   MOVE      CMIFSP        X4IFSP            1
     C*** End   Add *** 10/17/11 **************************************
     C                   ENDIF
     C                   ELSE
     C     SCFLR         CAT(P)    '/':0         X4FOLD
     C                   CAT(P)    SCSFLR:0      X4FOLD
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     *BLANKS       X4UNCN          255
     C                   MOVE      *BLANKS       X4IFSP            1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C                   ENDIF
     C*
     C*** CALL THE DSPFAXC PROGRAM TO DISPLAY THE FAX, ELSE
     C                   CALL      'DSPFAXC'
     C                   PARM                    SCDCID
     C                   PARM      X4FOLD        $$FOLD           80
     C                   PARM      *BLANKS       Q@RTRN            1
     C*
     C     SCFL02        WHENEQ    'S'                                          SPL DOC
     C**** NOTHING TO DO HERE *** CANT VIEW A SPOOLED IMAGE
      *
     C                   OTHER                                                  IMAGE
     C*** IMAGE DISPLAY
     C                   MOVEL     *BLANKS       QQRTN
     C                   MOVEL     'FAX     '    X6DTYP
     C     SCDCID        CHAIN     PDO01                              69
     C     *IN69         IFEQ      *OFF
     C                   MOVEL     DODTYP        X6DTYP            8
     C                   ENDIF
     C                   MOVEL(P)  CDSC          X6DESC           40
     C*
     C                   CALL      'IS211C'
     C                   PARM                    SCDCID           12
     C                   PARM                    SCFLR             8
     C                   PARM                    SCSFLR           12
     C                   PARM      X6DTYP        XXDOCT            8
     C                   PARM      X6DESC        XXDESC           40
     C                   PARM      *BLANKS       QQRTN             6
     C                   ENDSL
      *
      *
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   UPDATE    SCR01SC
     C                   ENDSL
     C*
     C                   ENDIF                                                  ELSE EQ '2'
     C*
     C*
     C     SCSEL         IFEQ      '7'
     C*
     C                   CALL      'IS016R'
     C                   PARM                    SCDCID
     C                   PARM      'IS215R  '    XXPNAM           10
     C*
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   UPDATE    SCR01SC
     C                   ENDSL
     C*
     C                   ENDIF                                                  END  EQ '7'
     C*
     C*
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
     C                   Z-ADD     0             RRN2B             5 0
     C                   Z-ADD     0             RRN2C             5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01  '     Q2RECF
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   MOVEL     'A'           @VIEW             1
     C     @VIEW         WHENEQ    'B'
     C                   MOVEL     'B'           @VIEW             1
     C     @VIEW         WHENEQ    'C'
     C                   MOVEL     'C'           @VIEW             1
     C                   OTHER
     C                   MOVEL     'A'           @VIEW
     C                   ENDSL
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
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C     FTKY1         READE     PFT01                                  60
     C     @VIEW         WHENEQ    'B'
     C     FTKY1         READE     PFT01                                  60
     C     @VIEW         WHENEQ    'C'
     C     FTKY1         READE     PFT01                                  60
     C                   ENDSL
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   ADD       1             RRN2A
     C     @VIEW         WHENEQ    'B'
     C                   ADD       1             RRN2B
     C     @VIEW         WHENEQ    'C'
     C                   ADD       1             RRN2C
     C                   ENDSL
     C*
     C**** CVT DATE TO MMDDYYYY
     C*
     C                   Z-ADD     FTFCDT        SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCFCDT
     C                   MOVE      FTFCTM        SCFCTM
     C*
     C*
     C                   MOVEL     FTFLR         SCFLR
     C                   MOVEL     FTSFLR        SCSFLR
     C                   MOVEL     FTDCID        SCDCID
     C                   MOVEL     FTQID         SCQID
     C                   MOVEL     FTUSRN        SCUSR
     C                   MOVEL     FTFL01        SCFL01
     C                   MOVEL     FTFL02        SCFL02
     C                   MOVEL     FTRCID        SCRECP
     C                   MOVEL     FTJOB#        SCJOB#
     C                   MOVEL     FTJOBN        SCJOBN
     C*
     C                   MOVEL     FTFILN        SCFILN
     C                   MOVEL     FTFIL#        SCFIL#
     C*
     C                   MOVEL     FTKEY         SCKEY
     C                   MOVEL     FTSEQ#        SCSEQ#
     C                   MOVEL     FTCVRI        SCCVRI
      ********BGN CHG 07/13/00 ***************************************
     C                   Z-ADD     FTFCDT        SHFCDT
     C                   Z-ADD     FTFCTM        SHFCTM
      ********END CHG 07/13/00 ***************************************
     C                   SELECT
     C     FTFAXC        WHENEQ    'C'
     C                   MOVEL     CCOMP         SCFAXC
     C     FTFAXC        WHENEQ    'S'
     C     FTFAXC        OREQ      ' '
     C                   MOVEL     CSND          SCFAXC
     C     FTFAXC        WHENEQ    'E'
     C                   MOVEL     CERR          SCFAXC
     C     FTFAXC        WHENEQ    'R'
     C                   MOVEL     CRSND         SCFAXC
     C                   OTHER
     C                   MOVEL     *BLANKS       SCFAXC
     C                   ENDSL
     C                   MOVEL     FTCPT1        SCCPT1
     C*
     C                   MOVEL     FTFPH#        SCFPH#
     C*
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C*
     C*
     C                   MOVE      '0'           *IN06
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   WRITE     SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   WRITE     SCR01SB
     C     @VIEW         WHENEQ    'C'
     C                   WRITE     SCR01SC
     C                   ENDSL
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C     RRN2A         IFEQ      0
     C********** *IN60     ANDEQ'1'
     C                   MOVEL     NOLNK         SCCPT1
     C                   MOVEL     *ZEROS        SCFCDT
     C                   MOVEL     *ZEROS        SCFCTM
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCQID
     C                   MOVEL     *BLANKS       SCUSR
     C                   MOVEL     *BLANKS       SCFL01
     C                   MOVEL     *BLANKS       SCFL02
     C                   MOVEL     *BLANKS       SCRECP
     C                   MOVEL     *BLANKS       SCJOB#
     C                   MOVEL     *BLANKS       SCJOBN
     C                   MOVEL     *BLANKS       SCFILN
     C                   MOVEL     *BLANKS       SCFIL#
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCSEQ#
     C                   MOVEL     *BLANKS       SCCVRI
     C                   MOVEL     *BLANKS       SCFAXC
     C                   MOVEL     *BLANKS       SCFPH#
     C*
      ********BGN CHG 07/13/00 ***************************************
     C                   Z-ADD     *ZERO         SHFCDT
     C                   Z-ADD     *ZERO         SHFCTM
      ********END CHG 07/13/00 ***************************************
     C                   MOVEL     *BLANKS       SCSEL
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   END
     C     @VIEW         WHENEQ    'B'
     C     RRN2B         IFEQ      0
     C*********  *IN60     ANDEQ'1'
     C                   MOVEL     NOLNK         SCCPT1
     C                   MOVEL     *ZEROS        SCFCDT
     C                   MOVEL     *ZEROS        SCFCTM
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCQID
     C                   MOVEL     *BLANKS       SCUSR
     C                   MOVEL     *BLANKS       SCFL01
     C                   MOVEL     *BLANKS       SCFL02
     C                   MOVEL     *BLANKS       SCRECP
     C                   MOVEL     *BLANKS       SCJOB#
     C                   MOVEL     *BLANKS       SCJOBN
     C                   MOVEL     *BLANKS       SCFILN
     C                   MOVEL     *BLANKS       SCFIL#
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCSEQ#
     C                   MOVEL     *BLANKS       SCCVRI
     C                   MOVEL     *BLANKS       SCFAXC
     C                   MOVEL     *BLANKS       SCFPH#
     C*
      ********BGN CHG 07/13/00 ***************************************
     C                   Z-ADD     *ZERO         SHFCDT
     C                   Z-ADD     *ZERO         SHFCTM
      ********END CHG 07/13/00 ***************************************
     C                   MOVEL     *BLANKS       SCSEL
     C                   Z-ADD     1             RRN2B
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SB
     C                   ENDIF
     C     @VIEW         WHENEQ    'C'
     C     RRN2C         IFEQ      0
     C********   *IN60     ANDEQ*ON
     C                   MOVEL     NOLNK         SCCPT1
     C                   MOVEL     *ZEROS        SCFCDT
     C                   MOVEL     *ZEROS        SCFCTM
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCQID
     C                   MOVEL     *BLANKS       SCUSR
     C                   MOVEL     *BLANKS       SCFL01
     C                   MOVEL     *BLANKS       SCFL02
     C                   MOVEL     *BLANKS       SCRECP
     C                   MOVEL     *BLANKS       SCJOB#
     C                   MOVEL     *BLANKS       SCJOBN
     C                   MOVEL     *BLANKS       SCFILN
     C                   MOVEL     *BLANKS       SCFIL#
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCSEQ#
     C                   MOVEL     *BLANKS       SCCVRI
     C                   MOVEL     *BLANKS       SCFAXC
     C                   MOVEL     *BLANKS       SCFPH#
     C*
      ********BGN CHG 07/13/00 ***************************************
     C                   Z-ADD     *ZERO         SHFCDT
     C                   Z-ADD     *ZERO         SHFCTM
      ********END CHG 07/13/00 ***************************************
     C                   MOVEL     *BLANKS       SCSEL
     C                   Z-ADD     1             RRN2C
     C                   MOVE      *ON           *IN06
     C                   WRITE     SCR01SC
     C                   ENDIF
     C                   ENDSL
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   Z-ADD     RRN2A         #@$2A
     C     @VIEW         WHENEQ    'B'
     C                   Z-ADD     RRN2B         #@$2B
     C     @VIEW         WHENEQ    'C'
     C                   Z-ADD     RRN2C         #@$2C
     C                   ENDSL
     C*
     C                   MOVEL     *ZEROS        SCDATE
     C                   MOVEL     *ZEROS        ##DATE
     C                   MOVEL     *ZEROS        SCTIME
     C*
     C                   ENDSR
     C*
