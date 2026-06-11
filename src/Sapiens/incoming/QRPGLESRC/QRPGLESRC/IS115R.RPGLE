     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE IS115R  - REASSIGN SELECTION SCREEN
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF ALL SHARED QUEUE ID'S(USER)*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/10/17 C:062 Support: 43914(114)                            *
     H*                Don't seton LR if handling multi-docs          *
     H*                                                               *
     H* 10/09/14 C:302 Support: 87048                                 *
     H*                Revert to page at a time logic.  Grid has      *
     H*                too many entries for user.                     *
     H*                                                               *
     H* 07/16/14 C:062 Support: IM1144 86105                          *
     H*                Set the document status to RL                  *
     H*                                                               *
     H* 06/09/14 C:302 Support: 85228                                 *
     H*                Queues not displaying in second screen         *
     H*                Program Clean-up.                              *
     H*                                                               *
     H* 03/31/14 C:062 Support IM147 85216                            *
     H*                Changes to reasign process.                    *
     H*                                                               *
     H* 06/28/13 C:267 Project: 2257 (SIS - Haifa Kealy)              *
     H*                Recompile due to screen changes.               *
     H*                                                               *
     H* 12/14/11 C:33  Project: 114IAF                                *
     H*                Process multiple reassignments.                *
     H*                                                               *
     H* 10/14/11 C:282 SUPPORT: WA114 77832                           *
     H*               -CHECK ON QUEUE SECURITY LEVELS.                *
     H*                                                               *
     F* 07/09/07 C:191 PROJECT: 10IJI                                 *
     F*               -WRITE RECIEVED DATE TO IMIEP                   *
      *                                                               *
     H* 10/25/06 C:062 PROJECT: 55427                                 *
     H*                DON'T REMOVE FROM QUEUE IF TRASNSFERRED TO SAME*
     H*                                                               *
     H* 04/08/03 C:062 PROJECT: 114MEX                                *
     H*                Pass back Queue id.                            *
     H*                                                               *
     H* 07/07/00 C:128 PROJECT: DR122                                 *
     H*                Changed to use new 7/0 date field in IMDLP.    *
     H***                                                             *
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
     H*    IMIQL3  (PRT01): ID    - VIEW 1                            *
     H*****************************************************************
     FIS115F    CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F                                     SFILE(SCR01SB:RRN2B)
     F                                     SFILE(SCR02S:RRN2)
     F*
     FIMIQL3    IF   E           K DISK
     FIMIELH    UF A E           K DISK
     FIMDLL     IF   E           K DISK
     FIMDLL4    UF   E           K DISK
     F                                     RENAME(PDL01:PDL02)
     FIMDOL1    UF   E           K DISK
     FSMSLL2    IF   E           K DISK

     D QS              S             10    DIM(99)                              Q'S SELCTD
     D DN              S             75    DIM(4)                               DOCN's

     D NOLNK           C                   CONST('No queues available ')
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D MID1            C                   CONST('&DOCUMENT ID')
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('REASSIGN')
     D DHBR            C                   CONST('has been reassigned')
     D DTOQ            C                   CONST('to Queue:')
     D DRTQ            C                   CONST(' Retain on Q: ')

     D                 DS
     D  ##MSG1                 1     64
     D  SM                     1     64
     D                                     DIM(64)                              SCMSG

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8               MDY(YY)
     C                   PARM      *BLANKS       SRTFMT            8               MDYY
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*CYMD'       SRTFMT            8            CYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1

     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     IEKY          KLIST
     C                   KFLD                    QQINQ
     C                   KFLD                    QQDCID
     C                   KFLD                    $$SDT
     C                   KFLD                    $$STM

     C     IEKY1         KLIST
     C                   KFLD                    ##QID            10
     C                   KFLD                    QQDCID
     C                   KFLD                    $$SDT
     C                   KFLD                    $$STM

     C     DLKY2         KLIST
     C                   KFLD                    SCDLID
     C                   KFLD                    ##DT              7 0
     C                   KFLD                    ##TM              6 0
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     C     SLKY          KLIST
     C                   KFLD                    Q1USER

     C     *ENTRY        PLIST
     C                   PARM                    QQSDT             8            MMDDYYYY
     C                   PARM                    QQSTM             6
     C                   PARM                    QQDOCT            8
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFL01            1
     C                   PARM                    QQFLR             8
     C                   PARM                    QQSFLR           12
     C                   PARM                    QQPTH            63
     C                   PARM                    QQINQ            10
     C                   PARM                    QQDESC           30
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQOUTQ           10
      /Free
       scPrDs = 'Reassign Entry';
      /End-Free

     C                   TIME                    DSDTTM

     C                   Z-ADD     *ZEROS        ##DT
     C                   Z-ADD     *ZEROS        ##TM
     C                   MOVEL     QQDCID        SCDCID
     C                   MOVEL     QQDESC        SCDDSC

     C                   MOVE      QQSTM         $$STM             6 0

     C                   MOVE      *ZEROS        SRDTI
     C                   MOVE      QQSDT         SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         $$SDT             8 0
     C     SRERR         IFEQ      *ON

     C                   ENDIF

     C     *LIKE         DEFINE    QQRTCD        XXRTCD
     C                   MOVE      QQRTCD        XXRTCD

     C                   SELECT
     C     QQRTCD        WHENEQ    'P'
     C                   MOVE      *ON           *IN45
     C     QQRTCD        WHENEQ    '*'
     C                   EXSR      REASGN
     C                   RETURN
     C     QQRTCD        WHENEQ    'L'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   OTHER
     C                   MOVE      *OFF          *IN45
     C                   ENDSL

     C     *LIKE         DEFINE    *IN45         XXMULT
     C                   MOVE      *IN45         XXMULT
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG

     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT

     C     SCQID         SETLL     PIQ01                              60

     C*** CLEAR SUBFILE
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   MOVE      '1'           *IN03
     C                   MOVE      '1'           *IN04
     C                   MOVE      '0'           *IN01
     C                   Z-ADD     0             RRN2A             5 0
     C                   WRITE     SCR01CA
     C                   MOVE      '0'           *IN03
     C                   MOVE      '0'           *IN04
     C                   MOVE      '1'           *IN01
     C                   EXSR      LOADA                                        LOAD RCDS FROM FILE
     C     @VIEW         WHENEQ    'B'
     C                   seton                                        03
     C                   seton                                        04
     C                   Z-ADD     0             RRN2B             5 0
     C                   WRITE     SCR01CB
     C                   setoff                                       03
     C                   setoff                                       04
     C                   EXSR      LOADB                                        LOAD RCDS FROM FILE
     C                   ENDSL

     C     STRT          TAG

     C                   MOVEL     *BLANKS       SCQID
     C                   MOVE      *BLANKS       QS                             CLEAR SLCTD QS

     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
      /Free
       av_keys = 'F3=Exit   F10=Search   F11=Change View   F12=Cancel';
      /End-free

     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CA
     C                   ENDIF

     C     @VIEW         WHENEQ    'B'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CB
     C                   ENDIF
     C                   ENDSL

     C                   MOVE      '0'           *IN01

     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************

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

      ****************************************
      *   *IN18 -  ROLLUP REQUEST            *
      ****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF

     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   MOVE      SV$2A         RRN2A
     C                   EXSR      LOADA
     C     @VIEW         WHENEQ    'B'
     C                   MOVE      SV$2B         RRN2B
     C                   EXSR      LOADB
     C                   ENDSL

     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP

      ****************************************
      *   *INKC -  EXIT                      *
      ****************************************
     C     *INKC         IFEQ      '1'                                          EXIT
     C                   MOVEL     'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P


      ****************************************
      *   *INKJ -  SEARCH                 *
      ****************************************

     C     *INKJ         IFEQ      '1'

     C     FLD           IFEQ      'SCDLID  '
      *** CALL FAX PORT SEARCH PROGRAM
     C                   CALL      'IS146R'
     C                   PARM      SCDLID        QQDLID           10
     C                   PARM      ' '           ##RTC             1

     C     ##RTC         CABEQ     'A'           TERM                     LR
     C     ##RTC         CABEQ     'B'           STRT

     C                   MOVEL     QQDLID        SCDLID
     C                   GOTO      STRT

     C                   ENDIF

     C                   ENDIF

      ****************************************
      *   *INKK -  CHANGE VIEW            *
      ****************************************
     C     *INKK         IFEQ      *ON                                          EXIT
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   MOVEL     'B'           @VIEW             1
     C     @VIEW         WHENEQ    'B'
     C                   MOVEL     'A'           @VIEW
     C                   OTHER
     C                   MOVEL     'A'           @VIEW
     C                   ENDSL

     C                   GOTO      TOP
     C                   END                                                    END  *INKK       P

      ****************************************
      *   *INKL -  Cancel                    *
      ****************************************
     C     *INKL         IFEQ      '1'                                          EXIT
     C                   MOVEL     'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P

     C                   GOTO      STRT
     C                   END                                                    *IN11

     C                   MOVE      *BLANKS       @1ST              1
      **** 1ST CHECK THE DIST/LB ID TO SEE IF SOMETHING IS ENTERED.
      **** IF SO, USE THIS NOT THE SELECTED ITEMS (BELOW).
      *** ALSO, VERIFY THAT IF THE DIST/LB IS ENTERED NO SEL CAN BE MADE TOO
      *** THIS WILL CAUSE AND ERROR MSG.  ALSO MAKE SURE THE DIST/LB ID
      **** EXISTS. , AND ALLOW A SEARCH ON THIS.
      *****
     C     SCDLID        IFNE      *BLANKS
      * VERIFY SCDLID IS VALID (ERR 29)
     C     SCDLID        CHAIN     PDL01                              60
     C     *IN60         CABEQ     *ON           STRT                     29

      * VERIFY THAT NO SEL HAS BEEN MADE (OR JUST IGNORE) (ERR 28)

      * *** WILL NEED TO GET ALL RCDS EQ TO DLID IF A DIST, IF A LB THEN
      * *** WILL NEED TO ONLY GET THE ONE THAT IS NEXT........ (LGL)

     C     SCLBFL        IFEQ      'Y'
     C                   EXSR      LB
     C                   ELSE
     C                   EXSR      DIST
     C                   ENDIF
      * *** LOAD THIS TO THE SCR02 STUFF LIKE THE SEL STUFF BELOW.
      * *** THEN PROCESS THE SAME...

     C     @1ST          IFNE      *BLANK
      **** DISPLAY THE WINDOW THEN PERFORM REASSIGNMENT ***
     C                   EXSR      PRCSS
     C                   GOTO      TERM
     C                   ENDIF

     C                   GOTO      STRT

     C                   ENDIF

     C*********************
     C*** IF A REPOSITION WAS REQUESTED
     C*********************
     C     SCQID         IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF

     C                   MOVE      *BLANKS       @1ST              1

      *  CHECK FOR SEARCH SELECTION

     C     CKSEL         TAG

     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   READC     SCR01SA                                50
     C     @VIEW         WHENEQ    'B'
     C                   READC     SCR01SB                                50
     C                   ENDSL

     C     *IN50         IFEQ      *OFF

      *** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
      *** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN

     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C                   ENDSL

     C                   GOTO      STRT

     C                   ENDIF                                                   INVALID SEL     LCT

     C     SCSEL         IFEQ      ' '

     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C                   ENDSL

     C                   GOTO      CKSEL
     C                   END                                                    END EQ ' '

     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** Edit Entry

      *-------------------------------------------------------------------
      **** select "1" - Select
      *-------------------------------------------------------------------
     C     SCSEL         IFEQ      '1'
      *** IF A VALID SELECTION, THEN WE CHECK THE SEND MSG FLAG,
      *** IF IT IS 'Y' THEN WE EXFMT WDW01, ELSE BYPASS THAT,
      *** THEN EXECUTE CODE THAT WILL UPDATE THE ORIGINAL IE RCD,(IF
      ***  IT'S NOT "RETAIN", AND WRITE ADDITIONAL ONES, OR JUST WRITE
      ***  ADDITIONAL ONES IF IT IS "RETAIN"
      *
      * * * FILL AN ARRAY WITH THE Q ID'S THAT WILL BE PROCESSED.
      *
     C                   Z-ADD     1             X                 5 0
     C     *BLANKS       LOOKUP    QS(X)                                  67
     C     *IN67         IFEQ      *ON
     C                   MOVEL     SCID          QS(X)
     C                   MOVEL     '1'           @1ST

     C                   ELSE
     C                   MOVE      '1'           *IN26                           TOO MANY SELECTD

     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C                   ENDSL

     C                   GOTO      STRT

     C                   ENDIF

     C                   MOVEL     *BLANK        SCSEL

     C                   SELECT
     C     @VIEW         WHENEQ    'A'
     C                   UPDATE    SCR01SA
     C     @VIEW         WHENEQ    'B'
     C                   UPDATE    SCR01SB
     C                   ENDSL

     C                   GOTO      CKSEL

     C                   ENDIF                                                     EQ '1'

      ****
      *** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
      *** WITHOUT A SELECTED RECORD
     C                   ELSE                                                   ELSE 0 *IN50

     C     @1ST          IFNE      *BLANK
      **** DISPLAY THE WINDOW THEN PERFORM REASSIGNMENT ***
     C                   EXSR      PRCSS
     C                   GOTO      TERM
     C                   ENDIF

     C                   GOTO      STRT

     C                   ENDIF                                                  END 0 *IN50
     C                   GOTO      STRT

     C     TERM          TAG
     C*** Begin Add *** 01/10/17 ***********************************
     C                   If        qqRtCd = 'P' Or qqRtCd = '*'
     C                   Return
     C                   EndIf
     C*** End   Add *** 01/10/17 ***********************************

     C                   MOVE      *ON           *INLR
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2A             5 0
     C                   Z-ADD     0             SV$2A
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             RRN2B             5 0
     C                   Z-ADD     0             SV$2B
     C                   MOVEL     *BLANK        SCSEL
     C                   MOVEL     *BLANK        @1ST              1
     C*
     C     @VIEW         IFEQ      *BLANK
     C                   MOVEL     'A'           @VIEW
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01  '     Q2RECF
     C                   MOVEL     'Y'           SCSNDM
     C                   MOVEL     'N'           SCRETN
     C                   MOVEL     'N'           SCBRKM
     C                   MOVEL     'Y'           SCLBFL

      *** LOAD MSG1 - 8 TEXT

     C                   MOVEL     *BLANKS       SCMSG1

     C                   SELECT
     C     XXMULT        WHENEQ    *ON
     C     MDOC          CAT(P)    MID1:1        SCMSG1
     C                   OTHER
     C     MDOC          CAT(P)    QQDCID:1      SCMSG1
     C                   ENDSL
     C                   CAT(P)    MHAS:1        SCMSG1
     C                   CAT(P)    MQUE:1        SCMSG1
     C                   CAT(P)    MID:1         SCMSG1
     C                   CAT(P)    '.  '         SCMSG1

     C                   MOVEL     *BLANKS       SCMSG2

     C                   CAT(P)    MSEN:1        SCMSG2
     C                   CAT(P)    Q1USER:1      SCMSG2

     C                   ENDSR
      *====================================================================*
      * * *   LOADBSUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOADB         BEGSR
      *
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANK        SCSEL
      *
      *  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   MOVE      *ZERO         X                 5 0
     C                   MOVE      1             X
     C     X             DOWLE     36
     C                   READ      PIQ01                                  60

     C     *IN60         IFEQ      *ON
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END

     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN05
     C                   LEAVE
     C                   END

     C     SLKY          CHAIN     PSL01                              59
     C                   MOVE      IQSECV        XXSECV            3 0
     C     *IN60         IFEQ      '0'
     C     *IN59         ANDEQ     '0'
     C     SLSECL        ANDGE     XXSECV
     C     IQSTAT        ANDEQ     'A'
     C                   ADD       1             X

     C                   ADD       1             RRN2B
     C                   MOVEL     IQID          SCID

     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SB
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   ENDIF
     C                   ENDDO
      *
      * * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2B         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCID
     C                   MOVEL     NOLNK         SCDESC
     C                   Z-ADD     1             RRN2B
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SB
     C                   END
      *
      *** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2B         #@$2B

     C                   ENDSR
      *====================================================================*
      * * *   LOADA SBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOADA         BEGSR

     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANK        SCSEL

      *  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   MOVE      *ZERO         X                 5 0
     C                   MOVE      1             X
     C     X             DOWLE     9
     C                   READ      PIQ01                                  60

     C     *IN60         IFEQ      *ON
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN05
     C                   LEAVE
     C                   END

     C     SLKY          CHAIN     PSL01                              59
     C                   MOVE      IQSECV        XXSECV            3 0

     C     *IN60         IFEQ      '0'
     C     *IN59         ANDEQ     '0'
     C     SLSECL        ANDGE     XXSECV
     C     IQSTAT        ANDEQ     'A'
     C                   ADD       1             X

     C                   ADD       1             RRN2A
     C                   MOVEL     IQID          SCID
     C                   MOVEL     IQDESC        SCDESC


     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   ENDIF
     C                   ENDDO

     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2A         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCID
     C                   MOVEL     NOLNK         SCDESC
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   END

      *** TO POSITION SCREEN AND CURSOR
     C     1             ADD       SV$2A         #@$2A
     C                   Z-ADD     RRN2A         SV$2A             3 0

     C                   ENDSR

      *====================================================================*
      * * *   PRCSS SUBROUTINE
      *====================================================================*
     C     PRCSS         BEGSR

      * * * DISPLAY THE PROCESS SCREEN SCR02S/C

      * * * PROCESS THE INFO, IF F3 GOTO TERM, IF F12 GOTO STRT, AFTER CLEAR
      * * * IF NO F3/12, THEN GET THE ORIGINAL RECORD FROM IE,
      * * * WRITE ONE FOR EACH QUEUE IN THE ARRAY, THEN IF RETAIN FLAG,
      * * * DONT DELETE THE EXISTING ONE, ELSE MARK IT AS DONE 'D'
      *

      * * * WHEN FINISHED, GOTO TERM TO END THE PROCESS , RETURN CODE
      * * * THAT SAYS IF RCD WAS REMOVED, OR "RETAINED".

     C                   seton                                        03
     C                   seton                                        04
     C                   WRITE     SCR02C
     C                   setoff                                       03
     C                   setoff                                       04
     C                   Z-ADD     1             K                 3 0
     C                   EXSR      LOAD2                                        LOAD RCDS FROM FILE

     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY

     C     SC02T         TAG

     C                   MOVE      *ON           *IN01
      /Free
       av_keys = 'F3=Exit   F12=Cancel';
      /End-free
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVE      *OFF          *IN01

      *******************************************************************
      *   PROCESS ALL COMMAND KEYS
      *******************************************************************

     C     *IN11         IFEQ      '1'

      ****************************************
      *   *INKC -  EXIT                      *
      ****************************************
     C     *INKC         IFEQ      '1'                                          EXIT
     C                   MOVEL     'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P

      ****************************************
      *   *INKL -  Cancel                    *
      ****************************************
     C     *INKL         IFEQ      '1'                                          EXIT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      TOP
     C                   END                                                    END  *INKC       P
     C                   GOTO      SC02T
     C                   END                                                    *IN11

      * * * ENTER WAS PRESSED , PROCESS THE REQUEST *****

      * * * READ THROUGH THE QS, AND FOR EACH:
      *        WRITE A NEW IE RCD.
      *        SEND A MESSAGE, IF REQUESTED
      *
      * * * WHEN FINISHED WITH ALL QS, DETERMINE WHAT TO DO WITH THE ORIGINL
      *      IE RCD.  (MARK IT "D" , OR NOT).
      *
      * * * WE SHOULD ALSO WRITE RCDS TO DOC NOTES FOR EACH NEW ONE AND THE
      * * * OLD ONE!!!!

     C**********************************************************************

     C*** EDIT THE SCREEN HERE ***
     C** SEND MSG FLG, Y OR N

     C     SCSNDM        IFNE      'Y'
     C     SCSNDM        CABNE     'N'           SC02T                2121
     C                   ENDIF

     C** BREAK MSG  FLG,   Y OR N

     C     SCSNDM        IFEQ      'Y'
     C     SCBRKM        IFNE      'Y'
     C     SCBRKM        CABNE     'N'           SC02T                2222
     C                   ENDIF
     C                   ENDIF

     C** RETAIN FLG,   Y OR N

     C     SCRETN        IFNE      'Y'
     C     SCRETN        CABNE     'N'           SC02T                2323
     C                   ENDIF

     C** MESSAGE  NOT BLANKS IF SNDMSG IS Y

     C     SCSNDM        IFEQ      'Y'

     C     SCMSG1        IFEQ      *BLANKS
     C     SCMSG2        ANDEQ     *BLANKS
     C     SCMSG3        ANDEQ     *BLANKS
     C     SCMSG4        ANDEQ     *BLANKS
     C     SCMSG5        ANDEQ     *BLANKS
     C     SCMSG6        ANDEQ     *BLANKS
     C     SCMSG7        ANDEQ     *BLANKS
     C     SCMSG8        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN24
     C                   GOTO      SC02T
     C                   ENDIF

     C                   ENDIF
     C*
     C                   EXSR      REASGN

     C                   ENDSR

     C*====================================================================
     C* REASGN - Reassign the record
     C*====================================================================
     C     REASGN        BEGSR

     C*** PROCESS THE REQUESTS ***
     C                   MOVEL     SCMSG1        ##MSG1           64

     C* Scan for &DOCUMENT and replace it with the document being
     C*  processed.  This will done for multiple documnet processing.
     C                   SELECT
     C     XXMULT        WHENEQ    *ON
     C     MID1          SCAN      ##MSG1        P1                5 0    56
     C     *IN56         IFEQ      *ON
     C                   MOVEA     QQDCID        SM(P1)
     C                   ENDIF
     C                   ENDSL

     C*** Begin Chg *** 07/16/14 ****************************************
     C     DOKY          CHAIN(N)  PDO01                              68
     C*** End   Chg *** 07/16/14 ****************************************
     C     *IN68         IFEQ      *OFF
     C                   Z-ADD     DORCVD        XXRCVD            8 0
     C                   ELSE
     C                   Z-ADD     $$SDT         XXRCVD
     C                   ENDIF

     C     IEKY          CHAIN     PIE01                              68
     C  N68              UNLOCK    IMIELH
     C   68              MOVE      '#'           QQRTCD
     C     *IN68         IFEQ      *OFF
     C                   Z-ADD     1             K
     C     QS(K)         DOWNE     *BLANK

     C                   MOVEL     QS(K)         ##QID
     C*** BYPASS WRITING IF THIS RECORD ALREADY EXISTS IN THE IE FILE
     C     IEKY1         SETLL     PIE01                                  67
     C     *IN67         CABEQ     *ON           BYPWRI


     C                   MOVEL     QS(K)         IEQID            10
     C     Q1PRMS        IFGT      11
     C                   MOVEL     QS(K)         QQOUTQ
     C                   ENDIF

     C                   MOVE      *BLANKS       IESTAT
     C*** Begin Add *** 07/16/14 ******************************************************
     C                   eval      IEDSTS = 'RL'
     C*** End   Add *** 07/16/14 ******************************************************
     C                   TIME                    DSDTTM
     C     10000.01      MULT      DSDATE        IEIDT
     C                   MOVE      IEIDT         IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVEL     Q1USER        IEIUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVE      DSTIME        IEITIM
     C                   MOVEL     Q1PGM         IELPGM
     C                   MOVEL     Q1PGM         IEIPGM
     C                   Z-ADD     XXRCVD        IERCVD

     C                   WRITE     PIE01
     C*** Begin Add *** 07/16/14 ****************************************
     C     DOKY          CHAIN     PDO01
     C                   If        %Found(Imdol1)
     C                   MOVE      IEIDT         DOLDT
     C                   MOVEL     Q1USER        DOLUSR
     C                   MOVE      DSTIME        DOLTIM
     C                   MOVEL     Q1PGM         DOLPGM
     C                   Eval      DOdSts='RL'
     C                   Update    pdo01
     C                   EndIf
     C*** End   Add *** 07/16/14 ****************************************
     C     BYPWRI        TAG

     C*** CALL PGM TO SEND MESSAGE
     C***       IN THIS PROGRAM , SCAN FOR &QUEUEID AND IF FOUND REPLACE IT
     C*         WITH THE QUEUE ID THAT IT IS BEING PROCESSED FOR.

     C     MID           SCAN      ##MSG1        P1                5 0    56
     C     *IN56         IFEQ      *ON
     C                   MOVEA     ##QID         SM(P1)
     C                   ENDIF

     C                   CALL      'ISSNDMR'
     C                   PARM                    ##QID
     C                   PARM                    ##MSG1
     C                   PARM                    SCMSG2
     C                   PARM                    SCMSG3
     C                   PARM                    SCMSG4
     C                   PARM                    SCMSG5
     C                   PARM                    SCMSG6
     C                   PARM                    SCMSG7
     C                   PARM                    SCMSG8
     C                   PARM                    SCBRKM
     C                   PARM                    @$RTN             1

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##CDAT            8 0
     C                   MOVE      ##CDAT        QQCDAT
     C                   MOVE      DSTIME        Q$TIME            6
      *
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     QQCDAT        XXM               2
     C                   MOVE      QQCDAT        XXY               4
     C     2             SUBST     QQCDAT:3      XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10

     C                   MOVEL     Q$TIME        XXH               2
     C                   MOVE      Q$TIME        XXS               2
     C     2             SUBST     Q$TIME:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75

     C                   MOVEL     XX75          DN(1)

     C     MDOC          CAT(P)    QQDCID:1      XX75
     C                   CAT(P)    DHBR:1        XX75
     C                   CAT(P)    DTOQ:1        XX75
     C                   CAT(P)    ##QID:1       XX75
     C                   CAT(P)    '.'           XX75

     C                   MOVEL     XX75          DN(2)

     C     MSEN          CAT(P)    Q1USER:1      XX75
     C                   CAT(P)    DRTQ:3        XX75
     C                   CAT(P)    SCRETN:1      XX75
     C                   CAT(P)    '.'           XX75
     C                   MOVEL     XX75          DN(3)
     C                   MOVEL     *BLANKS       DN(4)
      *

     C*** CALL PGM TO WRITE DOC NOTES

      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS

     C                   DO        4             G                 3 0

     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(G)         QQCM75
     C*
     C     G             IFLT      4
     C                   MOVE      ' '           Q$RTC
     C                   ENDIF
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE

     C                   ENDDO
     C*
     C     'Note: '      CAT       ##MSG1:0      XXMSG$           75
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM      XXMSG$        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C*
     C     SCMSG2        IFNE      *blanks
     C                   CALL      'ISDOCNR'
     C                   PARM      SCMSG2        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   endif
     C*
     C*
     C     SCMSG3        IFNE      *blanks
     C                   CALL      'ISDOCNR'
     C                   PARM      SCMSG3        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   endif
     C*
     C*
     C*
     C     SCMSG4        IFNE      *blanks
     C                   CALL      'ISDOCNR'
     C                   PARM      SCMSG4        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   endif
     C*
     C*
     C*
     C*
     C     SCMSG5        IFNE      *blanks
     C                   CALL      'ISDOCNR'
     C                   PARM      SCMSG5        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   endif
     C*
     C*
     C     SCMSG6        IFNE      *blanks
     C                   CALL      'ISDOCNR'
     C                   PARM      SCMSG6        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   endif
     C*
     C*
     C     SCMSG7        IFNE      *blanks
     C                   CALL      'ISDOCNR'
     C                   PARM      SCMSG7        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   endif
     C*
     C*
     C     SCMSG8        IFNE      *blanks
     C                   CALL      'ISDOCNR'
     C                   PARM      SCMSG8        QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   endif
     ****
     C                   CALL      'ISDOCNR'
     C                   PARM      *blanks       QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM      '1'           Q$RTC             1              RTN CODE
     C*

     C* * * CALL EXIT PROGRAM FOR REASSIGNS

     C                   CALL      'ISE001C'                            99      NO ERR
     C                   PARM                    QQINQ                          FROM QUEUE
     C                   PARM                    ##QID                          TO   QUEUE
     C                   PARM                    Q1USER                         FROM USER
     C                   PARM                    QQDCID                         DOC ID
     C                   PARM                    XRTCD             6            XIT RTN CD

     C                   ADD       1             K

     C                   ENDDO

     C* FIND OUT IF THE FROM QID IS LISTED WITH THE TO QID'S

     C     SCRETN        IFEQ      'N'
     C     1             DO        99            Q                 3 0
     C     QQINQ         IFEQ      QS(Q)
     C                   MOVEL(P)  'Y'           SCRETN
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF

     C*** IF NOT TO RETAIN RCD, THEN WE MARK IT AS "D"ONE....
     C     SCRETN        IFEQ      'N'
     C     IEKY          CHAIN     PIE01                              68
     C     *IN68         IFEQ      *OFF
     C                   MOVE      'D'           IESTAT
     C*** Begin Add *** 07/16/14 ******************************************************
     C                   eval      IEDSTS = 'DL'
     C*** End   Add *** 07/16/14 ******************************************************

     C                   TIME                    DSDTTM
     C     10000.01      MULT      DSDATE        IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVEL     Q1PGM         IELPGM

     C                   UPDATE    PIE01
     C                   ENDIF
     C                   ENDIF

     C                   ENDIF

     C                   GOTO      TERM
     C                   ENDSR
      *====================================================================*
      * * *   LOADA SBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD2         BEGSR

     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   Z-ADD     1             K

     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C     QS(K)         DOWNE     *BLANKS
     C     K             ANDLE     99

     C                   ADD       1             RRN2
     C                   MOVEL     QS(K)         SCID

     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR02S

     C                   ADD       1             K

     C                   ENDDO

     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C                   MOVE      *BLANK        SCID
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR02S
     C                   END

     C*** TO POSITION SCREEN AND CURSOR
     C     1             ADD       SV$2B         #@$2B
     C                   Z-ADD     RRN2B         SV$2B             3 0

     C                   ENDSR
      *====================================================================*
      * * *   LB SUBROUTINE                                             ***
      *====================================================================*
     C     LB            BEGSR

     C*** GET THE NEXT LOAD BALANCE QUEUE TO PROCESS HERE

     C     DLKY2         SETLL     PDL02
     C     SCDLID        READE     PDL02                                  61
     C     *IN61         IFEQ      *OFF


      *
      * * * FILL AN ARRAY WITH THE Q ID'S THAT WILL BE PROCESSED.
      *
     C                   Z-ADD     1             X                 5 0
     C     *BLANKS       LOOKUP    QS(X)                                  67
     C     *IN67         IFEQ      *ON
     C                   MOVEL     DLIMGQ        QS(X)
     C                   MOVEL     '1'           @1ST

     C                   END

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0          CYYMMDD
     C                   MOVE      DSTIME        XXTM              6 0
     C                   Z-ADD     XXDT          DLLDAT                           LAST MAINT DT.
     C                   Z-ADD     XXTM          DLLTIM                           LAST MAINT TM.

     C                   UPDATE    PDL02

     C                   END

     C                   ENDSR
      *====================================================================*
      * * *   DIST SUBROUTINE                                           ***
      *====================================================================*
     C     DIST          BEGSR

     C*** GOT 1ST RCD, NOW LOOP FOR ALL DL RCDS AND ADD TO THE ARRAY.
     C     *IN60         DOWEQ     *OFF
      *
      * * * FILL AN ARRAY WITH THE Q ID'S THAT WILL BE PROCESSED.
      *
     C                   Z-ADD     1             X                 5 0
     C     *BLANKS       LOOKUP    QS(X)                                  67
     C     *IN67         IFEQ      *ON
     C                   MOVEL     DLIMGQ        QS(X)
     C                   MOVEL     '1'           @1ST

     C                   END
     C     SCDLID        READE     PDL01                                  60
     C                   ENDDO

     C                   ENDSR
