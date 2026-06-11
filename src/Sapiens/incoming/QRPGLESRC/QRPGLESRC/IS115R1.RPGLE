     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS115R1 - QUEUE TRANSFER ITEMS
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     F* 07/09/07 C:191 PROJECT: 10IJI                                *
     F*               -WRITE RECIEVED DATE TO IMIEP                  *
      *
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
     H*****************************************************************
     F*
     FIMIELH    UF A E           K DISK
     F*** BEG ADD ********** 07/09/07 ********************************
     F*** LOGICAL OF IMDOP
     FIMDOL1    IF   E           K DISK

     D*** END ADD ********** 07/09/07 ********************************
     D*
     D DN              S             75    DIM(4)                               DOCN's
     D*
     D NOLNK           C                   CONST('No queues available ')
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('TRANSFER')
     D DHBR            C                   CONST('has been Transfered')
     D DTOQ            C                   CONST('to Queue:')
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8               MDY(YY)
     C                   PARM      *BLANKS       SRTFMT            8               MDYY
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*
     C     IEKY          KLIST
     C                   KFLD                    QQINQ
     C                   KFLD                    QQDCID
     C                   KFLD                    $$SDT
     C                   KFLD                    $$STM
     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    QQTOQ            10
     C                   KFLD                    QQDCID
     C                   KFLD                    $$SDT
     C                   KFLD                    $$STM
     C*
     *** BEG ADD ********** 07/09/07 ********************************
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     *** END ADD ********** 07/09/07 ********************************
      **
      *
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
     C                   PARM                    QQTOQ            10
     C                   PARM                    QQDESC           30
     C                   PARM                    QQRTCD            1
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQSTM         $$STM             6 0
     C*
     C                   MOVE      *ZEROS        SRDTI
     C                   MOVE      QQSDT         SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         $$SDT             8 0
     C     SRERR         IFEQ      *ON
     C*
     C                   ENDIF
     C*
      **
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C                   EXSR      DOIT
     C*
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C*
     C*
     C*
     C                   ENDSR
     C**********************************************************************
     C     DOIT          BEGSR
     C**********************************************************************
     C*
     C*** PROCESS THE REQUESTS ***
     *** BEG ADD ********** 07/09/07 ********************************
     C     DOKY          CHAIN     PDO01                              68
     C     *IN68         IFEQ      *OFF
     C                   Z-ADD     DORCVD        XXRCVD            8 0
     C                   ELSE
     C                   Z-ADD     $$SDT         XXRCVD
     C                   ENDIF
     *** END ADD ********** 07/09/07 ********************************
     C*
     C     IEKY          CHAIN     PIE01                              68
     C  N68              UNLOCK    IMIELH
     C     *IN68         IFEQ      *OFF
     C*
     C*** BYPASS WRITING IF THIS RECORD ALREADY EXISTS IN THE IE FILE
     C     IEKY1         SETLL     PIE01                                  67
     C     *IN67         CABEQ     *ON           BYPWRI
     C*
     C*
     C                   MOVEL     QQTOQ         IEQID            10
     C*
     C                   MOVE      *BLANKS       IESTAT
     C                   TIME                    DSDTTM
     C     10000.01      MULT      DSDATE        IEIDT
     C                   MOVE      IEIDT         IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVEL     Q1USER        IEIUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVE      DSTIME        IEITIM
     C                   MOVEL     Q1PGM         IELPGM
     C                   MOVEL     Q1PGM         IEIPGM
     *** BEG ADD ********** 07/09/07 ********************************
     C                   Z-ADD     XXRCVD        IERCVD
     *** END ADD ********** 07/09/07 ********************************
     C*
     C                   WRITE     PIE01
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
     C*
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
     C*
     C                   MOVEL     XX75          DN(1)
     C*
     C     MDOC          CAT(P)    QQDCID:1      XX75
     C                   CAT(P)    DHBR:1        XX75
     C                   CAT(P)    DTOQ:1        XX75
     C                   CAT(P)    QQTOQ:1       XX75
     C                   CAT(P)    '.'           XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C     MSEN          CAT(P)    Q1USER:1      XX75
     C                   CAT(P)    '.'           XX75
     C                   MOVEL     XX75          DN(3)
     C                   MOVEL     *BLANKS       DN(4)
      *
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
     C*
     C*
     C                   DO        4             G                 3 0
     C*
     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(G)         QQCM75
     C*
     C     G             IFGE      4
     C                   MOVE      '1'           Q$RTC             1
     C                   ELSE
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
     C*
     C                   ENDDO
     C     BYPWRI        TAG
     C*
     C*** IF NOT TO RETAIN RCD, THEN WE MARK IT AS "D"ONE....
     C     IEKY          CHAIN     PIE01                              68
     C     *IN68         IFEQ      *OFF
     C                   MOVE      'D'           IESTAT
     C*
     C                   TIME                    DSDTTM
     C     10000.01      MULT      DSDATE        IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVEL     Q1PGM         IELPGM
     C*
     C                   UPDATE    PIE01
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C*
     C                   GOTO      TERM
     C                   ENDSR
