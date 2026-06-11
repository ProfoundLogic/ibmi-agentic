     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS307R - QUEUE TRANSFER                                  *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program ALLOWS TRANSFER OF QUEUED ITEMS               *
     H*                                                               *
     H* CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  RETRN CODES
     H*             -
     H*                'C'       F12 WAS PRESSED IN CALLED PGM
     H*                '3'       F3  WAS PRESSED IN CALLED PGM
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
     H*    IMRTL   (PIE01): TSID  - VIEW 1                            *
     H*    IMRTL1  (PIE01): DESC  - VIEW 2                            *
     H*****************************************************************
     FIS307F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2B)
     F*
     FIMIELF    IF   E           K DISK
     FIMIFL2    IF   E           K DISK
     FIMDTL     IF   E           K DISK
     FIMDOL1    IF   E           K DISK

     D*
     D DN              S             75    DIM(5)                               DOCN's
     D NOLNK           C                   CONST('No entries available')
     D CIDX            C                   CONST('**Indexed**')
     D CRJ             C                   CONST('**Rejected**')
     D CRA             C                   CONST('**Reassigned**')
     D CRM             C                   CONST('**Remove**')
     D*&¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('By user:')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('REMOVED ')
     D DHBR            C                   CONST('has been removed')
     D DTOQ            C                   CONST('from Queue:')
     D DDL1            C                   CONST('and marked DELETED')
     D DDL2            C                   CONST('from the system. ')
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

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    SCUSRQ
     C                   KFLD                    SCDOCI
     C*
     C     IFKY          KLIST
     C                   KFLD                    SCDCID
     C*
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
     C*
     C*
     C     DOKY          KLIST
     C                   KFLD                    SCDCID
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
     C                   PARM      '*YYMD'       SRFFMT            8            YYMMDD
     C                   PARM      *BLANKS       SRTFMT            8            MMDDYY
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
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQQID            10
     C                   PARM                    QQRTCD            1
     C*
      /Free
       scPrDs = 'Queued Items to Transfer';
      /End-Free
     C     REDO          TAG
     C                   MOVE      QQQID         ##QID            10
     C**** CALL THE SELECT PROGRAM IF REQUIRED (SEE QQRTCD)
     C*
      *
     C** CALL IS120R3 TO GET THE USER QUEUE ID
     C                   CALL      'IS120R3'
     C                   PARM      *BLANKS       ##QID
     C                   PARM      *BLANKS       ##DSC            40
     C                   PARM      'F'           ##RTCD            1
     C     ##RTCD        CABEQ     'A'           TERM
     C     ##RTCD        CABEQ     'B'           TERM
     C*
     C     ##QID         IFEQ      *BLANKS
     C                   MOVEL     Q1USER        ##QID
     C                   ENDIF
     C                   MOVEL     ##QID         SCUSRQ
     C*
     C** CALL IS120R2 ****** GET USER QUE ID
     C                   CALL      'IS120R3'
     C                   PARM      *BLANKS       ##QID
     C                   PARM      *BLANKS       ##DSC            40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        CABEQ     'A'           TERM
     C     ##RTCD        CABEQ     'B'           REDO
     C*
     C     ##QID         IFEQ      *BLANKS
     C                   MOVEL     Q1USER        ##QID
     C                   ENDIF
     C*
     C*
     C                   MOVEL     ##QID         SCTOQ
     C                   MOVE      *BLANKS       QQRTCD
      *
     C*
     C     RSTRT         TAG
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C                   MOVE      *BLANKS       SCDATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       ##DATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCTIME                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCDOCI                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCDTYP                         REPOSITION KEY
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
     C***********          MOVE *ZEROS    SCDATE
     C                   ENDIF
     C*
     C                   Z-ADD     SCDATE        ##DATE            8 0
     C********** #SSDYY    CAT  #SSDMD    XXMDY   8
     C******************** MOVE XXMDY     ##DATE  80
     C*
     C*
     C     IEKY1         SETLL     PIE01                              60
     C*
     C*
     C                   MOVE      *BLANKS       SCDATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       ##DATE                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCTIME                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCDOCI                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCDTYP                         REPOSITION KEY
     C*
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      *ON           *IN04
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
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CA
     C                   ENDIF
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
     C                   MOVE      #@$2B         RRN2B
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C****************************************
     C*   *INKC -  EXIT                      *
     C****************************************
     C     *INKC         IFEQ      *ON                                          EXIT
     C                   MOVEL     'A'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF                                                  END  *INKC       P
     C*
     C****************************************
     C*   *INKE - REFRESH                    *
     C****************************************
     C     *INKE         IFEQ      *ON
     C                   GOTO      RSTRT
     C                   ENDIF                                                  END *INKE        P
     C*
     C*
     C****************************************
     C*   *INKF -                            *
     C****************************************
     C     *INKF         IFEQ      *ON
     C*
     C*
     C                   ENDIF                                                  END *INKF        P
     C*
     C*
     C****************************************
     C*   *INKK -  CHANGE VIEW               *
     C****************************************
     C     *INKK         IFEQ      *ON                                          ROLLUP
     C                   ENDIF                                                  END *INKK        P
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCDOCI        IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C*
     C*
     C                   Z-ADD     1             K                 5 0
     C     K             CHAIN     SCR01SA                            63
     C     *IN63         DOWEQ     *OFF
     C     SCDCID        ANDNE     *BLANKS
     C*** RE-ASSIGN ***
     C                   MOVE      ' '           QQRTCD            1
     C                   MOVE      SCSDT         #$SDT             8
     C                   MOVE      SCSTM         #$STM             6
     C                   CALL      'IS115R1'
     C                   PARM                    #$SDT
     C                   PARM                    #$STM
     C                   PARM                    SCDOCT
     C                   PARM                    SCDCID
     C                   PARM                    SCFL01
     C                   PARM                    SCFLR
     C                   PARM                    SCSFLR
     C                   PARM                    SCPTH
     C                   PARM                    SCUSRQ
     C                   PARM                    SCTOQ
     C                   PARM                    SCDESC
     C                   PARM      *BLANKS       QQRTC1            1
     C*
     C                   ADD       1             K
     C     K             CHAIN     SCR01SA                            63
     C                   ENDDO
     C*
     C**** DO TRANSFER THEN END!!!!!!!!
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
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANK        SCSEL             1
     C*
     C                   MOVE      *OFF          *IN60
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C****************     DO   13        X       50
     C                   Z-ADD     1             X                 5 0
     C     *IN60         DOWEQ     *OFF
     C*
     C     SCUSRQ        READE     PIE01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2B
     C*
     C**** CVT DATE TO MMDDYYYY
     C*
     C                   Z-ADD     IESDT         SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCSDT
     C*
     C********** IESDMD    CAT  IESDYY    XXMDY   8
     C******************** MOVE XXMDY     SCSDT
     C******************   MOVELIESDT     SCSDT
     C*
     C                   MOVEL     IESTM         SCSTM
     C                   MOVEL     IEDOCT        SCDOCT
     C                   MOVEL     IEDCID        SCDCID
     C*
     C                   MOVEL     IEFL01        SCFL01
     C** BUILD SCPTH FOLD/SFLR CAT.
     C     DOKY          CHAIN     PDO01                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     DOFLR         SCFLR
     C                   MOVEL     DOSFLR        SCSFLR
     C*
     C                   MOVEL     DOFLR         XXFLR             8
     C                   MOVEL     DOSFLR        XXSFLR           12
     C     XXFLR         CAT       '/'           XX9A              9
     C     XX9A          CAT(P)    XXSFLR        SCPTH
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPTH
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   ENDIF
     C**
     C*
     C* IF FAX, GET DESCRIPTION FROM THE FAX FILE, ELSE GET FROM THE
     C* DOC TYPE.
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C     IEFL01        IFEQ      'F'
     C     IFKY          CHAIN     PIF01                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     IFDESC        SCDESC
     C                   ELSE
     C                   MOVEL     '*N/A*'       SCDESC
     C                   ENDIF
     C                   ELSE
     C     DTKY          CHAIN     PDT01                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     DTDESC        SCDESC
     C                   ELSE
     C                   MOVEL     '*N/A*'       SCDESC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   ADD       1             X
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2B         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     NOLNK         SCDESC
     C                   MOVEL     *ZEROS        SCSDT
     C                   MOVEL     *ZEROS        SCSTM
     C                   MOVEL     *BLANKS       SCDOCT
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCFL01
     C                   MOVEL     *BLANKS       SCPTH
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   Z-ADD     1             RRN2B
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   ENDIF
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     1             #@$2B                          SET TO 1ST RCD
     C****************     Z-ADDRRN2B     #@$2B
     C*
     C                   MOVEL     *BLANK        SCDOCI           12
     C                   MOVEL     *BLANK        SCDTYP            8
     C                   MOVEL     *ZEROS        SCDATE            8 0
     C                   MOVEL     *ZEROS        ##DATE            8 0
     C                   MOVEL     *ZEROS        SCTIME            6 0
     C*
     C                   ENDSR
     C*
