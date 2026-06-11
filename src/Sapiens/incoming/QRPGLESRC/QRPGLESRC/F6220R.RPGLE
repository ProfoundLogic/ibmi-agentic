     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F6220R -  PROMPT FOR EDI MEDICAL 824 LOAD PROCESS
     H*
     H*****************************************************************
     H* Description:  This pgm allows the user to load the Med 824    *
     H*   file from the shared folders drive to the Med 824 database  *
     H*   file.                                                       *
     H*                                                               *
     H* Calls to . :  F6220C                                          *
     H* Called from:  menu                                            *
     H* Created by :  C:15      Project#: KY010       11/29/1999      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  09/21/06 C:79  PROJECT: 25MNZ
     H*                 DO NOT WRITE INTO THE FISERV FIELDS
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     FF6220F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FFEA1P     IF   E           K DISK    USROPN
     FFEAHP     O  A E             DISK
     FFEA2P     O  A E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  TRLAK1                 1     12
     D  XTTSET                 1      3
     D  XTRCNT                 4     12
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D HEDAK1        E DS                  EXTNAME(FEAHP)
     D DETAK1        E DS                  EXTNAME(FEA2P)

     C*================================================================
     C*  Define Key lists
     C**********************
     C*  Key list for the User Parameter file
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Key list for the Company Master file
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  Key list for the Routing file
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
     C*  Key list for the State Table file
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*******************
     C*  Define PLISTS
     C*******************
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
     C************************
     C*  Define work fields
     C************************
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    *INLR         XXHDRF
     C     *LIKE         DEFINE    *INLR         XXTRLF
     C     *LIKE         DEFINE    ORFIL#        XXFILE
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C                   MOVE      *ZEROS        XXOTDT            8 0
     C                   MOVE      *ZEROS        XXOTTM            6 0
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVE      *ZEROS        XXRCNT            9 0
     C                   MOVE      *ZEROS        XXCNT             9 0
     C                   MOVE      *OFF          XXHDRF
     C*
     C*===== MAINLINE =================================================
     C*
     C     STRT01        TAG
     C*
     C*  Retrieve the User parm file
     C*
     C     UPKY          CHAIN     PUP01                              60
     C   60              Z-ADD     *ZERO         UPCO#
     C*
     C*  Retrieve the Company Master
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*  Initialize the Record Format
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCJST
     C*________________________________________________________________
     C*   Display all formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*________________________________________________________________
     C*   Process all command keys
     C*
     C     *IN11         IFEQ      *ON
     C*************
     C*  F3=EXIT
     C*************
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C***************
     C*  F10=SEARCH
     C***************
     C     *INKJ         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C* JURISDICTION STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCJST'
     C                   CALL      'SY600R'
     C                   PARM      '  '          ##JST             2
     C                   PARM      ' '           ##RTCD            1
     C     ##RTCD        CABEQ     'A'           TERM
     C*
     C     ##RTCD        IFEQ      'B'
     C     ##JST         OREQ      '  '
     C                   GOTO      STRT
     C                   ENDIF                                                  ##RTCD = B
     C*
     C     ##RTCD        IFEQ      '*'
     C                   MOVEL(P)  ##JST         SCJST
     C                   SETON                                        25        pos cursor
     C                   GOTO      STRT
     C                   ENDIF                                                  ##RTCD = '*'
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ = *ON
     C*********************************
     C*  F13 = Change user parameters
     C*********************************
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'F0006R'
     C                   GOTO      STRT01
     C*
     C                   ENDIF
     C*****************
     C*  Help Request
     C*****************
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*****
     C                   ENDIF                                                  End of F keys
     C*================================================================
     C*  Process Prompt
     C*
     C     SCR01T        TAG
     C*
     C*  Validate Jurisdiction State
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  Call the Med 824 flatfile load program
     C*
     C                   CALL      'F6220C'
     C*
     C                   OPEN      FEA1P
     C*
     C*  Retrieve the Report Distribution record and print routing
     C*
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        XXTITL
     C                   EXCEPT    RPDIST
     C                   ENDIF
     C*
     C                   EXCEPT    RPHDR
     C                   MOVE      *OFF          *INOF
     C*________________________________________________________________
     C*   All edits have passed.  Read the Acknowledgement File and
     C*   do the following:
     C*   1. Retrieve the first record and insure it is the
     C*      acknowledgement header.
     C*   2. Retrieve the subsequent records and write them to the
     C*      acknowledgement detail formatted file.
     C*   3. when the acknowledgement trailer is encountered, compare
     C*      the count to the detail records read and written, and
     C*      write to the load report.
     C*   4. Repeat steps 1 through 3 for each transmission set
     C*      encountered.
     C*
     C                   READ      PA101                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*  Check for the acknowledgement header.
     C*
     C     XXHDRF        IFEQ      *OFF
     C                   EXSR      SRXHDR
     C*
     C     XXHDRF        IFEQ      *OFF
     C                   EXCEPT    RPER01
     C                   GOTO      TERM
     C                   ELSE
     C                   READ      PA101                                  60
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Write the acknowledgement details until trailer found.
     C*
     C                   EXSR      SRXDTL
     C*
     C     XXTRLF        IFEQ      *ON
     C*
     C                   MOVE      XTRCNT        XXCNT
     C     XXCNT         IFNE      XXRCNT
     C                   MOVE      *ON           *IN41
     C                   MOVE      'Y'           AHERRF
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF
     C*
     C                   MOVE      AHOTDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXOTDT
     C                   MOVE      AHOTTM        XXOTTM
     C                   EXCEPT    RPDTL
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    RPHDR
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   MOVE      XXRCNT        AHRCNT
     C                   MOVE      'P'           AHSTAT
     C                   WRITE     PAH01
     C                   MOVE      *ZEROS        XXRCNT
     C                   MOVE      *OFF          XXHDRF
     C*
     C                   ENDIF
     C*
     C                   READ      PA101                                  60
     C                   ENDDO
     C*
     C*  Last record found should be trailer.
     C*
     C     XXTRLF        IFEQ      *OFF
     C                   EXCEPT    RPER02
     C                   ENDIF
     C*
     C*  End Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*================================================================
     C*   Routine - SRXHDR
     C*
     C*   Save Acknowledgement in header structure.
     C*
     C     SRXHDR        BEGSR
     C*
     C                   CLEAR                   HEDAK1
     C*** BEG CHG *** 09/21/06 **********************************
     C***********          MOVELA1DATA    HEDAK1
     C     118           SUBST     A1DATA:1      DTA118          118
     C                   MOVEL     DTA118        HEDAK1
     C*** BEG CHG *** 09/21/06 **********************************
     C     AHTHDR        IFEQ      'HD1'
     C                   MOVE      *ON           XXHDRF
     C                   MOVE      UPCO#         AHCO#
     C                   MOVE      SCJST         AHJST
     C                   ENDIF
     C*
     C     ENXHDR        ENDSR
     C*================================================================
     C*   Routine - SRXDTL
     C*
     C*   Move the acknowledgement detail into structure and write.
     C*
     C     SRXDTL        BEGSR
     C*
     C                   MOVE      *OFF          XXTRLF
     C*
     C                   CLEAR                   DETAK1
     C*** BEG CHG *** 09/21/06 **********************************
     C************         MOVELA1DATA    DETAK1
     C     256           SUBST     A1DATA:1      DTA256          256
     C                   MOVEL     DTA256        DETAK1
     C     123           SUBST     A1DATA:1      DTA123          123
     C*******              MOVE *ZEROS    ZERO97 97
     C*******              MOVE ZERO97    DTA123
     C                   MOVEL     DTA123        DETAK1
     C*** BEG CHG *** 09/21/06 **********************************
     C*
     C*******    A2RTYP    IFEQ 'AK1'
     C     A2TAK1        IFEQ      'AK1'
     C                   MOVE      AHOTDT        A2OTDT
     C                   MOVE      AHOTTM        A2OTTM
     C                   MOVE      *BLANKS       A2STAT
     C                   MOVE      *BLANKS       A2ERRF
     C                   MOVE      UPCO#         A2CO#
     C                   MOVE      SCJST         A2JST
     C                   ADD       1             XXRCNT
     C                   WRITE     PA201
     C*
     C                   ELSE
     C*
     C*******    A2RTYP    IFEQ 'TR1'
     C     A2TAK1        IFEQ      'TR1'
     C                   CLEAR                   TRLAK1
     C                   MOVEL     A1DATA        TRLAK1
     C                   MOVE      *ON           XXTRLF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ENXDTL        ENDSR
     C**************************************************************************
     OQSYSPRT   E            RPDIST         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDIST         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDIST         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDIST         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDIST         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDIST         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDIST         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDIST         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDIST         2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            RPDIST         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDIST         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           RPDIST
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1999'
     O          E            RPDIST         1 59
     O                       #BRND1
     O          E            RPDIST         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            RPHDR          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    128
     O          E            RPHDR          2
     O                       Q1PGM               10
     O                       XXTITL              99
     O                                          123 'PAGE:'
     O                       PAGE          Z    128
     O          E            RPHDR          2
     O                                           19 'JURISDICTION STATE:'
     O                       SCJST               22
     O          E            RPHDR          2
     O                                            7 'T-SET'
     O                                           20 'SENDER FEIN'
     O                                           35 'RECEIVER FEIN'
     O                                           58 'ORIGINAL TRANSMISSION'
     O                                           64 'TEST'
     O                                           78 'RECORDS SENT'
     O                                           96 'RECORDS RECEIVED'
     O                                          104 'ERROR'
     O*
     O          E            RPDTL          1
     O                       AHTSET               6
     O                       AHSEIN              18
     O                       AHREIN              31
     O                       XXOTDT        Y     45
     O                       XXOTTM              55 '  :  .  '
     O                       AHTPFG              62
     O                       XXCNT         Z     78
     O                       XXRCNT        Z     96
     O                       AHERRF             102
     O          E            RPER01         1
     O                                           50 'NO HEADER FOUND'
     O          E            RPER02         1
     O                                           50 'NO TRAILER FOUND'
