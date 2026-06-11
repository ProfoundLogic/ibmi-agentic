     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/TITLE IE114CR - Correspondence screen
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
      ****************************************************************************
      * Description:                                                             *
      *                                                                          *
      * Screen would present on the view document option in the work with        *
      * incoming documents screen.  The user title must equal to "adjuster"      *
      * and the document type would any other than medical bill.                 *
      * Remark field would auto populate with the current remark on the          *
      * document.  User would have the option to enter a new remark.             *
      * If the user selects yes to retain on queue, the document would           *
      * remain in the user's work with documents queue, otherwise it would       *
      * be removed.                                                              *
      * On [enter] the remark would update and the user would return to          *
      * work with incoming documents.                                            *
      * On [canel] or [exit] the screen would return to work with incoming       *
      * documents.                                                               *
      *                                                                          *
      *                                                                          *
      * Called from: IS215R   - Work with User Queues Index Entries              *
      * Calls to   : W4110R27 - Additional Info Screen                           *
      *              W4110C   - Department Letter Print Job                      *
      *                                                                          *
      ****************************************************************************
      * Revisions:                                                               *
      *
      * 06/09/20 C:212    Project : SY420
      *                   Mask SSN
      *                                                                          *
      * 06/26/14 c:302    Project 114INP                                         *
      *                   - Add Reassign functionality and make                  *
      *                   sure to save comments in the process.                  *
      *                   - Clean up program                                     *
      *                                                                          *
      * 01/22/14 c:062    Support IM114 83760                                    *
      *                   Remove edit since screen fields were                   *
      *                   removed.                                               *
      *                                                                          *
      * 09/30/13 c:191    Project 114WJC                                         *
      *                   - Add function key for Reassign                        *
      *                                                                          *
      * 01/25/12 c:190    Project 114IZS                                         *
      *                   -Remark comment change on Medical Bill                 *
      *                    authorization screen.                                 *
      *                                                                          *
      * 01/27/03 c:62 for Project 114MDY                                         *
      *                   -Add adequate medical reserves logic.                  *
      *                   -Add F8 and F14.                                       *
      *                                                                          *
      ****************************************************************************
     FIE114CF   CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FIMDOLD    IF   E           K DISK
     F                                     RENAME(PDO01:PDO03)
     FIMIEP     IF   E             DISK
     FIMDTL     IF   E           K DISK    Rename(PDt01:pdt00)
     FSDCMLE    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0X)
     F                                     INFDS(DBFDS1)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     D                 DS
     D  $$PARM                 1    136
     D  $$CO#                  1      3
     D  $$FND                  4      6
     D  $$REQ#                 7     11
     D  $$DPCD                12     16
     D  $$DCID                17     28
     D  $$DESC                29     56
     D  $$FLDR                57    136

     D                 DS
     D  SCCLM#                 1     11
     D  SCFYR                  1      3  0
     D  XXHYPH                 4      4    INZ('-')
     D  SCCASE                 5     11  0

     D                 DS
     D  XXSS#                  1      9
     D  XXSSA                  1      3
     D  XXSSB                  4      5
     D  XXSSC                  6      9

     D                 DS
     D  SCSS#                  1     11
     D  SCSSA                  1      3
     D  SCHY1                  4      4    INZ('-')
     D  SCSSB                  5      6
     D  SCHY2                  7      7    INZ('-')
     D  SCSSC                  8     11
       // DARTS
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0

     D OPTNM           C                   CONST('Correspondence Proc-
     D                                     essing')
     D MSG1            C                   CONST('This document was processed -
     D                                     on ')
     D MSG2            C                   CONST(' by ' )
     D PDSNAM          C                   CONST('Progressive Data So-
     D                                     lutions, Inc.')
     D COMAPI          C                   CONST('WA001R')

     D FIDS01        E DS                  EXTNAME(SZQ2P)

      // Parameter Lists

     C     *ENTRY        PLIST
     C                   PARM                    @@FND             3 0          Group #
     C                   PARM                    @@FYR             3 0          Year
     C                   PARM                    @@CASE            7 0          Case
     C                   PARM                    @@RRN             9 0          RRN IMIEP
     C                   PARM                    @@DESC           30            Description
     C                   PARM                    @@TITL           15            Title
     C                   PARM                    @@OUTQ           10            Out Queue
     C                   PARM                    @@RTRN            6            Return Codes

     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40


     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN         ##RRN
     C                   PARM      Q1PGM         ##PGM
      // Key Lists
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE

     C     DOKY2         KLIST
     C                   KFLD                    IEDCID

     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV

     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND

     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY

     C     DRK           KLIST
     C                   KFLD                    DODKEY
     C                   KFLD                    DODSQ#
      * Like Defines
     C     *LIKE         DEFINE    DODCID        KKDCID
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
      *
     C                   EXSR      MAIN

      *===============================================================
      * ERRCK1 - Screen edits
      *===============================================================
     C     ERRCK1        BEGSR
      * Validate the comment has a value in it.
     C     *IN90         IFEQ      *OFF
     C     SCCOMM        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
      * Validate the retain on queue flag is either "Y" or "N".
     C     *IN90         IFEQ      *OFF
     C     SCROQ         IFNE      'Y'
     C     SCROQ         ANDNE     'N'
     C                   SETON                                        7490
     C                   ENDIF
     C                   ENDIF
      *  We have passed all edits.
     C                   ENDSR

      *================================================================
      *  FINISH - Final processing for file update/display
      *================================================================
     C     FINISH        BEGSR
      * Only two things happen here, 1) update comment  2)retain on queue
     C     SCROQ         IFEQ      'Y'
      * Retain!!
     C                   MOVEL     *BLANKS       @@RTRN
     C                   ELSE
     C                   MOVEL     'REMOVE'      @@RTRN
     C                   ENDIF

     C                   EXSR      WRTUPD

     C                   ENDSR

      *****************************************************************
      * WRTUPD - Writes WCMS Department Letter Record
      *****************************************************************
     C     WRTUPD        BEGSR

     C                   MOVE      DBRRN1        ##RRN1            9 0
     C                   CALL      'DA704R'
     C                   PARM                    ##RRN1
     C                   PARM      SCCOMM        ##COMM          256
     C                   PARM      Q1PGM         ##PGM

     C                   ENDSR

      *================================================================
      *  GETCOF - Gets company and fund names if possible; otherwise
      *           get PDS name for company name
      *================================================================
     C     GETCOF        BEGSR

     C                   CALL      COMAPI        COMPL                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  PDSNAM        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM

     C                   ENDSR

      *================================================================
      *  GETPGT - Get program title for display heading
      *================================================================
     C     GETPGT        BEGSR

     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS

     C                   ENDSR

      *================================================================
      *  KGDO - F7=Image Notes
      *================================================================
     C     KGDO          BEGSR

     C                   CALL      'IS016R'
     C                   PARM      IEDCID        $$DCID           12
     C                   PARM      'IS215R'      $$PNAM           10
     C                   SETON                                        90

     C                   ENDSR

      *===============================================================
      * KHDO - Subroutine for F8=Claims
      *===============================================================
     C     KHDO          BEGSR

     C                   CALL      'W0070R'                             99
     C                   PARM      '1'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *BLANKS       ##SORT            2
     C                   SETON                                        90

     C                   ENDSR

      *===============================================================
      * KIDO - F9
      *===============================================================
     C     KIDO          BEGSR
     C                   MOVE      'C'           ##MODE
     C                   CALL      'W0070RX1'    ACDPL1                 99

     C                   ENDSR

      *===============================================================
      * KJDO - F10 Reassign
      *===============================================================
     C     KJDO          BEGSR
       //    Beg Change  06/26/14
      /Free
       Exsr ERRCK1;
       If *in90 = *off;
          Exsr FINISH;
      /End-Free
       //    End Change  06/26/14


     C                   Z-ADD     IESDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         #$SDT             8

     C                   MOVE      IESTM         #$STM             6
     C                   EVAL      @#PTH = %trim(DOFLR) + '/' + %trim(DOSFLR)
     C     IEDoct        Chain     pdt00
     C                   If        %found(imdtl)
     C                   Eval      @#DDSC = DTDESC
     C                   Else
     C                   Eval      @#DDSC = '*N/A*'
     C                   Endif
     C                   eval      scReassign=' '

     C                   CALL      'IS115R'
     C                   PARM                    #$SDT
     C                   PARM                    #$STM
     C                   PARM      IEDOCT        QQDOCT            8
     C                   PARM      IEDCID        QQDCID           12
     C                   PARM      IEFL01        QQDCT             1
     C                   PARM      DOFLR         @#FLR             8
     C                   PARM      DOSFLR        @#SFLR           12
     C                   PARM                    @#PTH            63
     C                   PARM      IEQID         QQQID            10
     C                   PARM                    @#DDSC           30
     C                   PARM      *BLANKS       @#RTC1            1

     C                   Select
      *  End Program
     C     @#RTC1        Wheneq    'A'
     C                   eval      *inkc=*On
      *  Cancel
     C     @#RTC1        Wheneq    'B'
     C                   eval      *inkl=*On
      *  Not Reassigned
     C     @#RTC1        Wheneq    '#'
     C                   eval      scReassign='**Not Reassigned**'
      *  Reassigned
     C                   Other
       //    Beg Change  06/26/14
      /Free
          Exsr DNOT;
      /End-free
       //    End Change  06/26/14
     C                   eval      *inlr=*On
     c                   Return
     C                   EndSl
       //    Beg Change  06/26/14
      /Free
       Endif;
      /End-Free
       //    End Change  06/26/14

     C                   ENDSR

      *===============================================================
      * KNDO - Subroutine for F14 = DARTS
      *===============================================================
     C     KNDO          BEGSR
     C*  Establish key
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C*  Call darts entry program
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      ##PGM            10
     C                   SETON                                        90

     C                   ENDSR

      *===============================================================
      * MAIN - Main Processing
      *===============================================================
     C     MAIN          BEGSR

     C                   MOVEL     Q1USER        SCUSER           10
     C                   EXSR      GETCOF
     C                   EXSR      GETPGT
     C                   EXSR      PGMSTR

     C     *IN90         DOUEQ     *OFF
     C     *INKC         OREQ      *ON
     C                   EXFMT     SCR01
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   SETOFF                                       90
     C     *INKC         IFEQ      *OFF
     C     *INKG         CASEQ     *ON           KGDO
     C     *INKH         CASEQ     *ON           KHDO
     C     *INKI         CASEQ     *ON           KIDO
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS

     C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCK1
     C     *IN90         IFEQ      *OFF
      * If no errors we do the finish subroutine. this is where we updte
     C                   EXSR      FINISH
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO

     C                   EXSR      PGMEND

     C                   ENDSR

      *===============================================================
      * PGMEND - Ends Program
      *===============================================================
     C     PGMEND        BEGSR

     C     *INKC         IFEQ      *ON
     C                   MOVEL     *BLANKS       @@RTRN
     C                   ENDIF
     C                   SETON                                        LR
     C                   RETURN

     C                   ENDSR

      *===============================================================
      * PGMSTR - Starts Program
      *===============================================================
     C     PGMSTR        BEGSR

     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   eval      scReassign=' '
     C* Let's get the WCMC Subscriber/Group
     C                   MOVE      ##CO#         #1CO#             3
     C                   MOVE      KKFND         #1FND             3
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMEMP#        SCEMP#
     C                   Z-ADD     CMDIV         SCDIV
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C                   MOVEL(P)  CMNATR        SCNATR
     C                   MOVEL(P)  CMPART        SCPART
     C                   MOVEL(P)  CMCAUS        SCCAUS
     C                   MOVEL(P)  CMACL#        SCACL#
     C                   MOVEL(P)  CMSS#         XXSS#
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  XXSSA         SCSSA
     C***                MOVEL(P)  XXSSB         SCSSB
     C***                MOVEL(P)  XXSSC         SCSSC
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'XXSS#':XXSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
      * Default retain on queue to "Y"
     C                   MOVEL     'N'           SCROQ
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C     CMLSN         CAT(P)    ',':0         SCCNAM
     C                   CAT       CMFSN:1       SCCNAM
     C                   CAT       CMMID:1       SCCNAM

     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCGNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCGNAM
     C                   ENDIF

     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCENAM
     C                   ENDIF

     C                   MOVEL(P)  CMNATR        KKKEY            10
     C     TBKY          CHAIN     PNA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        SCNATD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNATD
     C                   ENDIF

     C                   MOVEL(P)  CMPART        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        SCPARD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPARD
     C                   ENDIF

     C                   MOVEL(P)  CMCAUS        KKKEY
     C     TBKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CADESC        SCCAUD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCAUD
     C                   ENDIF

     C                   MOVEL(P)  'Y'           KKPRIM            1

     C     @@RRN         CHAIN     PIE01                              99

      * Get DART remark to display (comment)
      * remark will be updated ***
      *
      * Use docid to get the indexed info, then DARTs.....
      *
      * 1) Get imdop1, from there 2) get darts key/seq 3) display the remark,
      *    and upon "enter" update the comment.  if retain on q
      *    then don't do the remove else allow the remove.

      * Get the do record using docid
     C     DOKY2         CHAIN     PDO03                              99
     C     *IN99         IFEQ      *OFF
     C     DODKEY        ANDNE     *BLANKS
      *  If hit, and if index is not blanks !!!
      *  now get the darts record

     C     DRK           CHAIN(N)  PCM0X                              99

     C     *IN99         IFEQ      *ON
     C                   EXSR      PGMEND
     C                   ELSE
      * Put comment in the screen field
     C                   MOVEL     CMCOMM        SCCOMM
     C                   ENDIF
     C                   ELSE

     C                   EXSR      PGMEND

     C                   ENDIF
     C                   MOVEL     *BLANKS       SCADQ             1

     C                   ENDSR

      *====================================================================*
      * DOC NOTES  - Create doc notes
      *====================================================================*
     C     DNOT          BEGSR
      *
      * Fill all doc note fields
      * Get current date/time to create notes...
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        #$NOWD            8
     C                   MOVE      DSTIME        #$NOWT            6
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     #$NOWD        XXM               2
     C                   MOVE      #$NOWD        XXY               4
     C     2             SUBST     #$NOWD:3      XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     #$NOWT        XXH               2
     C                   MOVE      #$NOWT        XXS               2
     C     2             SUBST     #$NOWT:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A
     C     XX3A          CAT(P)    XXM           XX5A
     C     XX5A          CAT(P)    ':'           XX6A
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
     C     MSG1          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    MSG2:1        XX75
     C                   CAT(P)    Q1USER:1      XX75
      * Call pgm to write doc notes
     C                   MOVE      '1'           Q$RTC             1

     C                   CALL      'ISDOCNR'
     C                   PARM                    xx75
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    #$NOWD                           NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE

     C                   ENDSR
