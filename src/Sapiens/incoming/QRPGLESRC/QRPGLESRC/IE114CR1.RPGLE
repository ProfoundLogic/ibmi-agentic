     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *****************************************************************
      /TITLE IE114CR1- CORRESPONDENCE EXIT PROGRAM
      *****************************************************************
      * Description: This program will call additional input programs *
      *                                                               *
      * Called from: IS215R   - Work with User Queues Index Entries   *
      * Calls to   : IE114CR  - CORRESPONDENCE SCREEN                 *
      *                                                               *
      *****************************************************************
      * Created on 12/03/2002 by c:02 for Project 114IYP              *
      *****************************************************************
      * Revisions:                                                    *
      *                                                               *
      * 10/14/14   C:302  Support item 84408                          *
      *                   Write document notes after deletion.        *
      *                   Program clean up.                                            *
      *                                                               *
      *****************************************************************
     FIE114CF1  CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
     FSMUPL     IF   E           K DISK
     FIMIELF    UF   E           K DISK
     F                                     INFDS(DBFDS1)
     FIMDOL1    UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     D                 DS
     D  KEY                    1     40
     D  KKCO#                  1      3  0
     D  KKFND                  4      6  0
     D  KKFYR                  7      9  0
     D  KKCASE                10     16  0

     D COMAPI          C                   CONST('WA001R')
     D ADJ             C                   CONST('ADJUSTER       ')
     D NURSE           C                   CONST('CASE NURSE     ')

     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D dn              s             75    dim(5)
     D I1              s              5  0

     DWriteNotes       pr                  extPgm('ISDOCNR')
     D ##Comm75                      75    const
     D ##DcId                        12    const
     D ##CDat                         8    const
     D ##PgmName                     10    const
     D ##PrFl                         1    const
     D ##RtCd                         1

      *===============================================================
      * Parameter Lists
      *===============================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@QID            10            10A Q ID
     C                   PARM                    @@USER           10            10A USER
     C                   PARM                    @@SDT             8 0           80 SCNDTMDY
     C                   PARM                    @@STM             6 0           60 SCTIM
     C                   PARM                    @@DCID           12            12A DCID
     C                   PARM                    @@DESC           30            30A DC DSC
     C                   PARM                    @@DOCT            8             8A DOCTYP
     C                   PARM                    @@FL01            1             1A IMG/FAX
     C                   PARM                    @@PTH            63            63A FULL PTH
     C                   PARM                    @@FLR             8             8A FOLD
     C                   PARM                    @@SFLR           12            12A SUBFLR
     C                   PARM                    @@RTNC            1             1A RTNCODE

     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40

     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

      *===============================================================
      * Key Lists
      *===============================================================

     C     DOKY          KLIST
     C                   KFLD                    KKDCID

     C     EOKY          KLIST
     C                   KFLD                    KKDCID

     C     IEKY          KLIST
     C                   KFLD                    KKQID
     C                   KFLD                    KKDCID

     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *===============================================================
      * Like Defines
      *===============================================================

     C     *LIKE         DEFINE    IEQID         KKQID
     C     *LIKE         DEFINE    IEDCID        KKDCID

     C                   EXSR      MAIN

      *===============================================================
      * MAIN - Main Processing
      *===============================================================
     C     MAIN          BEGSR

     C                   EXSR      GETCO#
     C                   EXSR      PGMSTR

     C     *IN90         IFEQ      *OFF
     C     UPTITL        CASEQ     ADJ           AUTH
     C     UPTITL        CASEQ     NURSE         AUTH
     C                   ENDCS                                                   IF NOT ADJ OR NURSE
     C                   ENDIF

     C                   EXSR      PGMEND

     C                   ENDSR                                                  MAIN
      *================================================================
      *  GETCO# - Gets company #.
      *================================================================
     C     GETCO#        BEGSR

     C                   CALL      COMAPI        COMPL                  99

     C                   ENDSR
      *===============================================================
      * PGMSTR - Starts Program
      *===============================================================
     C     PGMSTR        BEGSR

     C                   SETOFF                                       90
     C                   MOVEL(P)  @@QID         KKQID
     C                   MOVEL(P)  @@DCID        KKDCID
     C     DOKY          CHAIN(N)  PDO01                              99
     C     *IN99         IFEQ      *OFF
     C     DODKEY        IFNE      *BLANKS
     C                   MOVEL(P)  DODKEY        KEY
     C                   ELSE
     C                   EXFMT     WDW01
     C                   SETON                                        90
     C                   ENDIF
     C                   ELSE
     C                   ENDIF

     C     *IN90         IFEQ      *OFF

     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       UPTITL
     C                   ENDIF
     C                   ENDIF

     C                   ENDSR                                                  PGMSTR
      *===============================================================
      * AUTH - CORRESPONDENCE SCREEN
      *===============================================================
     C     AUTH          BEGSR
     C                   SETOFF                                       KL

     C     *INKL         IFEQ      *OFF

     C     IEKY          CHAIN(N)  PIE01                              99
     C     *IN99         IFEQ      *OFF

     C                   CALL      'IE114CR'
     C                   PARM      KKFND         ##FND             3 0          Group #
     C                   PARM      KKFYR         ##FYR             3 0          Year
     C                   PARM      KKCASE        ##CASE            7 0          Case
     C                   PARM      DBRRN1        ##RRN             9 0          RRN IMIEP
     C                   PARM      @@DESC        ##DESC           30            Description
     C                   PARM      UPTITL        ##TITL           15            Title
     C                   PARM      UPOUTQ        ##OUTQ           10            Out Queue
     C                   PARM      *BLANKS       ##RTRN            6            Return Codes

     C                   ENDIF

     C     ##RTRN        IFEQ      *BLANKS
     C                   MOVEL(P)  *BLANKS       @@RTNC
     C                   ELSE
     C     ##RTRN        IFEQ      'REMOVE'
     C                   MOVEL     'R'           @@RTNC
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   ENDIF

     C                   ENDSR
      *===============================================================
      * PGMEND - Ends Program
      *===============================================================
     C     PGMEND        BEGSR

     C     *IN90         IFEQ      *OFF
     C     @@RTNC        IFEQ      'R'
     C     IEKY          CHAIN     PIE01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'DL'          IEDSTS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         IELDT
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVE      DSTIME        IELTIM
     C                   MOVEL     Q1PGM         IELPGM
     C                   UPDATE    PIE01
     C                   ENDIF
     C     DOKY          CHAIN     PDO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'DL'          DODSTS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         DOLDT
     C                   MOVEL     Q1USER        DOLUSR
     C                   MOVE      DSTIME        DOLTIM
     C                   MOVEL     Q1PGM         DOLPGM
     C                   UPDATE    PDO01
     C                   ENDIF
      ***   Beg chg 10/14/14
     C                   EXSR      DLTNOTES
      ***   End chg 10/14/14
     C                   ENDIF
     C                   ENDIF

     C                   SETON                                        LR

     C                   ENDSR
      ***   Beg Chg 10/14/14
      /Free

       //====================================================================
       // DLTNOTES -  Update document notes with removal information
       //====================================================================
       Begsr DLTNOTES;

          dn(1) = 'Date: ' + %Char(%Date():*mdy/) + ' '
                + 'Time: ' + %char(%Time():*usa) + ' '
                + 'REMOVED';
          dn(2) = 'Document ' + %trim(kkDcId) + ' '
                + 'has been removed from Queue: ' + %trim(kkQid);
          dn(3) = '   and marked DELETED from the system.';
          dn(4) = 'By user: ' + %trim(Q1User) + '.';
          dn(5) = *blanks;
          For i1 = 1 to 5;
            WriteNotes(dn(i1):kkDcId:%char(%Date():*iso0):q1Pgm
                       :'H':@@RtnC);
          Endfor;

       EndSr;

      /End-free
      ***   End Chg 10/14/14
