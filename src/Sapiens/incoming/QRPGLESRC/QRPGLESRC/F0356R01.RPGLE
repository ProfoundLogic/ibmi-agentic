     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  F0356R01 - DETAIL EOB CODE/NOTES INFORMATION   C:62 06/23/98 *
     H*                                                               *
     H*  CALLED FROM: F0356R, F0353R, F0363R, F0373R, F0383R          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  10/10/23 C:079 Project: 99/114288                            *
     H*                 Dont cut off words on EOB Code Description    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*  08/18/22 C:333 Project: MC443                                *
     H*                 Modernized EOB Print                          *
     H*                                                               *
     H*  05/07/18 C:079 Project: 114MJC                               *
     H*                 Add EOB Cross reference 1 and 2 fields        *
     H*                                                               *
     H*  12/09/16 C:212 Project: 25MUH                                *
     H*                 change edit to allow > 2 user specific EOB    *
     H*                 for TASB only                                 *
     H*                                                               *
     H*  12/05/12 C:280 Support: 81222                                *
     H*                 change how to determine if window should be   *
     H*                 displayed                                     *
     H*                                                               *
     H*  08/07/12 C:280 Project: 114mer                               *
     H*                 change the override commands                  *
     H*                                                               *
     H*  05/04/11 C:191 Project: FS389                                *
     H*                 Add two new EOB notes lines 3rd and 4th       *
     H*                                                               *
     H*  12/19/05 C:79  Support: MC071 50401                          *
     H*                 For ECM always allow continuous entry         *
     H*                 For PPC always allow continuous entry         *
     H*                                                               *
     H*  10/05/05 C:79  Support: MC010 49208                          *
     H*                 For Summit always allow continuous entry      *
     H*                                                               *
     H*  06/24/05 C:79  Support: MC135 47606                          *
     H*                 If Texas bill Check for an EOB code           *
     H*                                                               *
     H*  05/10/05 C:62  Project: MC129 46961                          *
     H*                 Only do what was programmed in 2/28 change if *
     H*                 called from F0356R.                           *
     H*                                                               *
     H*  05/10/05 C:62  Project: MC129 46961                          *
     H*                 Only do what was programmed in 2/28 change if *
     H*                 called from F0356R.                           *
     H*                                                               *
     H*  02/28/05 C:79  Project: MC267 - Base change                  *
     H*                 After adding an eob code or note, after entry *
     H*                 stay in add mode                              *
     H*                                                               *
     H*  02/18/05 C:079 Project: TX029                                *
     H*                 Load 2nd desc line from FTEBPxx               *
     H*                                                               *
     H*  06/09/04 C:062 Support: MC025 42776                          *
     H*                 Correct the overide of FTEBP                  *
     H*                                                               *
     H*  01/29/03 C:079 Support: MC095 35654                          *
     H*                 Dont include inactive EOB codes in max of 2   *
     H*                                                               *
     H*  11/08/02 C:079 Support: MC090 34798                          *
     H*                 Give error when entering an inactive value    *
     H*                                                               *
     H*  06/27/02 C:062 Project: FS235                                *
     H*                 DEFAULT PRINT FLAG WITH 'Y'                   *
     H*                                                               *
     H*  05/31/02 C:062 Project: 114MDC                               *
     H*                 -ADD PROCESSING FOR SOFT DELETION OF RECORD.  *
     H*                 -PROTECT PRNT FLAG IF RECORD IS INACTIVE.     *
     H*
     H*  01/07/02 C:062 PROJECT: FS235
     H*                 ADD PRINT FIELD.
     H*
     H*  03/23/01 C:062 Project: FSMNT  BASE ONLY
     H*                 COMPILE ONLY
     H*
     H*  12/11/00 C:062 Project: MCMNT
     H*                 POPULATE LINE# AND SEQ#.
     H*
     H*  09/26/00 C:062 Project: MCMNT
     H*                 ADDED TWO ADDITIONAL FILE OVERRIDES.
     H*
     H*  02/08/00 C:31  Project: FSMNT
     H*                 FSMS Maintenance
     H*                 1. Add "Source" and "Creation" information
     H*
     H*  08/24/99 C:56  Project: MC152 - Base change
     H*             1.  After adding an eob code or note, after entry *
     H*                 return to calling program.                    *
     H*                                                               *
     H*  03/31/99 C:56  PROJECT: MC152                                *
     H*                 Add @@DSEQ as an entry parm                   *
     H*                                                               *
     H*  12/02/98 C:79  PROJECT: MC152                                *
     H*                 Use CAEOB# instead of CADCS# to load KKEOB#   *
     H*                                                               *
     H*  10/21/98 C:56  PROJECT: MC152                                *
     H*                 PER LORRI DAPPER, ALLOW EITHER EOB CODE OR    *
     H*                 NOTE BUT NOT BOTH                             *
     H*  08/03/98  C:94                                               *
     H*            TOOK OUT SERVICING PROVIDER IN API HEADING         *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0356F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDDEP     UF A E             DISK
     F                                     RENAME(PDE01:DBRCDF)
     FFDDEL     IF   E           K DISK
     FFDDEL4    IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFAEOP     IF   E             DISK
     FFDCAP     IF   E             DISK
     FFTABL     IF   E           K DISK    USROPN
     FFTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
      /Copy sprModule
     D*
     D**********          QCA     1   4 56               CMD EXEC/QCMDEXC
     D**** QCA            S             56    DIM(8) CTDATA PERRCD(1)
     D QCA             S             61    DIM(8) CTDATA PERRCD(1)
     D*
     D***PY SCOPYMT01I
     D*
     D*****                 DS
     D*****  QQCMD                  1     56
     D*****  QQST                  38     39
     D*
     D                 DS
     D  QQCMD                  1     61
     D  QQST                  38     39
     D*
     D*
     D*
     D*****            DS
     D*****  Q1CMD                  1     56
     D*****  Q1ST                  40     41
     D*
     D                 DS
     D  Q1CMD                  1     61
     D  Q1ST                  40     41
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Detail EOB Co-
     D                                     des/Notes')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     D*
     D*================================================================
     D* StandAlone declarations
     D*================================================================
      *** Beg Chg *** 08/18/22 ********************************
     d*** intMaxUEOB      s              1s 0 inz
     d intMaxUEOB      s              2s 0 inz
      *** End Chg *** 08/18/22 ********************************
       //*** Beg Add *** 10/10/23 *************************************
       dcl-pr FormatNote ExtPgm('FA769R');
         decCompany Like(DEco#);
        chrEob Like(DEeobCd);
        chrNote1 Like(DEnot1);
        chrNote2 Like(DEnot2);
        chrNote3 Like(DEnot3);
        chrNote4 Like(DEnot4);
        chrAst Like(EOast);
       End-pr;
       dcl-s decCompany Like(DEco#);
       dcl-s chrEob Like(DEeobCd);
       dcl-s chrNote1 Like(DEnot1);
       dcl-s chrNote2 Like(DEnot2);
       dcl-s chrNote3 Like(DEnot3);
       dcl-s chrNote4 Like(DEnot4);
       dcl-s chrAst Like(EOast);
       //*** End Add *** 10/10/23 *************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
1    C                   PARM      *INKC         @@KC              1
2    C                   PARM      *INKL         @@KL              1
3    C                   PARM                    @@MODE            1
4    C                   PARM                    @@RRN             9 0
5    C                   PARM                    @@RRN2            9 0
6    C                   PARM                    @@RRN3            9 0
7    C                   PARM                    @@DSEQ            5 0
8    C                   PARM                    @@CHRG            9 2
9    C                   PARM                    @@PAID            9 2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     EBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     DEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKESRC
     C*
     C     DEKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    K1DSEQ
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EBCO#         KKCO#
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    DESUB#        KKSUB#
     C     *LIKE         DEFINE    DEFND         KKFND
     C     *LIKE         DEFINE    DEFYR         KKFYR
     C     *LIKE         DEFINE    DECASE        KKCASE
     C     *LIKE         DEFINE    DEEOB#        KKEOB#
     C     *LIKE         DEFINE    DEDSEQ        KKDSEQ
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C     *LIKE         DEFINE    EORCN         XXRCN
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    DEDSEQ        K1DSEQ
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C*  EDITEB - EDIT TEXAS EOB CODES
     C*================================================================
     C     EDITEB        BEGSR
     C*
     C                   MOVE      'N'           FNDEB1            1
     C                   MOVE      @@DSEQ        K1DSEQ
     C*
     C     DEKY4         CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     DESTAT        IFEQ      'A'
     C     DEESRC        ANDNE     'A'
     C*** Beg Chg *** 07/18/23 **************************************
     C***  DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C                   MOVE      'Y'           FNDEB1
     C                   LEAVE
     C                   ENDIF
     C*
     C     DEKY1         READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEOB         ANDNE     *BLANKS
     C*
     C****************** EXSR      OVRSTR
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVEL(P)  SCEOB         KKKEY
     C*
     C     EBKY          CHAIN     PEB01                              99
     C*
     C   99              SETON                                        7190
     C     EBSTAT        IFEQ      'I'
     C                   SETON                                        7190
     C                   ENDIF
     C***
     C**N99***** SCNOT1    IFEQ *BLANKS
     C**********
     C**********           MOVELEBDESC    SCNOT1    P
     C**********           SETON                     90
     C**********
     C**********           ENDIF
     C*
     C*****************  EXSR      OVREND
     C*
     C                   ENDIF
     C*
     C* VALIDATE THAT SOMETHING WAS ENTERED
     C*
     C     SCNOT1        IFEQ      *BLANKS
     C     SCEOB         ANDEQ     *BLANKS
     C     *IN90         ANDEQ     *OFF
     C*
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE THAT BOTH EOB CODE AND NOTES ARE NOT ENTERED
     C*
     C     SCEOB         IFNE      *BLANKS
     C*
     C     SCNOT1        IFNE      *BLANKS
     C     SCNOT2        ORNE      *BLANKS
     C                   SETON                                        7390
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C* CHECK TO SEE IF THE MAXIMUM NUMBER OF 'USER ENTERED' EOB CODES
     C* HAS BEEN REACHED
     C*
     C/free
        // Default the max allowed User EOB code value
        //*** Beg Chg *** 08/18/22 ******************************
        //***intMaxUEOB = 2;
        If isModEnabled('Modernized EOB Print');
           intMaxUEOB = 10;
        Else;
           intMaxUEOB = 2;
        EndIf;
        //*** End Chg *** 08/18/22 ******************************

        // For TASB, the count is different
        If xxCLT# = '025';
           intMaxUEOB = 4;
        EndIf;

      /end-free
     C     @@MODE        IFEQ      'A'
     C     SCEOB         ANDNE     *BLANKS
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     Q1PRMS        IFGE      7
     C     @@DSEQ        IFGT      *ZEROS
     C                   Z-ADD     @@DSEQ        KKDSEQ
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      'U'           KKESRC
     C                   Z-ADD     0             COUNT             2 0
     C     DEKY3         CHAIN     PDE02                              98
     C     *IN98         DOWEQ     *OFF
     C     DESTAT        IFNE      'I'
     C*** Beg Chg *** 07/18/23 **************************************
     C***  DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C                   ADD       1             COUNT
     C                   ENDIF
     C     DEKY3         READE     PDE02                                  98
     C                   ENDDO
     C***  COUNT         IFGE      2
     C     COUNT         IFGE      intMaxUEOB
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCEOB         IFNE      *BLANKS
     C  N99SCNOT1        IFEQ      *BLANKS
     C                   MOVEL(P)  EBDESC        SCNOT1
     C                   ENDIF
     C  N99SCNOT2        IFEQ      *BLANKS
     C                   MOVEL(P)  EBDSC2        SCNOT2
     C                   ENDIF
     C  N99SCNOT3        IFEQ      *BLANKS
     C                   MOVEL(P)  EBDSC3        SCNOT3
     C                   ENDIF
     C*
     C  N99SCNOT4        IFEQ      *BLANKS
     C                   MOVEL(P)  EBDSC4        SCNOT4
     C                   ENDIF
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C     SCPRNT        ANDNE     'Y'
     C     SCPRNT        ANDNE     'N'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C******************** MOVE '1'       OUTFLG
     C                   MOVE      '1'           OUTFLG
     C*
     C* Check to see if called by F0356R
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'F0356R'      ##PGM            10
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      ' '           OUTFLG
     C                   ENDIF
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C     XXCLT#        IFEQ      '010'
     C     XXCLT#        OREQ      '071'
     C     XXCLT#        OREQ      '135'
     C                   MOVE      ' '           OUTFLG
     C                   ENDIF
     C     Q1PRMS        IFGE      9
     C     EOAST         IFEQ      'TX'
     C     SCEOB         ANDEQ     *BLANKS
     C     @@PAID        ANDLT     @@CHRG
     C     @@DSEQ        ANDGT     0
     C*
     C                   EXSR      EDITEB
     C*
     C     FNDEB1        IFEQ      'N'
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCEOB'
     C*
     C****************** EXSR      OVRSTR
     C*
     C                   CALL      'F2330R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      EOAST         ##ST              2
     C                   PARM      *BLANKS       ##KEY            15
     C*
     C     *INKL         IFEQ      *ON
     C     ##KEY         OREQ      *BLANKS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ##KEY         SCEOB
     C     *IN16         IFEQ      *OFF
     C                   SETON                                        32
     C                   ELSE
     C                   SETON                                        33
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C****************** EXSR      OVREND
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C*** Beg Chg *** 07/18/23 **************************************
     C*****              MOVEL(P)  DEEOB         SCEOB
     C                   MOVEL(P)  DEEOBCD       SCEOB
     C*** End Chg *** 07/18/23 **************************************
       //*** Beg Chg *** 10/10/23 *************************************
     C****************   MOVEL(P)  DENOT1        SCNOT1
     C****************   MOVEL(P)  DENOT2        SCNOT2
     C****************   MOVEL(P)  DENOT3        SCNOT3
     C****************   MOVEL(P)  DENOT4        SCNOT4
       decCompany = DEco#;
       chrEob = DEeobCd;
       chrNote1 = DEnot1;
       chrNote2 = DEnot2;
       chrNote3 = DEnot3;
       chrNote4 = DEnot4;
       chrAst   = *blanks;
       Callp FormatNote(decCompany:chrEob:chrNote1:chrNote2:chrNote3:
          chrNote4: chrAst);
       scNot1 = chrNote1;
       scNot2 = chrNote2;
       scNot3 = chrNote3;
       scNot4 = chrNote4;
       //*** End Chg *** 10/10/23 *************************************
     C                   MOVEL(P)  DESTAT        SCSTAT
     C                   MOVEL(P)  DEESRC        SCESRC
     C                   MOVEL(P)  'EOMG'        KKTBID
     C                   MOVEL(P)  DEESRC        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSRCD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSRCD
     C                   ENDIF
     C                   MOVEL(P)  DEUSER        SCUSER
     C                   MOVEL(P)  DEPGM         SCPGM
     C                   Z-ADD     DEDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDT
     C                   MOVE(P)   DETIME        SCTIME
     C                   MOVEL(P)  DEPRNT        SCPRNT
     C     SCSTAT        IFEQ      'I'
     C                   SETON                                        16
     C                   ENDIF
      /Free
                         If is835PayAd = *on;
                            Chain (EOco#:scEob) peb01;
                            If not %eof (ftabl);
                               scXref1 = EBxref;
                               scXref2 = EBxrf2;
                            Endif;
                         Endif;
                         If @@mode = 'D';
                            Exsr ovrEnd;
                         Endif;
      /End-free
     C*
     C                   ELSE
     C*
     C**********           MOVEL*BLANKS   SCPRNT    P
     C                   MOVEL(P)  ##PREO        SCPRNT
     C                   MOVEL(P)  *BLANKS       SCEOB
     C                   MOVEL(P)  *BLANKS       SCNOT1
     C                   MOVEL(P)  *BLANKS       SCNOT2
     C                   MOVEL(P)  *BLANKS       SCNOT3
     C                   MOVEL(P)  *BLANKS       SCNOT4
     C                   MOVEL(P)  'A'           SCSTAT
     C                   MOVEL     *BLANKS       SCSRCD
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     EOCO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C     Q1PRMS        IFGE      7
     C     @@DSEQ        IFGT      *ZEROS
     C                   Z-ADD     @@DSEQ        DEDSEQ
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     XXSEQ#        DESEQ#
     C*
     C                   ENDIF
     C*
     D*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  SCEOB         DEEOB
     C                   MOVEL(P)  SCEOB         DEEOBCD
     D*** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  SCNOT1        DENOT1
     C                   MOVEL(P)  SCNOT2        DENOT2
     C                   MOVEL(P)  SCNOT3        DENOT3
     C                   MOVEL(P)  SCNOT4        DENOT4
     C                   MOVEL     SCSTAT        DESTAT
     C                   MOVEL(P)  SCPRNT        DEPRNT
     C                   Z-ADD     XXRCN         DERCN
     C*
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OVREND - END OVERIDE OF FTEBP AND FTABL
     C*================================================================
     C     OVREND        BEGSR
     C*
     C                   CLOSE     FTABL
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDSR
     C*================================================================
     C* OVRSTR - START OVERIDE OF FTEBP AND FTABL
     C*================================================================
     C     OVRSTR        BEGSR
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   MOVEL     QCA(2)        QQCMD
     C                   MOVEL     EOAST         QQST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   MOVEL     QCA(4)        QQCMD
     C                   MOVEL     EOAST         QQST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   MOVEL     QCA(6)        Q1CMD
     C                   MOVEL     EOAST         Q1ST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    Q1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C*****              Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C                   MOVEL     QCA(8)        Q1CMD
     C                   MOVEL     EOAST         Q1ST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    Q1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      FTABL
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      /Free
                         Exsr ovrEnd;
      /End-free
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1
     C                   PARM      *BLANK        ##MCIF            1
     C                   PARM      *BLANK        ##WCIN            1
     C                   PARM      *BLANK        ##AR              1
     C                   PARM      *BLANK        ##NTWK            1
     C                   PARM      *BLANK        ##UR              1
     C                   PARM      *BLANK        ##MED             1
     C                   PARM      *BLANK        ##EDI             1
     C                   PARM      *BLANK        ##WCDK            1
     C                   PARM      *BLANKS       ##RHAB            1
     C                   PARM      *BLANKS       ##MSS             1
     C                   PARM      *BLANKS       ##FSCH            1
     C                   PARM      *BLANKS       ##PRVB            1
     C                   PARM      *BLANKS       ##NPWC            1
     C                   PARM      *BLANKS       ##OS              1
     C                   PARM      *BLANKS       ##ARLS            1
     C                   PARM      *BLANKS       ##CCOD            1
     C                   PARM      *BLANKS       ##CRVW            1
     C                   PARM      *BLANKS       ##PROM            1
     C                   PARM      *BLANKS       ##MCRV            1
     C                   PARM      *BLANKS       ##FREQ            1
     C                   PARM      *BLANKS       ##CRAC            1
     C                   PARM      *BLANKS       ##PREO            1
24   C                   PARM                    ##PARN            1
25   C                   PARM                    ##MGRI            1
26   C                   PARM                    ##KCIF            1
27   C                   PARM                    ##CURS            1
28   C                   PARM                    @@PCIB            1
29   C                   PARM                    ##CLT#            3
30   C                   PARM                    ##UCPF            1
31   C                   PARM                    ##DCF             1
32   C                   PARM                    ##AUTB            1
33   C                   PARM                    ##DEIF            1
34   C                   PARM                    ##AURE            1
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     @@RRN3        CHAIN     PCA01                              99
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C                   Z-ADD     EORCN         XXRCN
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANKS       ##L1             78
     C     SCL2HD        PARM      *BLANKS       ##L2             78
     C*
     C                   CALL      'FA707R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C     SCL3HD        PARM      *BLANKS       ##L1             78
     C*********  SCL4HD    PARM *BLANKS   ##L2   78        DEL 08/03/98
     C*
     C                   MOVEL(P)  EONTWK        SCNTWK
     C                   MOVE      EOEOB#        SCEOB#
     C                   Z-ADD     CALIN#        SCLIN#
     C                   Z-ADD     CADSEQ        SCDSEQ
     C                   SETOFF                                       16
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     XXCLT#        IFEQ      '114'
     C     EOCRNE        ANDEQ     'Y'
     C     *in40         ANDEQ     *on
     C*
     C     XXCLT#        OREQ      '114'
     C     EOCRDO        ANDEQ     'Y'
     C     *in40         ANDEQ     *on
     C*
     C*
     C*** check for active
     C*
     C                   MOVE      'N'           FNDEB1            1
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DEKY1         CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     DEESRC        IFEQ      'K'
     C                   MOVE      'Y'           FNDEB1
     C                   LEAVE
     C                   ENDIF
     C*
     C     DEKY1         READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C                   IF        FNDEB1 = 'Y'
     C*
     C*
     C*
     C*
     C*****              EVAL      SCCLMCK = '1'
     C*****              EVAL      SCADJST = *BLANKS
     C                   EVAL      SCADJST = '1'
     C                   EVAL      SCCLMCK = *BLANKS
     C*
     C*
     C                   EXFMT     WDW01
     C*
     C                   IF        SCCLMCK = '1'
     C                   EVAL      EOAST = 'CK'
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
      /Free
                         If (isModEnabled('835 ASC X12 Pay Adv')) and
                             @@Mode <> 'A';
                            is835PayAd = *on;
                         Endif;
                         Exsr ovrStr;
      /End-free
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      UPDATR
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     @@MODE        IFNE      'S'
     C                   EXSR      MVSCDB
     C                   ELSE
     C                   MOVEL(P)  'I'           DESTAT
     C                   MOVEL(P)  'N'           DEPRNT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1PGM         DEPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   MOVEL     DSTIME        DETIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C******************** Z-ADDCADCS#    KKEOB#
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     Q1PRMS        IFGE      7
     C     @@DSEQ        IFGT      *ZEROS
     C                   Z-ADD     @@DSEQ        KKDSEQ
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     DEKY2         SETGT     PDE01
     C     DEKY1         READPE    PDE01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DESEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVE      'U'           DEESRC
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1USER        DEIUSR
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   MOVEL     SRDTO         DEIDT
     C*
     C                   MOVEL     DSTIME        DETIME
     C                   MOVEL     DSTIME        DEITIM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FDDEP
     C*
     C                   ENDSR
**  (QCMDEX)
DLTOVR FILE(FTEBP) LVL(*)                                                      q
OVRDBF FILE(FTEBP) TOFILE(*LIBL/FTEBP  ) OVRSCOPE(*CALLLVL)                    q
DLTOVR FILE(FTABL) LVL(*)                                                      q
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  ) OVRSCOPE(*CALLLVL)                    q
DLTOVR FILE(FTEBL1) LVL(*)                                                     q
OVRDBF FILE(FTEBL1) TOFILE(*LIBL/FTEBL1  ) OVRSCOPE(*CALLLVL)                  q
DLTOVR FILE(FTEBL2) LVL(*)                                                     q
OVRDBF FILE(FTEBL2) TOFILE(*LIBL/FTEBL2  ) OVRSCOPE(*CALLLVL)                  q
