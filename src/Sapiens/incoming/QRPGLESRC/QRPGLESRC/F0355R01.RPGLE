     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0355R01 - EOB CODE/NOTES INFORMATION          C:62 06/23/98 *
     H*                                                               *
     H*  CALLED FROM: F0355R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/10/23 C:079 Project: 99/114288                            *
     H*                 Dont cut off words on EOB Code Description    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*  05/04/11 C:191 PROJECT: FS389                                *
     H*                 ADD TWO ADDITIONAL NOTES LINE (3RD AND 4TH)   *
     H*                                                               *
     H*  02/28/05 C:79  Project: MC267 - Base change                  *
     H*                 After adding an eob code or note, after entry *
     H*                 stay in add mode                              *
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
     H*                                                               *
     H*  01/07/02 C:062 Project: FS235                                *
     H*                 POPULATE "PRINT ON EOMB" FIELD.               *
     H*                                                               *
     H*  03/23/01 C:062 Project: FSMNT  BASE ONLY                     *
     H*                 COMPILE ONLY                                  *
     H*                                                               *
     H*  12/11/00 C:062 Project: MCMNT                                *
     H*                 POPULATE CREATED BY FIELDS.                   *
     H*                                                               *
     H*  09/26/00 C:062 Project: MCMNT                                *
     H*                 ADDED TWO ADDITIONAL FILE OVERRIDES.          *
     H*                                                               *
     H*  11/16/99 C:117 Project: MC044   Support Seq# 20282           *
     H*                 After adding an eob code or note, after entry *
     H*                 return to calling program.                    *
     H*                                                               *
     H*  08/03/98  C:94                                               *
     H*            TOOK OUT SERVICING PROVIDER IN API HEADING         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0355F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEEP     UF A E             DISK
     F                                     RENAME(PEE01:DBRCDF)
     FFAEEL     IF   E           K DISK
     FFAEOP     IF   E             DISK
     FFMCML1    IF   E           K DISK
     FFTABL     IF   E           K DISK    USROPN
      /COPY SCOPYMT01D
     D*
     D QCA             S             56    DIM(8) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D*
     D                 DS
     D  Q1CMD                  1     56
     D  Q1ST                  40     41
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Bill EOB Co-
     D                                     des/Notes')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
       //*** Beg Add *** 10/10/23 *************************************
       dcl-pr FormatNote ExtPgm('FA769R');
        decCompany Like(EEco#);
        chrEob Like(EEeobCd);
        chrNote1 Like(EEnot1);
        chrNote2 Like(EEnot2);
        chrNote3 Like(EEnot3);
        chrNote4 Like(EEnot4);
        chrAst Like(EOast);
       End-pr;
       dcl-s decCompany Like(EEco#);
       dcl-s chrEob Like(EEeobCd);
       dcl-s chrNote1 Like(EEnot1);
       dcl-s chrNote2 Like(EEnot2);
       dcl-s chrNote3 Like(EEnot3);
       dcl-s chrNote4 Like(EEnot4);
       dcl-s chrAst Like(EOast);
       //*** End Add *** 10/10/23 *************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
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
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     EEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKSEQ#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EBCO#         KKCO#
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    EESUB#        KKSUB#
     C     *LIKE         DEFINE    EEFND         KKFND
     C     *LIKE         DEFINE    EEFYR         KKFYR
     C     *LIKE         DEFINE    EECASE        KKCASE
     C     *LIKE         DEFINE    EEEOB#        KKEOB#
     C     *LIKE         DEFINE    EESEQ#        KKSEQ#
     C     *LIKE         DEFINE    EESEQ#        XXSEQ#
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
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEOB         ANDNE     *BLANKS
     C*
     C                   EXSR      OVRSTR
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
     C*
     C*
     C                   EXSR      OVREND
     C*
     C                   ENDIF
     C*
     C     SCNOT1        IFEQ      *BLANKS
     C     SCEOB         ANDEQ     *BLANKS
     C     *IN90         ANDEQ     *OFF
     C*
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C     SCEOB         IFNE      *BLANKS
     C*
     C     SCNOT1        IFNE      *BLANKS
     C     SCNOT2        ORNE      *BLANKS
     C                   SETON                                        7390
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCNOT1 NE *BLANKS
     C                   ENDIF                                                  SCEOB NE *BLANKS
     C*
     C  N99SCNOT1        IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  EBDESC        SCNOT1
     C*
     C                   ENDIF
     C  N99SCNOT2        IFEQ      *BLANKS
     C                   MOVEL     EBDSC2        SCNOT2
     C                   ENDIF
     C*
     C  N99SCNOT3        IFEQ      *BLANKS
     C                   MOVEL     EBDSC3        SCNOT3
     C                   ENDIF
     C*
     C  N99SCNOT4        IFEQ      *BLANKS
     C                   MOVEL     EBDSC4        SCNOT4
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C     SCPRNT        ANDNE     'Y'
     C     SCPRNT        ANDNE     'N'
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
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
     C                   EXSR      OVRSTR
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
     C                   EXSR      OVREND
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
     C     *IN40         IFEQ      *OFF
     C*
     C*** Beg Add ****** 07/18/23 ************************************
     C****               MOVEL(P)  EEEOB         SCEOB
     C                   MOVEL(P)  EEEOBCD       SCEOB
     C*** End Add ****** 07/18/23 ************************************
     C*** Beg Chg ****** 10/10/23 ************************************
     C*****              MOVEL(P)  EENOT1        SCNOT1
     C*****              MOVEL(P)  EENOT2        SCNOT2
     C*****              MOVEL     EENOT3        SCNOT3
     C*****              MOVEL     EENOT4        SCNOT4
       decCompany = EEco#;
       chrEob = EEeobCd;
       chrNote1 = EEnot1;
       chrNote2 = EEnot2;
       chrNote3 = EEnot3;
       chrNote4 = EEnot4;
       chrAst   = *blanks;
       Callp FormatNote(decCompany:chrEob:chrNote1:chrNote2:chrNote3:
          chrNote4: chrAst);
       scNot1 = chrNote1;
       scNot2 = chrNote2;
       scNot3 = chrNote3;
       scNot4 = chrNote4;
       //*** End Chg *** 10/10/23 *************************************
     C                   MOVEL(P)  EESTAT        SCSTAT
     C                   MOVEL(P)  EEUSER        SCUSER
     C                   MOVEL(P)  EEPGM         SCPGM
     C                   Z-ADD     EEDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDT
     C                   MOVE(P)   EETIME        SCTIME
     C                   MOVEL(P)  EEPRNT        SCPRNT
     C     SCSTAT        IFEQ      'I'
     C                   SETON                                        16
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ##PREO        SCPRNT
     C                   MOVEL(P)  *BLANKS       SCEOB
     C                   MOVEL(P)  *BLANKS       SCNOT1
     C                   MOVEL(P)  *BLANKS       SCNOT2
     C                   MOVEL     *BLANKS       SCNOT3
     C                   MOVEL     *BLANKS       SCNOT4
     C                   MOVEL(P)  'A'           SCSTAT
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
     C                   Z-ADD     EOCO#         EECO#
     C                   Z-ADD     EOSUB#        EESUB#
     C                   Z-ADD     EOFND         EEFND
     C                   Z-ADD     EOFYR         EEFYR
     C                   Z-ADD     EOCASE        EECASE
     C                   Z-ADD     EOEOB#        EEEOB#
     C                   Z-ADD     EORCN         EERCN
     C                   Z-ADD     XXSEQ#        EESEQ#
     C*
     C                   ENDIF
     C*
     C*** Beg Chg ****** 07/18/23 ************************************
     C***                MOVEL(P)  SCEOB         EEEOB
     C                   MOVEL(P)  SCEOB         EEEOBCD
     C*** Beg Chg ****** 07/18/23 ************************************
     C                   MOVEL(P)  SCNOT1        EENOT1
     C                   MOVEL(P)  SCNOT2        EENOT2
     C                   MOVEL     SCNOT3        EENOT3
     C                   MOVEL     SCNOT4        EENOT4
     C                   MOVEL     SCSTAT        EESTAT
     C                   MOVEL(P)  SCPRNT        EEPRNT
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   MOVEL(P)  CMSS#         EESS#
     C                   Z-ADD     CMACDT        EEACDT
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
     C*** BEGIN ADD *** 09/26/00 **************************************
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*** END   ADD *** 09/26/00 **************************************
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
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
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        QQCMD
     C                   MOVEL     EOAST         QQST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(4)        QQCMD
     C                   MOVEL     EOAST         QQST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(6)        Q1CMD
     C                   MOVEL     EOAST         Q1ST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    Q1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
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
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
     C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
     C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
     C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
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
     C*
     C                   MOVEL(P)  EONTWK        SCNTWK
     C                   MOVE      EOEOB#        SCEOB#
     C                   SETOFF                                       16
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
     C                   MOVEL(P)  'I'           EESTAT
     C                   MOVEL(P)  'N'           EEPRNT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        EEUSER
     C                   MOVEL     Q1PGM         EEPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C*
     C                   MOVEL     DSTIME        EETIME
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
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     EEKY2         SETGT     PEE01
     C     EEKY1         READPE    PEE01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EESEQ#        ADD       1             XXSEQ#
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
     C                   MOVEL     Q1USER        EEUSER
     C                   MOVEL     Q1USER        EEIUSR
     C                   MOVEL     Q1PGM         EEPGM
     C                   MOVEL     Q1PGM         EEIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C*
     C                   MOVEL     SRDTO         EEIDT
     C*
     C                   MOVEL     DSTIME        EETIME
     C                   MOVEL     DSTIME        EEITIM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FAEEP
     C*
     C                   ENDSR
**  (QCMDEX)
DLTOVR FILE(FTBEP)
OVRDBF FILE(FTBEP) TOFILE(*LIBL/FTBEP  )
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FTEBL1)
OVRDBF FILE(FTEBL1) TOFILE(*LIBL/FTEBL1  )
DLTOVR FILE(FTEBL2)
OVRDBF FILE(FTEBL2) TOFILE(*LIBL/FTEBL2  )
