     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE WA700R - API TO WRITE AUTO-DARTS REMARKS
     H*****************************************************************
     H*  Description:
     H*
     H*  This API will write Auto DARTS remarks for a claim.
     H* *****************************************************************
     H*  Called From:                                                   *
     H*    VARIOUS PROGRAMS                                             *
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*  Calls To:                                                      *
     H*    DA700R - Write DARTS API                                     *
     H*                                                                 *
     H* *****************************************************************
     H*  Created:   2/15/05  C:58  Project:  WC1128
     H* ****************************************************************
     H** REVISIONS:
     H*
     H* ****************************************************************
     H*  Indicators:
     H*
     H*    51 - CHAIN/Read Auto-DARTS Control File      WMADL
     H*    52 - CHAIN User file                         WMUPL1
     H*    53 - CHAIN/Read DARTS remarks                SDCMLU
     H*
     H*    55 - This remark not selected: skip DARTS generation.
     H*
     H*    60 - General File Access
     H* ****************************************************************
     F*
     F* ****************************************************************
     F*      F I L E S
     F* ****************************************************************
     H*   Auto DARTS Control File - WMADL
     FWMADL     IF   E           K DISK
     F*
     H*   Claim Master File  - WMCML1                              *
     FWMCML1    IF   E           K DISK
     F*
     F*   DARTS remarks
     FSDCMLU    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*
     F*   Remark table
     FSTCTL1    IF   E           K DISK
     F*
     F*   Users by staf code.
     FWMUPL1    IF   E           K DISK
     F*

     D* -----------------------------------------------------------
     D*    A r r a y s
     D* -----------------------------------------------------------
     D*============================================================
     D*    D A T A
     D*============================================================
     D*    DARTS key for claim.
     D                 DS
     D  DSKEY                  1     40
     D  CMCO#                  1      3  0
     D  CMFND                  4      6  0
     D  CMFYR                  7      9  0
     D  CMCASE                10     16  0
     D*
     D*    Claim master staff codes.
     D                 DS
     D  ST                     1     24
     D                                     DIM(8)                               Staf codes DIUS/AU
     D  CMCLEX                 1      3
     D  CMSUPR                 4      6
     D  CMAMAN                 7      9
     D  CMUMAN                10     12
     D  CMNRSE                13     15
     D  CMFEAD                16     18
     D  CMSDFA                19     21
     D  CMESUP                22     24
     D*
     D*    Claim master claim indicators.
     D                 DS
     D  CI                     1      8
     D                                     DIM(8)                               Claim indic. codes
     D  CMDNY                  1      1
     D  CMCVFL                 2      2
     D  CMSUBR                 3      3
     D  CMEXCS                 4      4
     D  CMACQR                 5      5
     D  CMOSHA                 6      6
     D  CMSDF                  7      7
     D  CMUSLH                 8      8
     D*
     D*    System date and time.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*============================================================
     C*    M A I N      L O G I C
     C*============================================================
     C*    Set up keys, access control and table files
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     ##FND         KKFND
     C                   Z-ADD     ##FYR         KKFYR
     C                   Z-ADD     ##CASE        KKCASE
     C                   MOVEL(P)  ##KEY         KKKEY
     C*
     C*    Get claim master.
     C     CLMKY         CHAIN     PCM01                              60
     C     *IN60         CABEQ     *ON           ENDAP
     C*
     C*    Read Auto DARTS Control records.
     C     ACKEY         CHAIN     PAD01                              51
     C     *IN51         CABEQ     *ON           ENDAP
     C*
     C*    Process all with this key.
     C     *IN51         DOWEQ     *OFF
     C     ADSTAT        CABEQ     'I'           ENDRD
     C*
     C*    Get Remark Table record.
     C                   MOVE(P)   ADRMCD        KKRMCD
     C     RMKEY         CHAIN     PCT01                              60
     C     *IN60         CABEQ     *ON           ENDRD
     C     CTSTAT        CABEQ     'I'           ENDRD
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*      SELECT AND PROCESS
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*    Selection
     C                   EXSR      CLSEL
     C     *IN55         CABEQ     *ON           ENDRD
     C*
     C*    Execute DARTS API
     C                   EXSR      DRAPI
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*    Next record for this key
     C     ENDRD         TAG
     C     ACKEY         READE     PAD01                                  51
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   ENDDO                                                  e DOW IN51 OFF
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*    Return to caller.
     C     ENDAP         TAG
     C                   RETURN
     C*
     C*================================================================
     C*    CLSEL - Claim selection screening.
     C*================================================================
     C     CLSEL         BEGSR
     C                   MOVE      *OFF          *IN55                           Selected
     C*
     C*          Lost time.
     C*          ------------------------------------------------------
     C     ADLTFL        IFEQ      'N'
     C     CMLTFL        ANDNE     'R'
     C     CMLTFL        ANDNE     'M'
     C     ADLTFL        ORNE      ' '
     C     ADLTFL        ANDNE     CMLTFL
     C                   MOVE      *ON           *IN55
     C                   GOTO      ENDSEL
     C                   ENDIF                                                  e LTFL = ...
     C*
     C*          Claim status.
     C*          ------------------------------------------------------
     C     ADCSTS        IFNE      *BLANK
     C     ADCSTS        CABNE     CMSTAT        ENDSEL               5555
     C                   ENDIF                                                  e CSTS <> STAT
     C*
     C*          Claim indicator.
     C*          ------------------------------------------------------
     C     ADCIND        IFNE      ' '
     C                   MOVE      ADCIND        I
     C     I             CABLE     0             ENDSEL                 5555       invalid
     C     I             CABGT     8             ENDSEL               55           invalid
     C     CI(I)         CABNE     'Y'           ENDSEL               5555       Indic not on
     C                   ENDIF                                                  e CIND <> ' '
     C*
     C*          Ocurrence code.
     C*          ------------------------------------------------------
     C     ADOCUR        IFNE      ' '
     C     ADOCUR        ANDNE     '3'
     C     DARKY         CHAIN     PCM02                              53
     C     *IN53         CABEQ     *ON           ENDSEL                          Nonesuch
     C*                                                     Accept(No IN55)
     C*     Read all this claim/remark code.
     C     *IN53         DOWEQ     *OFF                                           Check all
     C     ADOCUR        CABEQ     '1'           ENDSEL                   55
     C     ADOCUR        IFEQ      '2'
     C     CMDIFL        ANDEQ     'Y'
     C     CMDCDT        CABEQ     0             ENDSEL                   55
     C                   ENDIF                                                  e OCUR=2
     C     DARKY         READE     PCM02                                  53
     C                   ENDDO                                                  e DOW 53 OFF
     C*
     C                   ENDIF                                                  e OCUR <> ' ',3
     C*
     C     ENDSEL        TAG
     C                   ENDSR
     C*================================================================
     C*    DRAPI - CALL DARTS API DA700R
     C*================================================================
     C     DRAPI         BEGSR
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C*
     C                   MOVEL(P)  DSKEY         @@KEY            40              1
     C                   MOVEL(P)  ##COMM        @@COMM          256              2
     C     @@COMM        IFEQ      *BLANKS
     C                   MOVEL(P)  CTDESC        @@COMM
     C                   ENDIF                                                  e COMM = ' '
     C*          ------------------------------------------------------
     C*     Diary data for diary type remarks
     C     CTFL01        IFEQ      'Y'
     C                   EXSR      GETDAT
     C     @@CDAT        CABEQ     *ZERO         ENDAPI
     C     SRERR         CABEQ     '1'           ENDAPI
     C*          ------------------------------------------------------
     C                   MOVEL(P)  *BLANKS       @@USER           10              7
     C     ADUSR         IFNE      *BLANKS
     C                   MOVEL(P)  ADUSR         @@USER                           7
     C                   ELSE                                                   x ADUSR<> ' '
     C                   EXSR      GETUSR
     C     @@USER        CABEQ     *BLANK        ENDAPI
     C                   ENDIF                                                  e ADUSR<> ' '
     C                   ENDIF                                                  e FL01=Y
     C*
     C                   MOVEL(P)  CTDEPG        @@DCID           12              4
     C                   MOVEL(P)  'W0070R'      @@PNAM           10              5
     C                   MOVE      ADSLVL        @@SECL            3              6
     C                   MOVEL(P)  Q1PGM         @@PGM            10              8
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  DSKEY         @@FOLD           80              9
     C                   ELSE                                                   x DEPG NE ' '
     C                   MOVEL(P)  *BLANKS       @@FOLD
     C                   ENDIF                                                  e DEPG NE ' '
     C                   MOVEL(P)  CTFL01        @@DIFL            1             10
     C                   MOVEL(P)  Q1PGM         @@IPGM           10             11
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           @@FL01            1             12
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE      'N'           @@FL01                          12
     C                   ENDIF                                                  DCID <> blanks
     C     CTDPRG        IFGT      0
     C                   MOVE(P)   'Y'           @@FL02            1             13
     C                   ELSE                                                   DPRG > 0
     C                   MOVE      'N'           @@FL02                          13
     C                   ENDIF                                                  DPRG > 0
     C                   MOVEL(P)  ADRMCD        @@RMCD            5             14
     C                   MOVE      '00000'       @@SEQ#            5             15
     C     ADSLVL        IFGT      0
     C                   MOVEL(P)  'Y'           @@SBYL            1             16
     C                   ELSE                                                   X-SLVL > 0
     C                   MOVE(P)   *BLANKS       @@SBYL                          16
     C                   ENDIF                                                  SLVL <= 0
     C*          ------------------------------------------------------
     C                   CALL      'DA700R'
     C                   PARM                    @@KEY
     C                   PARM                    @@COMM
     C                   PARM                    @@CDAT
     C                   PARM                    @@DCID
     C                   PARM                    @@PNAM
     C                   PARM                    @@SECL
     C                   PARM                    @@USER
     C                   PARM                    @@PGM
     C                   PARM                    @@FOLD
     C                   PARM                    @@DIFL
     C                   PARM                    @@IPGM
     C                   PARM                    @@FL01
     C                   PARM                    @@FL02
     C                   PARM                    @@RMCD
     C                   PARM                    @@SEQ#
     C                   PARM                    @@SBYL
     C*          ------------------------------------------------------
     C     ENDAPI        TAG
     C                   ENDSR
     C*
     C*================================================================
     C*    GETDAT - Calculate CDAT for remark/diary
     C*================================================================
     C     GETDAT        BEGSR
     C                   SELECT
     C     ADDIDT        WHENEQ    '1'
     C                   MOVE      DSDATE        SRDATE            8 0
     C     ADDIDT        WHENEQ    '2'
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C     ADDIDT        WHENEQ    '3'
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C     ADDIDT        WHENEQ    '4'
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C                   ENDSL
     C*
     C*
     C                   Z-ADD     ADDIDY        SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C                   MOVEL(P)  SRDATE        @@CDAT            8
     C*
     C                   ENDSR
     C*================================================================
     C*    GETUSR - Determine user for remark/diary
     C*================================================================
     C     GETUSR        BEGSR
     C                   MOVE      *OFF          *IN52
     C*
     C*    First try DIUS
     C     ADDIUS        IFGE      '01'                                          numeric ?
     C     ADDIUS        ANDLE     '08'                                          numeric ?
     C                   MOVE      ADDIUS        I                 2 0            index
     C                   MOVE(P)   ST(I)         KKSTAF
     C     KKSTAF        CHAIN     PUP01                              52
     C     *IN52         IFEQ      *OFF
     C                   MOVEL(P)  UPUSR         @@USER
     C                   ENDIF                                                   e 52 OFF
     C                   ENDIF                                                   e DIUS>01
     C*
     C*    OK now try DIAU
     C     @@USER        IFEQ      *BLANK
     C                   MOVE      *OFF          *IN52
     C     ADDIAU        IFGE      '01'                                          numeric ?
     C     ADDIAU        ANDLE     '08'                                          numeric ?
     C                   MOVE      ADDIAU        I                 2 0            index
     C                   MOVE(P)   ST(I)         KKSTAF
     C     KKSTAF        CHAIN     PUP01                              52
     C     *IN52         IFEQ      *OFF
     C                   MOVEL(P)  UPUSR         @@USER
     C                   ENDIF                                                   e 52 OFF
     C                   ENDIF                                                   e DIAU>01 ...
     C                   ENDIF                                                   e USER=' '
     C                   ENDSR
     C*================================================================
     C*    *INZSR - INITIALIZATION SUBROUTINE
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       PARAMETER LISTS
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     *ENTRY        PLIST
     C                   PARM                    ##CO#             3 0          01
     C                   PARM                    ##FND             3 0          02
     C                   PARM                    ##FYR             3 0          03
     C                   PARM                    ##CASE            7 0          04
     C                   PARM                    ##KEY            10            05
     C                   PARM                    ##COMM          256            06
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       K  E  Y      L  I  S  T  S
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C     RMKEY         KLIST
     C                   KFLD                    KKRMCD
     C*
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C     CTKEY         KLIST
     C                   KFLD                    KKRMCD
     C*
     C*   KEY LIST FOR AUTO DARTS CONTROL    WMADL
     C     ACKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKKEY
     C*
     C*   KEY LIST FOR DARTS MASTER - SDCMLU
     C     DARKY         KLIST
     C                   KFLD                    DSKEY
     C                   KFLD                    KKRMCD
     C*
     C*   KEY LIST FOR CLAIM MASTER - WMCML1
     C     CLMKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       DATA DEFINITIONS
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     *LIKE         DEFINE    CMDT          XXDT
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    ADKEY         KKKEY
     C     *LIKE         DEFINE    ADRMCD        KKRMCD
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C*
     C* GET TODAY'S DATE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C* --------------------------------------------------------
     C                   ENDSR
     C*================================================================
