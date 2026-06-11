     H* (C) Copyright Fiserv Insurance Solutions, Inc., 1997 - 2006
     H*
     H/TITLE WA061R - DUPLICATE PAYMENT AUTO DIARY API
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*    THIS PROGRAM WILL CREATE THE AUTO DIARY TO THE SUPERVISOR, *
     H*    IF A "DUPPMT" AUTO CLAIM DIARY EXISTS IN WMADP.            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0080R                                                     *
     H*    W0081R                                                     *
     H*    F8052R                                                     *
     H*    F8062R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*    PROGRAMMER: C:183                                          *
     H*    CREATED:    02/16/07     PROJECT: 114WLY                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWARPP     IF   E             DISK    INFDS(DBFDS)
      *
     FWMADL     IF   E           K DISK
      *
     FWMCML1    IF   E           K DISK
      *
     F*   REMARK TABLE
     FSTCTL1    IF   E           K DISK
     F*
     F*   USERS BY STAF CODE
     FWMUPL1    IF   E           K DISK
     F*
     D*    CLAIM MASTER STAFF CODES.
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
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D@@KEY            S             40    INZ
     D@@COMM           S            256    INZ
     D@@CDAT           S              8    INZ
     D@@DCID           S             12    INZ
     D@@PNAM           S             10    INZ
     D@@SECL           S              3  0 INZ
     D@@USER           S             10    INZ
     D@@PGM            S             10    INZ
     D@@FOLD           S             80    INZ
     D@@DIFL           S              1    INZ
     D@@IPGM           S             10    INZ
     D@@FL01           S              1    INZ
     D@@FL02           S              1    INZ
     D@@RMCD           S              5    INZ
     D@@SEQ#           S              5    INZ
     D@@SBYL           S              1    INZ
     D@@COM2           S             19    INZ
     D@@APID           S              2    INZ
     D@@IDOC           S             12    INZ
     D@@IQUE           S             10    INZ
     D@@IMGF           S              1    INZ
     D@@IPTH           S             10    INZ
     D@@PIDX           S              5  0 INZ
     D@@QPTY           S              1    INZ
     D@@DCTP           S              5    INZ
     D@@CSID           S              2    INZ
      *
     D@@DUPPMT         S             10    INZ('DupPmt')
      *
     DKKRMCD           S                   LIKE(ADRMCD)
     DKKSTAF           S                   LIKE(UPSTAF)
      *
     D*    DARTS KEY FOR CLAIM
     D                 DS
     D  DSKEY                  1     40
     D  RPCO#
     D  RPFND
     D  RPFYR
     D  RPCASE
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RTCD            1
      *
     C     ADLKY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    @@DUPPMT
      *
     C     CMLKY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
      *
      *   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C     RMKEY         KLIST
     C                   KFLD                    KKRMCD
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     @@RRN         CHAIN     WARPP
      *
     C                   IF        %FOUND(WARPP)
      *
     C     ADLKY         CHAIN     WMADL
      *
     C                   IF        %FOUND(WMADL)
      *
     C*    GET REMARK TABLE RECORD
      *
     C                   MOVE(P)   ADRMCD        KKRMCD
     C     RMKEY         CHAIN     PCT01                              60
      *
     C                   IF        %FOUND(STCTL1) AND
     C                             CTSTAT <> 'I'
      *
     C     CMLKY         CHAIN     WMCML1
      *
     C                   IF        %FOUND(WMCML1)
      *
     C                   EVAL      @@KEY = DSKEY
     C                   EVAL      @@COMM = 'Duplicate payment diary' +
     C                             'Payee:' + RPPNAM + 'Payment amount'+
     C                             %CHAR(RPAMT) + 'Dates of services' +
     C                             %EDITC(RPFRDT:'X') + '-' +
     C                             %EDITC(RPTODT:'X')
      *
     C                   EVAL      @@DCID = CTDEPG
      *
     C*
     C                   EVAL      @@PNAM = 'W0070R'
     C                   EVAL      @@SECL = ADSLVL
      *
     C                   IF        CTFL01 = 'Y'
      *
     C                   EXSR      GETDAT
      *
     C                   IF        @@CDAT <> *ZERO AND
     C                             SRERR <> '1'
     C*
     C*     Diary data for diary type remarks
     C*
     C                   EVAL      @@USER = *BLANKS
     C                   IF        ADUSR <> *BLANKS
     C                   EVAL      @@USER = ADUSR
     C                   ELSE
     C                   EXSR      GETUSR
     C                   IF        @@USER = *BLANKS
     C     @@USER        CABEQ     *BLANK        ENDAPI
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   EVAL      @@PGM = Q1PGM
      *
     C                   IF        CTDEPG <> *BLANKS
     C                   EVAL      @@FOLD = @@KEY
     C                   ELSE
     C                   EVAL      @@FOLD = *BLANKS
     C                   ENDIF
      *
     C                   EVAL      @@DIFL = CTFL01
     C                   EVAL      @@IPGM = Q1PGM
      *
     C                   IF        CTDEPG <> *BLANKS
     C                   EVAL      @@FL01 = 'Y'
     C                   ELSE
     C                   EVAL      @@FL01 = 'N'
     C                   ENDIF
      *
     C                   IF        CTDPRG > 0
     C                   EVAL      @@FL02 = 'Y'
     C                   ELSE
     C                   EVAL      @@FL02 = 'N'
     C                   ENDIF
      *
     C                   EVAL      @@RMCD = ADRMCD
     C                   EVAL      @@SEQ# = '00000'
      *
     C                   IF        ADSLVL > 0
     C                   EVAL      @@SBYL = 'Y'
     C                   ELSE
     C                   EVAL      @@SBYL = *BLANK
     C                   ENDIF
      *
     C                   EVAL      @@COM2 = *BLANKS
     C                   EVAL      @@APID = *BLANKS
     C                   EVAL      @@IDOC = *BLANKS
     C                   EVAL      @@IQUE = *BLANKS
     C                   EVAL      @@IMGF = *BLANKS
     C                   EVAL      @@IPTH = *BLANKS
     C                   EVAL      @@PIDX = *ZEROS
     C                   EVAL      @@QPTY = *BLANKS
     C                   EVAL      @@DCTP = *BLANKS
     C                   EVAL      @@CSID = *BLANKS
      *
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
     C                   PARM                    @@COM2
     C                   PARM                    @@APID
     C                   PARM                    @@IDOC
     C                   PARM                    @@IQUE
     C                   PARM                    @@IMGF
     C                   PARM                    @@IPTH
     C                   PARM                    @@PIDX
     C                   PARM                    @@QPTY
     C                   PARM                    @@DCTP
     C                   PARM                    @@CSID
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     ENDAPI        TAG
     C*
     C                   EVAL      *INLR = *ON
     C*================================================================
     C*    GETUSR - Determine user for remark/diary
     C*================================================================
     C     GETUSR        BEGSR
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
     C*
     C*=====================================================================
     C*    Calculate CDAT for remark/diary
     C*=====================================================================
      *
     C     GETDAT        BEGSR
      *
     C                   SELECT
      *
     C                   WHEN      ADDIDT = '1'
     C                   MOVE      *DATE         SRDATE            8 0
      *
     C                   WHEN      ADDIDT = '2'
     C                   Z-ADD     CMRCVD        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
      *
     C                   WHEN      ADDIDT = '3'
     C                   Z-ADD     CMACDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
      *
     C                   WHEN      ADDIDT = '4'
     C                   Z-ADD     CMDTRP        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C                   ENDSL
     C*
     C                   Z-ADD     ADDIDY        SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C                   MOVEL     SRDATE        @@CDAT
      *
     C                   ENDSR
