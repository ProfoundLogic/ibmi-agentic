     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
     H*****************************************************************
     H/TITLE W0578R - Address History Information
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY THE ADDRESS HISTORY  *
     H*                INFORMATION.                                  .*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0578R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/29/07  PROGRAMMER: C:183 PROJECT: 114WOC         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*****************************************************************
     H*
     FW0578F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDAHP     UF A E             DISK
     F                                     RENAME(PAH01:DBRCDF)
      *
     FWDAHL3    IF   E           K DISK    PREFIX('X_')
     F                                     RENAME(PAH01:PAH02)
     FWTGTL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Claimant Demographic Audit')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('SA020R')
     D #OFSCR          C                   CONST(1)
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C/COPY SCOPYMT01C
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     dtYYMD        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtMDYY1       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      *ZEROS        ##DEC             3 0
     C                   PARM      *BLANK        ##THOU            1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*   KEY LIST FOR THE FUND
     C     FNKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C*
     C     AHKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHFYR
     C                   KFLD                    AHCASE
     C                   KFLD                    AHDT
     C                   KFLD                    AHTIME
     C                   KFLD                    @AHBAFL           1
      *
      *  SYSTEM TABLE VALUE FILE
      *
     C*    GTKY          KLIST
     C*                  KFLD                    CTCO#
     C*                  KFLD                    XXTBID
     C*                  KFLD                    XXKEY
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C  N99              IF        AHBAFL = 'A'
     C                   MOVE      AHFND         SCFND
     C                   MOVE      AHFYR         SCFYR
     C                   MOVE      AHCASE        SCCASE
     C                   MOVEL(P)  AHLSN         SCALSN
     C                   MOVEL(P)  AHFSN         SCAFSN
     C                   MOVEL(P)  AHMID         SCAMID
     C*
     C     20            SUBST     AHADR1:1      SCAAD11
     C     20            SUBST     AHADR1:21     SCAAD12
     C*                  MOVEL(P)  AHADR2        SCAAL2
     C     20            SUBST     AHADR2:1      SCAAD21
     C     20            SUBST     AHADR2:21     SCAAD22
     C                   MOVE      AHCTY         SCACTY
     C                   MOVE      AHST          SCAST
     C                   MOVE      AHZIP         SCAZIP
     C                   MOVE      AHCTRY        SCACTRY
     C*                  MOVEL(P)  AHADR3        SCAAD3
     C     20            SUBST     AHADR3:1      SCAAD31
     C     20            SUBST     AHADR3:21     SCAAD32
     C                   MOVEL(P)  AHUSER        SCUSR
     C                   MOVEL(P)  AHTIME        SCTIME
     C                   Z-ADD     AHDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY1
     C                   MOVEL(P)  SRDTO         SCDT
     C                   EVAL      SCACKAL = AHCKAL
     C*
     C                   EVAL      @AHBAFL = 'B'
     C     AHKY          CHAIN     WDAHL3                             32
      *
     C                   IF        *IN32 = *OFF
     C                   MOVEL(P)  X_AHLSN       SCBLSN
     C                   MOVEL(P)  X_AHFSN       SCBFSN
     C                   MOVEL(P)  X_AHMID       SCBMID
     C*
     C     20            SUBST     X_AHADR1:1    SCBAD11
     C     20            SUBST     X_AHADR1:21   SCBAD12
      *
     C     20            SUBST     X_AHADR2:1    SCBAD21
     C     20            SUBST     X_AHADR2:21   SCBAD22
     C                   MOVE      X_AHCTY       SCBCTY
     C                   MOVE      X_AHST        SCBST
     C                   MOVE      X_AHZIP       SCBZIP
     C                   MOVE      X_AHCTRY      SCBCTRY
      *
     C     20            SUBST     X_AHADR3:1    SCBAD31
     C     20            SUBST     X_AHADR3:21   SCBAD32
     C                   EVAL      SCBCKAL = X_AHCKAL
     C                   ENDIF
      *
     C                   ELSE
     C*
     C  N99              IF        AHBAFL = 'B'
     C*
     C                   MOVE      AHFND         SCFND
     C                   MOVE      AHFYR         SCFYR
     C                   MOVE      AHCASE        SCCASE
     C                   MOVEL(P)  AHLSN         SCBLSN
     C                   MOVEL(P)  AHFSN         SCBFSN
     C                   MOVE      AHMID         SCBMID
     C     20            SUBST     AHADR1:1      SCBAD11
     C     20            SUBST     AHADR1:21     SCBAD12
     C     20            SUBST     AHADR2:1      SCBAD21
     C     20            SUBST     AHADR2:21     SCBAD22
     C                   MOVEL(P)  AHCTY         SCBCTY
     C                   MOVE      AHST          SCBST
     C                   MOVE      AHZIP         SCBZIP
     C                   MOVE      AHCTRY        SCBCTRY
     C     20            SUBST     AHADR3:1      SCBAD31
     C     20            SUBST     AHADR3:21     SCBAD32
     C                   MOVEL(P)  AHUSER        SCUSR
     C                   MOVEL(P)  AHTIME        SCTIME
     C                   Z-ADD     AHDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY1
     C                   MOVEL(P)  SRDTO         SCDT
     C                   EVAL      SCBCKAL = AHCKAL
      *
     C                   EVAL      @AHBAFL = 'A'
     C     AHKY          CHAIN     WDAHL3                             32
      *
     C                   IF        *IN32 = *ON
      *
     C                   EVAL      SCALSN = 'Record Added'
     C                   CLEAR                   SCAFSN
     C                   CLEAR                   SCAMID
     C                   CLEAR                   SCAFSN
     C                   CLEAR                   SCAAD11
     C                   CLEAR                   SCAAD12
     C                   CLEAR                   SCAAD21
     C                   CLEAR                   SCAAD22
     C                   CLEAR                   SCACTY
     C                   CLEAR                   SCAST
     C                   CLEAR                   SCAZIP
     C                   CLEAR                   SCACTRY
     C                   CLEAR                   SCAAD31
     C                   CLEAR                   SCAAD32
     C                   CLEAR                   SCACKAL
      *
     C                   ELSE
      *
     C                   MOVEL(P)  X_AHLSN       SCALSN
     C                   MOVEL(P)  X_AHFSN       SCAFSN
     C                   MOVEL(P)  X_AHMID       SCAMID
      *
     C     20            SUBST     X_AHADR1:1    SCAAD11
     C     20            SUBST     X_AHADR1:21   SCAAD12
     C*                  MOVEL(P)  X_AHADR2      SCAAL2
     C     20            SUBST     X_AHADR2:1    SCAAD21
     C     20            SUBST     X_AHADR2:21   SCAAD22
     C                   MOVE      X_AHCTY       SCACTY
     C                   MOVE      X_AHST        SCAST
     C                   MOVE      X_AHZIP       SCAZIP
     C                   MOVE      X_AHCTRY      SCACTRY
     C*                  MOVEL(P)  X_AHADR3      SCAAD3
     C     20            SUBST     X_AHADR3:1    SCAAD31
     C     20            SUBST     X_AHADR3:21   SCAAD32
     C                   MOVE      X_AHCKAL      SCACKAL
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WDAHP
     C                   ENDSR
