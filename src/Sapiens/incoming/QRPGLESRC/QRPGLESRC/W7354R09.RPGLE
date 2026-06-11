     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7354R09 - UNIT TOTALS INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W7354R                                          *
     H*                                                               *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 10/20/05  PROGRAMMER: C:15  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7354F09  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMSQP     UF A E             DISK
     F                                     RENAME(PSQ01:DBRCDF)
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     F***WMSRP   IF  E                    DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Unit Totals Informat-
     D                                     ion')
     D COMAPI          C                   CONST('WA001R')

     I*
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
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
     C                   Z-ADD     SQCO#         KKCO#
     C                   Z-ADD     SQFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     SQFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     SQRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   Z-ADD     SQEMP#        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C     EMKY          SETLL     PEM01
     C                   READ      PEM01                                  60
     C                   Z-ADD     SQEMP#        SCEMP#
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   Z-ADD     SQFYR         SCFYR
     C                   MOVE      SQSTCD        SCSTCD
     C*
     C                   Z-ADD     SQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      SQCORR        SCCORR
     C                   Z-ADD     SQEPAY        SCEPAY
     C                   Z-ADD     SQEOTH        SCEOTH
     C                   Z-ADD     SQSUBP        SCSUBP
     C                   Z-ADD     SQSTDP        SCSTDP
     C                   Z-ADD     SQ#CLT        SC#CLT
     C                   Z-ADD     SQ#RCD        SC#RCD
     C                   Z-ADD     SQTIND        SCTIND
     C                   Z-ADD     SQTMED        SCTMED
     C                   Z-ADD     SQPDIN        SCPDIN
     C                   Z-ADD     SQPDMD        SCPDMD
     C                   Z-ADD     SQCLAF        SCCLAF
     C                   Z-ADD     SQEMAF        SCEMAF
     C                   Z-ADD     SQTALP        SCTALP
     C                   Z-ADD     SQTALI        SCTALI
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     KKCO#         SQCO#
     C                   Z-ADD     SCFND         SQFND
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         SQRPMY
     C*
     C                   Z-ADD     SCEMP#        SQEMP#
     C                   Z-ADD     SCFYR         SQFYR
     C                   MOVE      SCSTCD        SQSTCD
     C*
     C                   Z-ADD     SQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SQEFDT
     C*
     C                   MOVE      SCCORR        SQCORR
     C*
     C                   Z-ADD     SCEPAY        SQEPAY
     C                   Z-ADD     SCEOTH        SQEOTH
     C                   Z-ADD     SCSUBP        SQSUBP
     C                   Z-ADD     SCSTDP        SQSTDP
     C                   Z-ADD     SC#CLT        SQ#CLT
     C                   Z-ADD     SC#RCD        SQ#RCD
     C                   Z-ADD     SCTIND        SQTIND
     C                   Z-ADD     SCTMED        SQTMED
     C                   Z-ADD     SCPDIN        SQPDIN
     C                   Z-ADD     SCPDMD        SQPDMD
     C                   Z-ADD     SCCLAF        SQCLAF
     C                   Z-ADD     SCEMAF        SQEMAF
     C                   Z-ADD     SCTALP        SQTALP
     C                   Z-ADD     SCTALI        SQTALI
     C*
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
     C*
     C                   Z-ADD     1             #OFSCR            1 0          # OF SCREENS
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C                   ENDIF
     C*
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
     C                   FEOD      WMSQP
     C                   ENDSR
