     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W0305R02 - WCMS DARTS DIARY INFORMATION
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays DIARY Category code   **
     H**     Type Code file.                                         **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC305R01 - Work with Remark Type Code Maintenance       **
     H**   CALLS TO:                                                 **
     H**     W2493R - Generic Table Search                           **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   11/06/97      PROJECT: 87MCK                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      41   Summ ref code used
     H**      60   Used for chaining
     H**      71   SCR02 SC40001 Error - Invalid diary catagory (FL02)
     H**      72 - 89 are reserved for error screen error indicators
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      STCTP  (DBRCDF) Remark Type Table                      **
     H**      STSML  (PSM01)  Remark Summary Code Table (SMCD)       **
     H**      WTGTL  (PGT01)  Generic Table File (Table (CO#/ID/KEY) **
     H*****************************************************************
     H*
     H*
     FW0305F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FSTCTP     UF   E             DISK
     F                                     RENAME(PCT01:DBRCDF)
     FSTSML     IF   E           K DISK
     FWTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Remark Type Codes')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

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
     C*          PGT01 - Generic Table File
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXID
     C                   KFLD                    XXKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        XXID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*
     C/COPY SCOPYMT01C
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
     C                   DO
     C*
     C*          Validate category code
     C*             71 -  SC40001 Invalid diary catagory (FL02)
     C*          ------------------------------------------------------
     C     SCFL02        IFNE      *BLANK
     C                   MOVEL(P)  'DRCT'        XXID
     C                   MOVEL(P)  SCFL02        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C   60              SETON                                        7190
     C   60              LEAVE                                                  VVV Leave
     C                   ENDIF                                                  FL02 <> Blanks
     C*
     C                   ENDDO                                                  DO Loop
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
     C                   MOVEL     CTSTAT        SCSTAT
     C                   MOVEL     CTRMCD        SCRMCD
     C                   MOVEL     CTDESC        SCDESC
     C                   MOVEL     CTSMCD        SCSMCD
     C     SCSMCD        IFNE      *BLANKS
     C                   SETON                                        41
     C     SCSMCD        CHAIN     PSM01                              60
     C                   MOVEL(P)  SMDESC        SCSMCT
     C                   ENDIF                                                  SMCD <> Blanks
     C                   MOVEL     CTFL02        SCFL02
     C     CTFL02        IFNE      *BLANKS
     C                   MOVEL(P)  'DRCT'        XXID
     C                   MOVEL(P)  CTFL02        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SC02TX
     C                   ENDIF                                                  FL02 <> blanks
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCFL02        CTFL02
     C                   MOVEL(P)  Q1USER        CTUSER
     C                   MOVEL(P)  Q1PGM         CTPGM
     C                   Z-ADD     DSTIME        CTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
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
     C*
     C*          Process Function 10: Search Provider
     C*          ------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C                   DO
     C     HFLD          IFEQ      'SCFL02'
     C                   MOVEL(P)  SCPRDS        QQTL             40
     C                   MOVE      *BLANKS       QQCALL            1
     C                   MOVE      'DRCT'        QQID              4
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'W2493R'
     C                   PARM                    QQTL
     C                   PARM                    QQCALL
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C*                 Return activity code to screen
     C*                ------------------------------------------------
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           OUTFLG
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = 'A'
     C*
     C     QQRTC         IFEQ      'B'
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = 'B'
     C*
     C                   SETON                                        90
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQKEY         SCFL02
     C                   MOVEL(P)  'DRCT'        XXID
     C                   MOVEL(P)  SCFL02        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SC02TX
     C                   LEAVE                                                  VVV LEAVE
     C                   ENDIF                                                  RTC = '*'
     C*
     C                   ENDIF                                                  HFLD = FL02
     C*
     C                   ENDDO                                                   DO Loop
     C                   ENDIF                                                   *INKJ = On
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*          Return function key settings and exit
     C*          ------------------------------------------------------
     C                   MOVE      *INKC         @@KC
     C                   MOVE      *INKL         @@KL
     C                   SETON                                        LR
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
     C                   ENDSR
