     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F2141R02 - SPECIALTY CODE TABLE UPDATE DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates or displays the specialty code     **
     H**     table.                                                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F2141R - Work with specialty Code Table                 **
     H**   CALLS TO:                                                 **
     H**     None                                                    **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   07/21/97      PROJECT: MC156                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
    H*  12/12/06 C:182  PROJECT TN005
    H*                  IMPLEMENTED EDITS FOR FIELDS SCXREF FOR
    H*                  EDI REQ. FIELD/EDITS.
    H*
     H*  08/06/98 C:110  PROJECT MC145
     H*                  CHANGED SPELLIING SPECIALITY TO SPECIALITY
     H*                  IN PHT SERVICES, LTD
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      General usage                                  **
     H**      87      Error: MCR0355 - Code can not be blanks        **
     H**      88      Error: MCR0900 - Code already exists           **
     H**      89      Error: MCR0355 - Description can not be blanks **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTSPP  (DBRCDF) : Specialty Code Table File            **
     H**      FTABL  (      ) : Logical Over Table Files (CO#/KEY)   **
     H**             (PSP01 ) : Specialty Code Table File            **
     H*****************************************************************
     H*
     H*
     FF2335F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTSPP     UF A E             DISK
     F                                     RENAME(PSP01:DBRCDF)
     F*----------------------------------------------------------------
     F*           Specialty code table file: used to check dup
     F*
     FFTABL     IF   E           K DISK
    F*** BEG ADD ***** 12/12/06 **************************************
     F*
     FFTGTL     IF   E           K DISK
     F*
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*
    F*** END ADD ***** 12/12/06 **************************************
     F*----------------------------------------------------------------
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
      *** BEG CHG *** 08/06/98 ****************************************
     D OPTNM           C                   CONST('Specialty Code Tabl-
     D                                     e')
      *** END CHG *** 08/06/98 ****************************************
     D COMAPI          C                   CONST('FA007R')
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
     C*----------------------------------------------------------------
     C*       Specialty Code Table file
     C*
     C     SPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*----------------------------------------------------------------
    C*** BEG ADD ***** 12/12/06 **************************************
     C*       Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    QQID
     C                   KFLD                    QQKEY
     C*----------------------------------------------------------------
    C*** BEG ADD ***** 12/12/06 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*----------------------------------------------------------------
     C*       Field defines
     C*
     C     *LIKE         DEFINE    SPKEY         KKKEY
    C*** BEG ADD ***** 12/12/06 **************************************
     C     *LIKE         DEFINE    GTTBID        QQID
     C     *LIKE         DEFINE    GTKEY         QQKEY
    C*** END ADD ***** 12/12/06 **************************************
     C*----------------------------------------------------------------
     C*
     C/COPY SCOPYMT01C
     C*----------------------------------------------------------------
     C*================================================================
     C*  CGHSTS Status - User added subroutine
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C     SPSTAT        IFEQ      'I'
     C                   MOVE      'A'           SPSTAT
     C                   ELSE                                                   STAT = 'I'
     C                   MOVE      'I'           SPSTAT
     C                   END                                                    STAT = 'I'
     C                   MOVEL(P)  Q1USER        SPUSER
     C                   MOVEL(P)  Q1PGM         SPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVELSRDTO     XXDTO   7
     C*** END   DEL *** 12/02/97 **************************************
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         SPDT
     C*** END   CHG *** 12/02/97 **************************************
     C                   Z-ADD     DSTIME        SPTIME
     C                   UPDATE    DBRCDF
     C                   END                                                    *IN60 = *Off
     C                   ENDSR                                                  CHGSTS
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*          If add: check for blank or duplicate code
     C*
     C     *IN40         IFEQ      *ON
    C*** BEG ADD ***** 12/12/06 **************************************
     C     *IN41         OREQ      *ON
     C     *INKJ         ANDNE     *ON
     C*          *IN84     ANDNE*ON
    C*** END ADD ***** 12/12/06 **************************************
     C     SCSPEC        IFEQ      *BLANKS
     C                   SETON                                        8790
     C                   ELSE                                                   SPEC = blanks
    C*** BEG ADD ***** 12/12/06 **************************************
     C     @@MODE        IFEQ      'A'
    C*** END ADD ***** 12/12/06 **************************************
     C                   MOVEL(P)  SCSPEC        KKKEY
     C     SPKY          CHAIN     PSP01                              60
     C  N60              SETON                                        8890
    C*** BEG ADD ***** 12/12/06 **************************************
     C                   END                                                    @@MODE = 'A'
    C*** END ADD ***** 12/12/06 **************************************
     C                   END                                                    SPEC = blanks
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        8990
     C                   END                                                    DESC = blanks
    C*** BEG ADD ***** 12/12/06 **************************************
    C*
    C* CROSS REFERENCE EDTIS
    C*
     C     SCSPEC        IFNE      *BLANKS
     C     *IN90         ANDNE     '1'
     C     SCXREF        IFEQ      *BLANKS
     C                   SETON                                        8590
     C                   ELSE                                                   XREF = blanks
     C                   SETOFF                                       60
     C                   MOVEL(P)  'SCXR'        QQID
     C                   MOVEL(P)  SCXREF        QQKEY
     C     GTKY          CHAIN     PGT01                              60
     C   60              SETON                                        8690
     C                   END                                                    SCXR = blanks
     C                   END                                                    SPEC = blanks
    C*** END ADD ***** 12/12/06 **************************************
     C                   END                                                    *IN40 = On
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*          Move data base fields to screen
     C*
     C                   MOVEL(P)  SPKEY         SCSPEC
     C                   MOVEL(P)  SPDESC        SCDESC
     C                   MOVEL(P)  SPSTAT        SCSTAT
     C   40              MOVE      'A'           SCSTAT
    C*** BEG ADD ***** 12/12/06 **************************************
     C                   MOVEL     SPXREF        SCXREF
    C*** END ADD ***** 12/12/06 **************************************
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*----------------------------------------------------------------
     C*          Move screen fields to data base
     C*
     C   40              MOVEL     ##CO#         SPCO#
     C   40              MOVEL(P)  SCSPEC        SPKEY
     C                   MOVEL(P)  SCDESC        SPDESC
     C                   MOVEL(P)  SCSTAT        SPSTAT
     C                   MOVEL(P)  Q1USER        SPUSER
     C                   MOVEL(P)  Q1PGM         SPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVELSRDTO     XXDTO
     C*** END   DEL *** 12/02/97 **************************************
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         SPDT
     C*** END   CHG *** 12/02/97 **************************************
     C                   Z-ADD     DSTIME        SPTIME
    C*** BEG ADD ***** 12/12/06 **************************************
     C                   MOVEL     SCXREF        SPXREF
    C*** END ADD ***** 12/12/06 **************************************
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*          Process Function 20: Change status
     C*
     C     *INKU         IFEQ      *ON
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      'A'           SCSTAT
     C                   ELSE                                                   STAT = 'I'
     C                   MOVE      'I'           SCSTAT
     C                   ENDIF                                                  STAT = 'I'
     C                   ENDIF                                                  *INKU = On
     C*----------------------------------------------------------------
    C*** BEG ADD ***** 12/12/06 **************************************
    C*
    C*******************
    C* F10=SEARCH      *
    C*******************
    C*
     C     *INKJ         IFEQ      '1'
    C*
    C*================================================================
    C* SCR01
    C*================================================================
    C*
    C*************************
    C*   CROSS REFERENCE CODE
    C*************************
    C*
     C     HFLD          IFEQ      'SCXREF'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'SCXR'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCXREF
     C*******************  MOVELGTDESC    SCCTD
     C                   SETON                                        9024
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        902361
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCSPEC'
     C                   SETON                                        8390
     C     HFLD          WHENEQ    'SCDESC'
     C                   SETON                                        8490
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   ENDIF                                                  INKJ IFEQ
     C*
    C*** END ADD ***** 12/12/06 **************************************
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
     C*----------------------------------------------------------------
     C*          If add: set on indicator 40
     C*
     C     @@MODE        COMP      'A'                                    40
    C*** BEG ADD ***** 12/12/06 **************************************
     C********   @@MODE    COMP 'C'                      40
     C     @@MODE        COMP      'C'                                    41
     C                   SETOFF                                           61
    C*** END ADD ***** 12/12/06 **************************************
     C*
     C*          If change status: flip status and return
     C*
     C     @@MODE        IFEQ      'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDIF                                                  MODE = 'S'
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
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
     C                   FEOD      FTSPP
     C                   ENDSR
