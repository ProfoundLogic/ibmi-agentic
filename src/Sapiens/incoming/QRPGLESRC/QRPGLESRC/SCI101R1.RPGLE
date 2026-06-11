     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  Created by C: 117  Project: 115AAC  03/30/01
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  05/17/07 C:191  Support: 58389
     H*                  - Update Initial date time when adding recs
     H*
     F**  C:02   8/17/06 - CHANGED TO NOT REQUIRE ARCHIVE FOLDER.  (IMPT WILL
     F**                   NOT COPY FILE TO ARCHIVE IF NOT ENTERED
     H*****************************************************************
     FSCI101F1  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMI1P     UF A E             DISK
     F                                     RENAME(PI101:DBRCDF)
     F*  File for Remark Code Description
     FSTCTL1    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Object Import Proces-
     D                                     s')
     D COMAPI          C                   CONST('SA005R')
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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Key list for Remark Code Description
     C*
     C     CTKY          KLIST
     C                   KFLD                    KKRMCD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    I1RMCD        HLRMCD
     C     *LIKE         DEFINE    I1DOCT        HLDOCT
     C     *LIKE         DEFINE    I1RMCD        KKRMCD
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     I1STAT        IFEQ      'I'
     C                   MOVE      'A'           I1STAT
     C                   ELSE
     C                   MOVE      'I'           I1STAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        I1USER
     C                   MOVEL     Q1PGM         I1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         I1DT
     C                   MOVEL     DSTIME        I1TIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
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
     C*
     C*  SCPTY is a required field
     C*
     C     SCPTY         IFEQ      *ZEROS
     C                   SETON                                        7090
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  SCIDXT is a required field with a 'D' or 'M' type only
     C*
     C     SCIDXT        IFEQ      *BLANKS
     C                   SETON                                        7190
     C                   GOTO      ER1END
     C                   ELSE
     C     SCIDXT        IFNE      'D'
     C     SCIDXT        ANDNE     'M'
     C                   SETON                                        7290
     C                   GOTO      ER1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Valid values for SCIMGF: 'W', 'I', 'F', 'O', or ' '
     C*
     C     SCIMGF        IFNE      'W'
     C     SCIMGF        ANDNE     'I'
     C     SCIMGF        ANDNE     'F'
     C     SCIMGF        ANDNE     'O'
     C     SCIMGF        ANDNE     ' '
     C                   SETON                                        7390
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  Verify the Import Directory name exists
     C*
     C                   MOVEL     *BLANKS       DIRNAM           80
     C                   MOVEL     SCIDIR        DIRNAM
     C                   MOVEL     *BLANKS       RTNCD             2
     C                   CALL      'VFYOBJC'
     C                   PARM                    DIRNAM
     C                   PARM                    RTNCD
     C*
     C     RTNCD         IFNE      *BLANKS
     C                   SETON                                        8790
     C                   GOTO      ER1END
     C                   ENDIF
     C*** 8/17/06 *** ONLY VERIFY ARCHIVE IF IT'S ENTERED ********
     C***  ADD **************************************************
     C     SCADIR        IFNE      *BLANKS
     C*** END ADD 8/17/06 **************************************
     C*
     C*  Verify the Archive Directory name exists
     C*
     C                   MOVEL     *BLANKS       DIRNAM           80
     C                   MOVEL     SCADIR        DIRNAM
     C                   MOVEL     *BLANKS       RTNCD             2
     C                   CALL      'VFYOBJC'
     C                   PARM                    DIRNAM
     C                   PARM                    RTNCD
     C*
     C     RTNCD         IFNE      *BLANKS
     C                   SETON                                        8890
     C                   GOTO      ER1END
     C                   ENDIF
     C***  ADD *** 8/17/06 **************************************
     C                   ENDIF
     C*** END ADD 8/17/06 **************************************
     C*
     C*  Verify the Error Directory name exists
     C*
     C                   MOVEL     *BLANKS       DIRNAM           80
     C                   MOVEL     SCEDIR        DIRNAM
     C                   MOVEL     *BLANKS       RTNCD             2
     C                   CALL      'VFYOBJC'
     C                   PARM                    DIRNAM
     C                   PARM                    RTNCD
     C*
     C     RTNCD         IFNE      *BLANKS
     C                   SETON                                        8990
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  Validate the Import Process pgm
     C*
     C     SCKPGM        IFEQ      *BLANKS
     C                   SETON                                        9190
     C                   GOTO      ER1END
     C                   ELSE
     C                   CALL      'DA500C'
     C                   PARM      SCKPGM        QQOBJ            10
     C                   PARM      '*LIBL'       QQLIB            10
     C                   PARM      '*PGM'        QQTYP             7
     C                   PARM                    QQERR             1
     C*
     C     QQERR         IFEQ      '1'
     C                   SETON                                        9290
     C                   GOTO      ER1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  SCRMCD is required if SCIMGF is NOT I or F
     C*
     C     SCIMGF        IFNE      'I'
     C     SCIMGF        ANDNE     'F'
     C     SCRMCD        ANDEQ     *BLANKS
     C                   SETON                                        8590
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  SCRMCD must be validated
     C*
     C     SCRMCD        IFNE      *BLANKS
     C                   MOVEL     SCRMCD        KKRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CTDESC        SCDESC
     C                   ELSE
     C                   SETON                                        9084
     C                   GOTO      ER1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  SCDOCT is required if SCIMGF is I or F
     C*
     C     SCIMGF        IFEQ      'I'
     C     SCDOCT        ANDEQ     *BLANKS
     C     SCIMGF        OREQ      'F'
     C     SCDOCT        ANDEQ     *BLANKS
     C                   SETON                                        8690
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  Valid values for SCAIDX: 'Y' or ' '
     C*
     C     SCAIDX        IFNE      'Y'
     C     SCAIDX        ANDNE     ' '
     C                   SETON                                        7490
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C     ER1END        ENDSR
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
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* REMARK CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRMCD'
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   CALL      'SC580R'
     C                   PARM                    SCRMCD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   SETON                                        31
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      ' '
     C                   SETON                                        30
     C                   ENDIF
     C*
     C*  DOC TYPE SEARCH
     C*
     C     HFLD          WHENEQ    'SCDOCT'
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'IS100R'
     C                   PARM                    SCDOCT
     C                   PARM                    SCDFTT
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   SETON                                        32
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      ' '
     C                   SETON                                        31
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCPTY'
     C                   SETON                                        75
     C*
     C     HFLD          WHENEQ    'SCIDXT'
     C                   SETON                                        76
     C*
     C     HFLD          WHENEQ    'SCIMGF'
     C                   SETON                                        77
     C*
     C     HFLD          WHENEQ    'SCIDIR'
     C                   SETON                                        78
     C*
     C     HFLD          WHENEQ    'SCADIR'
     C                   SETON                                        79
     C*
     C     HFLD          WHENEQ    'SCEDIR'
     C                   SETON                                        80
     C*
     C     HFLD          WHENEQ    'SCKPGM'
     C                   SETON                                        81
     C*
     C     HFLD          WHENEQ    'SCDFTT'
     C                   SETON                                        82
     C*
     C     HFLD          WHENEQ    'SCAIDX'
     C                   SETON                                        83
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
     C     *IN40         IFEQ      *ON
     C*
     C* CLEAR FIELDS ON AN ADD
     C*
     C                   CLEAR                   SCPTY
     C                   CLEAR                   SCIDXT
     C                   CLEAR                   SCIMGF
     C                   CLEAR                   SCIDIR
     C                   CLEAR                   SCADIR
     C                   CLEAR                   SCEDIR
     C                   CLEAR                   SCKPGM
     C                   CLEAR                   SCRMCD
     C                   CLEAR                   SCDOCT
     C                   CLEAR                   SCDFTT
     C                   CLEAR                   SCAIDX
     C*
     C                   ELSE
     C*
     C                   Z-ADD     I1PTY         SCPTY
     C                   MOVEL     I1IDXT        SCIDXT
     C                   MOVEL     I1IMGF        SCIMGF
     C                   MOVEL     I1IDIR        SCIDIR
     C                   MOVEL     I1ADIR        SCADIR
     C                   MOVEL     I1EDIR        SCEDIR
     C                   MOVEL     I1KPGM        SCKPGM
     C                   MOVEL     I1RMCD        SCRMCD
     C                   MOVEL     I1DOCT        SCDOCT
     C                   MOVEL     I1DFTT        SCDFTT
     C                   MOVEL     I1AIDX        SCAIDX
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCPTY         I1PTY
     C                   MOVEL     SCIDXT        I1IDXT
     C                   MOVEL     SCIMGF        I1IMGF
     C                   MOVEL     SCIDIR        I1IDIR
     C                   MOVEL     SCADIR        I1ADIR
     C                   MOVEL     SCEDIR        I1EDIR
     C                   MOVEL     SCKPGM        I1KPGM
     C                   MOVEL     SCRMCD        I1RMCD
     C                   MOVEL     SCDOCT        I1DOCT
     C                   MOVEL     SCDFTT        I1DFTT
     C                   MOVEL     SCAIDX        I1AIDX
     C                   MOVEL     'A'           I1STAT
     C*
     C                   MOVEL     Q1USER        I1USER
     C                   MOVEL     Q1PGM         I1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         I1DT
     C                   MOVEL     DSTIME        I1TIME
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
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETOFF                                       094041
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
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
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
     C                   MOVEL     Q1USER        I1IUSR
     C*** BEG ADD ****** 05/17/07 **********************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         I1IDT
     C                   MOVEL     DSTIME        I1ITIM
     C*** END ADD ****** 05/17/07 **********************************************
     C                   WRITE     DBRCDF
     C                   FEOD      SMI1P
     C                   ENDSR
