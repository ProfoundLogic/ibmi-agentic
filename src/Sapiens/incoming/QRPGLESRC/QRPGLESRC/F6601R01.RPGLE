     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE F6601R01 - Work With NCCI Data Call Submissions
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: F6601R                                          *
     H*                                                               *
     H*  CALLS TO: F6602C                                             *
     H*            W0010R (FUND SEARCH)                               *
     H*                                                               *
     H*  CREATED: 04/21/16  PROGRAMMER: C:308 PROJECT: 114MWK         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FF6601F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMNCP     UF A E             DISK
     F                                     RENAME(PNC01:DBRCDF)
     FFMNCl4    IF   E           k DISK    RENAME(PNC01:PNC04)
      /COPY SCOPYMT01D
     D FNFDB           DS
     D  FNRRN                397    400B 0
     D*
     D                 DS
     D  XXRPMY                 1      5  0
     D  XXCYY                  1      3  0
     D  XXMM                   4      5  0
     D*
     D                 DS
     D  XXTRMY                 1      5  0
     D  XXTCYY                 1      3  0
     D  XXTMM                  4      5  0
     D*
     D                 DS
     D  XXRPMY1                1      7  0
     D  XXCYY1                 1      3  0
     D  XXMM1                  4      5  0
     D  XXDD1                  6      7  0 inz(01)
     D*
     D                 DS
     D  XXTRMY1                1      7  0
     D  XXTCYY1                1      3  0
     D  XXTMM1                 4      5  0
     D  XXDD2                  6      7  0 inz(01)
     D*
     D strCo#          S              3
     D scrpmyC         S              7
     D sctrmyC         S              7
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D XXPPGM          C                   CONST('F6601R01')
      *
     D OPTNM1          C                   CONST('NCCI Data Call Submi-
     D                                     ssion')
      *
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
     C     SSKY          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKFND             3 0
     C                   KFLD                    KKRPMY            5 0
     C*
     C*  Key list for general use (i.e. deleting all records assoc w/an extract)
     C     KEYX          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY            5 0
     C*
     C     KYMNC4        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXRPMY
     C*                  KFLD                    XXTRMY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
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
     C*
B01  C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
     C     SRERR         IFEQ      *ON
     C                   SETON                                        82  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCTRMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCTRMY
     C     SRERR         IFEQ      *ON
     C                   SETON                                        87  90
     C                   ENDIF
     C*
     C                   ENDIF
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXRPMY
     C*
     C                   Z-ADD     SCTRMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXTRMY
     C*
     C     *IN90         IFEQ      *OFF
     C     XXRPMY        ANDGT     XXTRMY
     C                   SETON                                        9089
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     Kymnc4        Setll     PNC04                                  90
     C   90              SETON                                          88
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     XXRPMY        IFNE      XXTRMY
     C                   SELECT
     C                   WHEN      XXMM = 01 AND  XXTMM = 03
     C                   WHEN      XXMM = 04 AND  XXTMM = 06
     C                   WHEN      XXMM = 07 AND  XXTMM = 09
     C                   WHEN      XXMM = 10 AND  XXTMM = 12
     C                   OTHER
     C                   SETON                                        908287
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
E01  C                   ENDIF                                                  *IN90=*OFF
     C*
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
     C* KJDO - Subroutine for F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* KFDO - Process F6=Add
     C*================================================================
     C     KFDO          BEGSR
     C*
     C                   MOVE      *ON           *INKL
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCFND
     C                   CLEAR                   SCRPMY
     C                   CLEAR                   SCTRMY
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     XXRPMY        XXRPMY1
     C                   MOVEL     XXTRMY        XXTRMY1
     C                   eval      strCo#  = %Char(##Co#)
     C                   eval      scrpmyC = %Char(XXRPMY1)
     C                   eval      sctrmyC = %Char(XXTRMY1)
     C*
     C                   Call      'F6601C'
     C                   Parm                    strCo#
     C                   Parm                    scrpmyC
     C                   Parm                    sctrmyC
     C*  Set outflg for return to main screen
     C                   MOVE      *ON           OUTFLG
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
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
     C                   MOVE      *BLANKS       OPTNM            40
     C                   MOVEL(P)  OPTNM1        OPTNM
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C     @@MODE        IFNE      'E'
      *
     C                   MOVE      ##CO#         @@CO#             3 0
     C                   CALL      'W7944C'
     C                   PARM      XXPPGM        @@PGM            10
      *
     C     @@PGM         IFEQ      '*ON'
     C                   RETURN
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   MOVE      ##CO#         KKCO#
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
     C*
     C                   MOVEL     XXRPMY        XXRPMY1
     C                   MOVEL     XXTRMY        XXTRMY1
     C                   eval      strCo#  = %Char(##CO#)
     C                   eval      scrpmyC = %Char(XXRPMY1)
     C                   eval      sctrmyC = %Char(XXTRMY1)
     C                   Call      'F6601C'
     C                   Parm                    strCo#
     C                   Parm                    scrpmyC
     C                   Parm                    sctrmyC
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   ENDSR
