     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                  Recompile - NY Compliance: CARC/RARC Update  *
     H*                                                               *
     H*  05/29/18 C:180 Support: 71259                                *
     H*                 Fix EOMB code 2 description                   *
     H*                                                               *
     H*  09/14/15 C:79  Support: 146/37044                            *
     H*                 Add 3 description lines                       *
     H*                 Fix EBDSC2                                    *
     H*                                                               *
     H*  04/15/04 C:79  Project: FL046                                *
     H*                 IF FLORIDA REQUIRED 1ST CROSS REFERENCE       *
     H*                                                               *
     H*  07/16/02 C:62  Project: 114MDC                               *
     H*                 CALL CODEREVIEW SPECIFIC PROGRAM.             *
     H*                                                               *
     H*  11/19/01 C:62  Project: MC025 29903                          *
     H*                 -MAKE 1ST X-REF REQUIRED IF STATE IS TEXAS    *
     H*                                                               *
     H*  08/28/00 C:62  Project: MCMNT                                *
     H*                 CHANGE HOW SCDSC2 IS POPULATED.               *
     H*                                                               *
     H*  01/26/00 C:53  Project: KY010                                *
     H*                 ADD 2ND EOMB CODE                             *
     H*                                                               *
     H*****************************************************************
     FF2330F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTEBP     UF A E             DISK
     F                                     RENAME(PEB01:DBRCDF)
     F*** BEGIN ADD *** 07/16/02 **************************************
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 07/16/02 **************************************
     FFTABL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     F*** BEG ADD *** 01/26/00 *********************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFTGIL     IF   E           K DISK
     F*** END ADD *** 01/26/00 *********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*** BEGIN ADD *** 07/16/02 **************************************
     D  DBRRN                397    400B 0
     D*** END   ADD *** 07/16/02 **************************************
     D*** BEGIN ADD *** 08/28/00 **************************************
     D*** Beg Del *** 09/14/15 ****************************************
     D***              DS
     D***  EBDSC2                 1     70
     D***  XXDSC2                 1      3
     D*** End Del *** 09/14/15 ****************************************
     D*** END   ADD *** 08/28/00 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMB Code Mai-
     D                                     ntenance')
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
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEG ADD *** 01/26/00 *********************************
     C     PGMLS         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    XXTBID
     C*** END ADD *** 01/26/00 *********************************
     C*** BEG ADD *** 05/29/18 *********************************
     C     PGMLS2        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    XXTBID2
     C*** END ADD *** 05/29/18 *********************************
     C*** BEGIN ADD *** 07/16/02 **************************************
     C     ACDPL         PLIST
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C                   PARM      @@MODE        ##CALL            1
     C*** END   ADD *** 07/16/02 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
      *
     C*** BEG ADD *** 01/26/00 *********************************
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C*** END ADD *** 01/26/00 *********************************
     C*** BEG ADD *** 05/29/18 *********************************
     C     GIKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID2
      *
     C     GTKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID2
     C                   KFLD                    XXKEY2
     C*** END ADD *** 05/29/18 *********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EBCO#         ##CO#
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    GTTBID        XXTBID                         ADD 01/26/00
     C*** BEG ADD *** 05/29/18 *********************************
     C     *LIKE         DEFINE    GTTBID        XXTBID2                        ADD 01/26/00
     C     *LIKE         DEFINE    GTKEY         XXKEY2                         ADD 01/26/00
     C*** END ADD *** 05/29/18 *********************************
     C     *LIKE         DEFINE    GTKEY         XXKEY                          ADD 01/26/00
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EBSTAT        IFEQ      'I'
     C                   MOVE      'A'           EBSTAT
     C                   ELSE
     C                   MOVE      'I'           EBSTAT
     C                   ENDIF                                                  EBSTAT
     C*
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     DSTIME        EBTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
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
     C*** BEGIN ADD *** 07/16/02 **************************************
     C                   EXSR      NEXTSC
     C*** END   ADD *** 07/16/02 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* EOMB CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE EOMB CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         KKKEY
     C*
     C   40
     CANN90EBKY          CHAIN     PEB01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* EOMB CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
      *** BEG ADD *** 01/06/99 ****************************************
     C*
     C* EOMB XREF CODE MUST BE BLANKS OR VALID
     C*
     C*** BEGIN CHG *** 11/19/01 **************************************
     C*****N90** SCXREF    IFNE *BLANKS
     C     *IN90         IFEQ      *OFF
     C     SCXREF        ANDNE     *BLANKS
     C     *IN90         OREQ      *OFF
     C     *IN16         ANDEQ     *ON
     C*** END   CHG *** 11/19/01 **************************************
     C                   MOVEL     SCXREF        XXKEY
     C*
     C*** BEG CHG *** 01/26/00 **************************************
     C                   MOVEL(P)  XREF          XXTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C**N99 ****           MOVELGTDESC    SCXRED    P
     C** 99 ****           MOVEL*BLANKS   SCXRED    P
     C*
     C********** EBKY      CHAINPEB01                99
     C*** END CHG *** 01/26/00 **************************************
     C   99              SETON                                        74  90
     C                   ELSE
     C     SCST          IFEQ      'KY'
     C                   SETON                                        76  90
     C                   ENDIF
     C                   ENDIF                                                  SCXREF
      *** END ADD *** 01/06/99 ****************************************
     C*** BEG ADD *** 01/26/00 ****************************************
     C*
     C*** Beg Chg *** 09/14/15 ***************************************
     C**N90SCDSC2        IFNE      *BLANKS
     C***                MOVEL     SCDSC2        XXKEY
     C  N90SCXRF2        IFNE      *BLANKS
     C                   MOVEL     SCXRF2        XXKEY
     C*** End Chg *** 09/14/15 ***************************************
     C                   MOVEL(P)  DSC2          XXTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C**N99 ****           MOVELGTDESC    SCDSC2    P
     C** 99 ****           MOVEL*BLANKS   SCDSC2    P
     C   99              SETON                                        75  90
     C                   ELSE
     C     SCST          IFEQ      'KY'
     C                   SETON                                        77  90
     C                   ENDIF
     C                   ENDIF                                                  SCDSC2
     C*** END ADD *** 01/26/00 ****************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90EBDT          IFGT      SAVEDT
     C     EBDT          OREQ      SAVEDT
     C     EBTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10 processing
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   MOVE      '1'           *IN90
     C*
     C     *IN09         IFEQ      *OFF                                         NOT DISP MODE
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCXREF'
     C                   MOVEL(P)  XREF          XXTBID
     C                   CALL      'F2380R'      PGMLS
     C*
     C     ##RRN         IFNE      *ZEROS
     C                   MOVE      '1'           *IN36
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCXREF
     C***************      MOVELGTDESC    SCXRED    P
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   MOVEL     GTDESC        SCXDS1
     C*** END   ADD *** 08/28/00 **************************************
     C                   ELSE
     C                   MOVE      '1'           *IN35
     C                   ENDIF                                                  ##RRN
     C*
     C*** Beg Chg *** 09/14/15 **************************************
     C**** HFLD          WHENEQ    'SCDSC2'
     C     HFLD          WHENEQ    'SCXRF2'
     C*** End Chg *** 09/14/15 **************************************
     C*
     C*** Beg Chg *** 05/29/18 **************************************
     C****               MOVEL(P)  DSC2          XXTBID
     C****               CALL      'F2380R'      PGMLS
     C                   MOVEL(P)  DSC2          XXTBID2
     C                   CALL      'F2380R'      PGMLS2
     C*** End Chg *** 05/29/18 **************************************
     C*
     C     ##RRN         IFNE      *ZEROS
     C*****                MOVE '1'       *IN36
     C     ##RRN         CHAIN     PGT02                              99
     C*** Beg Chg *** 09/14/15 **************************************
     C************       MOVEL(P)  GTKEY         SCDSC2
     C                   MOVEL(P)  GTKEY         SCXRF2
     C*** End Chg *** 09/14/15 **************************************
     C***************      MOVELGTDESC    SCDSCD    P
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   MOVEL     GTDESC        SCXDS2
     C*** END   ADD *** 08/28/00 **************************************
     C                   ELSE
     C                   MOVE      '1'           *IN36
     C                   ENDIF                                                  ##RRN
     C*
     C                   ENDSL
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     EBKEY         SCKEY
     C                   MOVEL     EBDESC        SCDESC
     C*** BEG ADD *** 09/14/15 ****************************************
     C                   MOVEL     EBDSC2        SCDESC2
     C                   MOVEL     EBDSC3        SCDESC3
     C                   MOVEL     EBDSC4        SCDESC4
     C*** END ADD *** 09/14/15 ****************************************
     C                   MOVEL     EBSTAT        SCSTAT
      *** BEG ADD *** 01/06/99 ****************************************
     C                   MOVEL     EBXREF        SCXREF
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   MOVEL(P)  EBXREF        XXKEY
     C                   MOVEL(P)  XREF          XXTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCXDS1
     C   99              MOVEL     *BLANKS       SCXDS1
     C*** END   ADD *** 08/28/00 **************************************
      *** END ADD *** 01/06/99 ****************************************
     C*** BEGIN CHG *** 08/28/00 **************************************
     C**********           MOVELEBDSC2    SCDSC2    P      ADD 01/26/00
     C*** Beg Del *** 09/14/15 ****************************************
     C*****************  MOVEL(P)  XXDSC2        SCDSC2
     C*****************  MOVEL(P)  XXDSC2        XXKEY
     C*****************  MOVEL(P)  DSC2          XXTBID
     C*** End Del *** 09/14/15 ****************************************
     C*** Beg Add *** 09/14/15 ****************************************
     C                   MOVEL(P)  EBXRF2        SCXRF2
     C*** End Add *** 09/14/15 ****************************************
      *** Beg CHG*** 05/29/18 ****************************************
     C***  GTKY          CHAIN     PGT01                              99
     C                   MOVEL(P)  DSC2          XXTBID2
     C                   MOVEL(P)  EBXRF2        XXKEY2
     C     GTKY2         CHAIN     PGT01                              99
      *** End CHG *** 05/29/18 ****************************************
     C*
     C  N99              MOVEL(P)  GTDESC        SCXDS2
     C   99              MOVEL     *BLANKS       SCXDS2
     C*** END   CHG *** 08/28/00 **************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     'A'           SCSTAT
      *** BEG ADD *** 01/06/99 ****************************************
     C                   MOVEL     *BLANKS       SCXREF
      *** END ADD *** 01/06/99 ****************************************
     C*** Beg Chg *** 09/14/15 **************************************
     C****************** MOVE      *BLANKS       SCDSC2                         ADD 01/26/00
     C                   MOVE      *BLANKS       SCXRF2                         ADD 01/26/00
     C*** End Chg *** 09/14/15 **************************************
     C*** BEG ADD *** 09/14/15 ****************************************
     C                   MOVEL     *BLANKS       SCDESC2
     C                   MOVEL     *BLANKS       SCDESC3
     C                   MOVEL     *BLANKS       SCDESC4
     C*** END ADD *** 09/14/15 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         EBCO#
     C                   MOVEL     SCKEY         EBKEY
     C                   MOVEL     SCDESC        EBDESC
     C*** BEG ADD *** 09/14/15 ****************************************
     C                   MOVEL     SCDESC2       EBDSC2
     C                   MOVEL     SCDESC3       EBDSC3
     C                   MOVEL     SCDESC4       EBDSC4
     C*** END ADD *** 09/14/15 ****************************************
     C                   MOVEL     SCSTAT        EBSTAT
      *** BEG ADD *** 01/06/99 ****************************************
     C                   MOVEL     SCXREF        EBXREF
      *** END ADD *** 01/06/99 ****************************************
     C*** BEGIN CHG *** 08/28/00 **************************************
     C**********           MOVELSCDSC2    EBDSC2    P      ADD 01/26/00
     C*** Beg Chg *** 09/14/15 ****************************************
     C***************    MOVEL(P)  SCDSC2        XXDSC2
     C                   MOVEL(P)  SCXRF2        EBXRF2
     C*** End Chg *** 09/14/15 ****************************************
     C*** END   CHG *** 08/28/00 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 07/16/02 **************************************
     C*================================================================
     C* NEXTSC - PROCESS NEXT SCREEN - A/R INFORMATION
     C*================================================================
     C     NEXTSC        BEGSR
     C*
     C     @@ST          IFEQ      'CR'
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVEL     @@MODE        ##MODE
     C                   ELSE
     C                   MOVEL     'C'           ##MODE
     C                   ENDIF
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     DBRRN         ##RRN
     C                   ELSE
     C                   Z-ADD     @@RRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'F2330RCR'    ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   EXSR      GETDTM
     C*
     C     @@MODE        IFEQ      'A'
     C     DBRRN         DELETE    DBRCDF                             99
     C                   MOVEL     *ON           OUTFLG
     C                   ENDIF
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *ON
     C     @@MODE        IFEQ      'A'
     C     DBRRN         DELETE    DBRCDF                             99
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 07/16/02 **************************************
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
      *
      *  Process F10 search
      *
     C     *INKJ         CASEQ     '1'           KJDO
     C                   ENDCS
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C                   EXSR      SETTBL                                       ADD 01/26/00
     C*** BEGIN ADD *** 11/19/01 **************************************
     C                   SETOFF                                       16
     C     @@ST          IFEQ      'TX'
     C*** BEG ADD *** 04/16/04 ****************************************
     C     @@ST          OREQ      'FL'
     C*** END ADD *** 04/16/04 ****************************************
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 11/19/01 **************************************
     C*
     C                   ENDSR                                                  PGMSTR
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
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     DSTIME        EBTIME
     C*
     C                   UPDATE    DBRCDF
     C*** BEGIN ADD *** 07/16/02 **************************************
     C                   EXSR      NEXTSC
     C*** END   ADD *** 07/16/02 **************************************
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C                   MOVEL     Q1USER        EBIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     SRDTO         EBIDT
     C*
     C                   MOVEL     DSTIME        EBTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTEBP
     C*** BEGIN ADD *** 07/16/02 **************************************
     C                   EXSR      NEXTSC
     C*** END   ADD *** 07/16/02 **************************************
     C*                     /
     C                   ENDSR
     C*** BEG ADD *** 01/26/00 *********************************
     C*================================================================
     C* SETTBL - SET THE TABLE ID'S FOR THE STATE SPECIFIC GEN TABLE
     C*================================================================
     C     SETTBL        BEGSR
     C*
     C* DETERMINE THE TBID'S FOR XREF
     C*
     C     @@ST          CAT(P)    'BG':0        XREF              4
     C                   MOVEL(P)  XREF          XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBG'        XREF
     C                   ENDIF
     C* DETERMINE THE TBID'S FOR DSC2
     C*
     C     @@ST          CAT(P)    'RE':0        DSC2              4
      *** BEGIN CHANGE *** 05/29/18 ********************************
     C***                MOVEL(P)  DSC2          XXTBID
     C***  GIKY          CHAIN     PGI01                              99
     C                   MOVEL(P)  DSC2          XXTBID2
     C     GIKY2         CHAIN     PGI01                              99
      *** END   CHANGE *** 05/29/18 ********************************
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZRE'        DSC2
     C                   ENDIF
     C                   ENDSR
     C*** END ADD *** 01/26/00 *********************************
