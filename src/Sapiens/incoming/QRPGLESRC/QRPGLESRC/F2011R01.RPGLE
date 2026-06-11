     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2011R01 - T/P COMPONENT INFORMATION
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/08/00 C:79 PROJECT:FSMNT                                  *
     H*                1. USE SCEFDT TO CHECK EFFECTIVE DATE INSTEAD  *
     H*                   OF XXEFDT.                                  *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                CHANGE OPTNM.                                  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF2011F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTUMP     UF A E             DISK
     F                                     RENAME(PUM01:DBRCDF)
     FFMRCL1    IF   E           K DISK
     FFMPRLUC   IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'UC MODIFIER LOGIC'   C         OPTNM
     D OPTNM           C                   CONST('T/P Component Inf-
     D                                     ormation')
     D*** END   CHG *** 05/05/00 **************************************
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C*
     C*** BEG DEL *** 03/17/1999 **************************************
     C***********DATCHK    PLIST
     C***********          PARM           SRDTI   80
     C***********          PARM *BLANKS   SRFFMT  8
     C***********          PARM *BLANKS   SRTFMT  8
     C***********          PARM           SRDTO  10
     C***********          PARM           SRERR   1
     C*
     C***********DTCYMD    PLIST
     C***********          PARM           SRDTI
     C***********          PARM *BLANKS   SRFFMT
     C***********          PARM '*CYMD'   SRTFMT
     C***********          PARM           SRDTO
     C***********          PARM           SRERR
     C*
     C***********DTMDYY    PLIST
     C***********          PARM           SRDTI
     C***********          PARM '*CYMD'   SRFFMT
     C***********          PARM *BLANKS   SRTFMT
     C***********          PARM           SRDTO
     C***********          PARM           SRERR
     C*** END DEL *** 03/17/1999 **************************************
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
     C*  Reasonable & Customary File
     C     RCKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXCPT4
     C*  CPT4 CODE FILE
     C     PRKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXCPT4
     C*** BEG CHG *** 03/17/1999 **************************************
     C***************      KFLD           SCEFDT
     C                   KFLD                    XXEFDT            7 0
     C*** END CHG *** 03/17/1999 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PRCPT4        XXCPT4
     C     *LIKE         DEFINE    PRDSC1        XXDES
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
     C*
     C* CHECK EFFECTIVE DATE NOT EQUAL TO BLANKS AND VALID
     C*
     C*** BEG DEL *** 05/08/00 ****************************************
     C**N90***** SCEFDT    COMP *ZEROS                   80
     C**N90***** SCEFDT    COMP *ZEROS                   90
     C*** END DEL *** 05/08/00 ****************************************
     C*** BEG ADD *** 05/08/00 ****************************************
     C  N90SCEFDT        IFEQ      *ZEROS
     C                   SETON                                        7090
     C                   ENDIF
     C*** END ADD *** 05/08/00 ****************************************
     C*
     C  N90SCEFDT        IFNE      *ZEROS
     C*** BEG CHG *** 05/08/00 ****************************************
     C******************** Z-ADDXXEFDT    SRDTI
     C                   Z-ADD     SCEFDT        SRDTI
     C*** END CHG *** 05/08/00 ****************************************
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEG CHG *** 03/17/1999 **************************************
     C                   MOVEL     SRDTO         SCEFDT
     C******************** MOVELSRDTO     XXEFDT
     C*** END CHG *** 03/17/1999 **************************************
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907080    80, POS CURSOR
     C*** BEG ADD *** 05/08/00 ****************************************
     C                   ELSE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*** END ADD *** 05/08/00 ****************************************
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*BLANK
     C*
     C*
     C* CHECK VALID FROM CPT4 CODE
     C*
     C  N90SCFRCP        IFNE      *BLANKS
     C                   MOVEL     SCFRCP        XXCPT4
     C                   EXSR      CHKCPT
     C     INVALD        IFEQ      'Y'
     C                   SETON                                        907282    82, POS CURSOR
     C                   ELSE
     C                   MOVEL(P)  XXDES         SCDES2
     C                   ENDIF
     C                   ENDIF                                                  END SCFRCP
     C*
     C*
     C* CHECK VALID TO CPT4 CODE
     C*
     C  N90SCTOCP        IFNE      *BLANKS
     C                   MOVEL     SCTOCP        XXCPT4
     C                   EXSR      CHKCPT
     C     INVALD        IFEQ      'Y'
     C                   SETON                                        907383    83, POS CURSOR
     C                   ELSE
     C                   MOVEL(P)  XXDES         SCDES3
     C                   ENDIF
     C                   ENDIF                                                  END SCTOCP
     C*
     C* CHECK PROFESSIONAL COMPONENT EQUAL TO VALID VALUE
     C*
     C  N90SCMD26        COMP      *ZEROS                               84
     C  N90SCMD26        COMP      *ZEROS                               90
     C  N90SCMD26        IFGT      100
     C                   SETON                                        907484
     C                   ENDIF
     C*
     C* CHECK TECHNICAL COMPONENT EQUAL TO VALID VALUE
     C*
     C  N90SCMD27        COMP      *ZEROS                               88
     C  N90SCMD27        COMP      *ZEROS                               90
     C  N90SCMD27        IFGT      100
     C                   SETON                                        907585
     C                   ENDIF
     C*
     C* VERIFY TECH COMP AND PROF COMP EQUAL TO 100%
     C                   Z-ADD     SCMD26        XXMD              9 2
     C                   ADD       SCMD27        XXMD
     C  N90XXMD          IFNE      100
     C                   SETON                                        907684
     C                   ENDIF
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C* GET ONLY ACTIVE RECORDS
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       SCFRCP
     C                   MOVE      *BLANKS       SCTOCP
     C                   MOVE      *ZEROS        SCMD26
     C                   MOVE      *ZEROS        SCMD27
     C                   MOVE      *BLANKS       SCDES2
     C                   MOVE      *BLANKS       SCDES3
     C                   MOVE      *OFF          *IN40
     C*
     C                   ELSE
     C*
     C                   Z-ADD     UMEFDT        SCEFDT
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   MOVE      UMFRCP        SCFRCP
     C                   MOVE      SCFRCP        XXCPT4
     C                   EXSR      CHKCPT
     C                   MOVEL(P)  XXDES         SCDES2
      *
     C                   MOVE      UMTOCP        SCTOCP
     C                   MOVE      SCTOCP        XXCPT4
     C                   EXSR      CHKCPT
     C                   MOVEL(P)  XXDES         SCDES3
      *
     C                   MOVE      UMOD26        SCMD26
     C                   MOVE      UMOD27        SCMD27
     C*
     C                   ENDIF                                                    *IN40
     C*
     C                   TIME                    DFDTTM
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     @@CO#         UMCO#
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   Z-ADD     XXDATE        UMEFDT
     C                   MOVE      SCFRCP        UMCPT4
     C*
     C                   MOVE      SCFRCP        UMFRCP
     C*
     C     SCTOCP        IFEQ      *BLANKS
     C                   MOVE      SCFRCP        UMTOCP
     C                   ELSE
     C                   MOVE      SCTOCP        UMTOCP
     C                   ENDIF                                                  END SCFRCP
     C*
     C                   MOVE      SCMD26        UMOD26
     C                   MOVE      SCMD27        UMOD27
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
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
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
     C*
     C                   MOVEL     Q1USER        UMUSER
     C                   MOVEL     Q1PGM         UMPGM
     C                   MOVEL     DFTIME        UMTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UMDT
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVE      'A'           UMSTAT
     C                   MOVEL     Q1USER        UMIUSR
     C                   MOVEL     Q1PGM         UMIPGM
     C                   MOVEL     DFTIME        UMITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     XXDATE        UMIDT
     C                   WRITE     DBRCDF
     C                   FEOD      FTUMP
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
     C*================================================================
     C* CHKCPT - Verify CPT4 code is valid and retreive description
     C*================================================================
     C     CHKCPT        BEGSR
     C                   MOVEL     'N'           INVALD            1
     C     RCKEY         SETGT     PRC01
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  RCDSC1        XXDES
     C                   ELSE
     C*
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  61
     C     *IN61         IFEQ      '0'
      *
     C                   MOVEL(P)  PRDSC1        XXDES
     C                   ELSE
     C                   MOVE      'Y'           INVALD
     C                   ENDIF                                                  END *IN61
     C                   ENDIF                                                  END *IN60
     C                   ENDSR
