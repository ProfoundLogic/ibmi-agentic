     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: FS900R1
     H*    DESCRIPTION: Convert MDR's UCR Payment File
     H*    CREATED: 02/27/01 C:117 PROJECT: FS225
     H*                            CREATED ANES MODULE
     H*                            MOVED MEDICODE PGMS TO BASE
     H*****************************************************************
     H*    FILE DESCRIPTION:   HIAA Version
     H*       1st Record "10" - containing information relative to the file
     H*                         Note: Record "10" not needed for conversion
     H*       2nd Record "20" - interposed between each geographic zip area
     H*                         and gives the geographic zip area, covered
     H*                         zips and text description
     H*       3rd Record "30" - Procedure Code xref to Percentile
     H*****************************************************************
     H*    PROCESSING DESCRIPTION:
     H*       Record "20" will be written to its own (new) file
     H*             FMGZP (Geographic Zip Area Master File)
     H*       Record "30" will be written to the Reasonable &
     H*             Customary File FMRCP
     H*****************************************************************
     H*
     H*  CALLED FROM: ????
     H*  CALLS TO   : NONE
     H*
     H******************************************************************
     H*  REVISIONS:                                                    *
     H*                                                                *
     H*  08/07/02 C:62 PROJECT FS244                                   *
     H*                ADD EFFECTIVE DATE TO FMGZP PROCESSING          *
     H*
     H*  01/05/99 C:53 Project: 95MAQ
     H*                MRA AMOUNT FIELD TO BE POPULATED WITH 1.00 IF
     H*                THE VALUE BEING LOADED IS LESS THAN 0
     H*
     H*  08/10/98 C:53 Project: MCMNT                                  *
     H*                CORRECT THE READ OF BY REPORT FLAG              *
     H*                                                                *
     H*  04/27/94 C:05 Project: MCMNT                                  *
     H*                Add initialization of new fields                *
     H*                                                                *
     H*  01/31/96 C:45 PROJECT: MC137                                  *
     H*                VALIDATE THE EXISTANCE OF THE NEW RECORDS PRIOR *
     H*                TO BEING WRITTEN. IF THE RECORD EXISTS, BYPASS  *
     H*                AND READ ANOTHER RECORD.                        *
     H*                                                                *
     H*  11/14/96 C:56 PROJECT: MCMNT                                  *
     H*                ADD PERCENTAGE PARM AND MATCH IT TO PRE-DEFINED *
     H*                PERCENT VALUES                                  *
     H*                                                                *
     H*  12/18/96 C:53 PROJECT: MCMNT                                  *
     H*                ADDED OUTPUT - COUNTER AND PERCENTILE LOADED
     H******************************************************************
     H*  INDICATORS:                                                   *
     H*  50    CHAIN ON PRC01                                          *
     H*  51    CHAIN ON PGZ01                                          *
     H*  52    CHAIN ON PGZ02                                          *
     H*  60    READ/DOWEQ ON MDR01                                     *
     H*  99    RESTART                                                 *
     H******************************************************************
     H*  FILES:                                                        *
     H*  MDR    (MDR01): None                                          *
     H*  FMGZP  (PGZ01): CO#/ZIPA                                      *
     H*  FMGZL1 (PGZ02): CO#/ZIPA/FRZP                                 *
     H*  FMRCP  (PRC01): CO#/ZIPA/CPT4/EFDT                            *
     H******************************************************************
     FQTAPE     IP   F  133        SEQ
     F*** BEGIN ADD *** 01/31/96 ***
     FFMGZL1    IF   E           K DISK
     F                                     RENAME(PGZ01:PGZ02)
     F*** END ADD *** 01/31/96 ***
     FFMGZL     O    E           K DISK
     FFMRCL     UF A E           K DISK
     F*** BEGIN ADD *** 12/18/96 ***
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*** END ADD *** 12/18/96 ***

     D CVZ             S              1    DIM(87)                              COVERED ZIP CODES
     D CVF             S              1    DIM(3)                               COVERED FROM ZIP COD
     D CVT             S              1    DIM(3)                               COVERED TO ZIP CODE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IQTAPE     AA  01
     I                                  1    2  XXTYPE
     I*** RECORD FORMAT 20
     I                                  3    5  X2ZIPA
     I                                  6   45  X2DESC
     I                                 47  133  X2CVZP
     I*** RECORD FORMAT 30
     I                                  3    5  X3ZIPA
     I                                  6   10  X3CPT4
     I                                  6    6  X3CPID
     I                                 11   11  X3BRPT
     I                                 12   46  X3DESC
     I                                 47   55  X3FIL1
     I                                 56   61  X3#CLM
     I                                 62   72  X3FIL2
     I                                 73   78  X3P50
     I                                 79   84  X3P60
     I                                 85   90  X3P70
     I                                 91   96  X3P75
     I                                 97  102  X3P80
     I                                103  108  X3P85
     I                                109  114  X3P90
     I                                115  120  X3P95
     I                                121  133  X3FIL3

     C*
     C*  Reasonable & Customary Master File
     C     RCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*** BEGIN ADD *** 01/31/96 ***
     C*
     C*  GEOGRAPHIC ZIP AREA LOGICAL
     C*
     C     GZKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXFRZP
     C*
     C*** END ADD *** 01/31/96 ***
     C*
     C*  Entry List
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQPCT             2            ADD 11/14/96
     C                   PARM                    QQDATA            1            ADD 06/24/98
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Initialization Section
     C                   TIME                    DSDTTM
     C     *LIKE         DEFINE    GZCO#         XXCO#
     C     *LIKE         DEFINE    GZZIPA        XXZIPA
     C*** BEGIN ADD *** 01/31/96 ***
     C     *LIKE         DEFINE    GZFRZP        XXFRZP
     C*** END ADD *** 01/31/96 ***
     C     *LIKE         DEFINE    RCCPT4        XXCPT4
     C     *LIKE         DEFINE    RCEFDT        XXEFDT
     C                   MOVE      QQCO#         XXCO#
     C*
     C*** BEGIN ADD *** 11/14/96 **************************************
     C                   MOVE      QQPCT         XXPCT             2
     C**************       Z-ADD0         RCRCDS
     C*** END ADD *** 11/14/96 ****************************************
     C*** BEGIN ADD *** 12/18/96 **************************************
     C     XXINIT        IFEQ      *BLANK
     C                   Z-ADD     0             RCRCDS            7 0
     C                   Z-ADD     0             GZRCDS            7 0
     C                   Z-ADD     0             RCBYP             7 0          ADD 01/05/98
     C                   MOVE      'X'           XXINIT            1
     C                   ENDIF
     C*** END ADD *** 12/18/96 ****************************************
     C*
     C                   Z-ADD     010101        XXEFDT
     C                   Z-ADD     0             XXPERC            6 0
     C                   Z-ADD     0             X                 2 0
     C                   Z-ADD     0             Y                 2 0
     C                   Z-ADD     0             Z                 2 0
     C*
     C*** Read the Tape - MDR Record
     C*
     C* ========
     C*  Record Format '20' - Geographic Zip Area
     C     XXTYPE        IFEQ      '20'
     C*
     C*  Verify that Zip Area record is not already in file
     C                   Z-ADD     XXCO#         GZCO#
     C                   MOVEL     X2ZIPA        GZZIPA
     C                   MOVEL     X2DESC        GZDESC
     C*
     C                   MOVE      *BLANKS       CVZ
     C                   MOVE      *BLANKS       CVF
     C                   MOVE      *BLANKS       CVT
     C                   MOVEA     X2CVZP        CVZ
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y
     C                   Z-ADD     1             Z
     C*
     C     NXTZIP        TAG
     C     Z             CABGT     90            NEXT
     C     X             IFLT      88
     C     CVZ(X)        ANDNE     ')'
     C*  Break out the Covered Zip Codes field and write 1 record for each range
     C*  Setup From Covered Zip Code field
     C     Y             DOWLT     4
     C                   MOVEA     CVZ(X)        CVF(Y)
     C                   ADD       1             X
     C                   ADD       1             Y
     C                   END                                                    END DOWLT 4
     C                   MOVEA     CVF           GZFRZP
     C                   MOVE      *BLANKS       CVF
     C*** BEGIN ADD *** 01/31/96 ***
     C*
     C*  CHAIN TO THE GEOGRAPHIC ZIP AREA LOGICAL AND IF THE RECORD EXISTS
     C*  *IN52 = *OFF GOTO NXTZIP.
     C                   MOVE      GZZIPA        XXZIPA
     C                   MOVE      GZFRZP        XXFRZP
     C     GZKY          CHAIN     PGZ02                              52
     C  N52              ADD       1             Z
     C     *IN52         CABEQ     *OFF          NXTZIP
     C*** BEGIN ADD *** 08/07/02 **************************************
     C                   Z-ADD     XXEFDT        GZEFDT
     C*** END   ADD *** 08/07/02 **************************************
     C*
     C*** END ADD *** 01/31/96 ***
     C*
     C*  If byte is a '-' then load To Covered Zip Code Field with next number
     C     CVZ(X)        IFEQ      '-'
     C                   ADD       1             X
     C                   Z-ADD     1             Y
     C     Y             DOWLT     4
     C                   MOVEA     CVZ(X)        CVT(Y)
     C                   ADD       1             X
     C                   ADD       1             Y
     C                   END                                                    END DOWLT 4
     C*
     C                   MOVEA     CVT           GZTOZP
     C                   MOVE      *BLANKS       CVT
     C                   ADD       1             X
     C                   Z-ADD     1             Y
     C                   MOVE      'A'           GZSTAT
     C                   MOVEL     Q1USER        GZUSER
     C                   MOVEL     Q1PGM         GZPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C                   Z-ADD     DSTIME        GZTIME
     C                   MOVEL     Q1USER        GZIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZIDT
     C                   WRITE     PGZ01
     C                   ADD       1             GZRCDS                         ADD 12/18/96
     C                   GOTO      NXTZIP
     C*
     C                   ELSE                                                   ELSE EQ '-'
     C*
     C*  If byte is a ',' then load To Covered Zip Code Field with "From" value
     C     CVZ(X)        IFEQ      ','
     C                   MOVE      GZFRZP        GZTOZP
     C                   ADD       1             X
     C                   Z-ADD     1             Y
     C                   MOVE      'A'           GZSTAT
     C                   MOVEL     Q1USER        GZUSER
     C                   MOVEL     Q1PGM         GZPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C                   Z-ADD     DSTIME        GZTIME
     C                   MOVEL     Q1USER        GZIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZIDT
     C                   WRITE     PGZ01
     C                   ADD       1             GZRCDS                         ADD 12/18/96
     C                   GOTO      NXTZIP
     C*
     C                   ELSE                                                   ELS ','
     C*
     C*  If byte is a BLK then load To Covered Zip Code Field with "From" value
     C     CVZ(X)        IFEQ      ')'
     C                   MOVE      GZFRZP        GZTOZP
     C                   MOVE      'A'           GZSTAT
     C                   MOVEL     Q1USER        GZUSER
     C                   MOVEL     Q1PGM         GZPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C                   Z-ADD     DSTIME        GZTIME
     C                   MOVEL     Q1USER        GZIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZIDT
     C                   WRITE     PGZ01
     C                   ADD       1             GZRCDS                         ADD 12/18/96
     C                   END                                                    END *BLANK
     C                   END                                                    END ','
     C                   END                                                    END '-'
     C*
     C                   END                                                    END LT 88
     C*
     C                   ELSE                                                   ELSE EQ '20'
     C*
     C* ========
     C*  Record Format '30' - Procedure Code
     C     XXTYPE        IFEQ      '30'
     C*
     C     QQDATA        IFEQ      'H'
     C*
     C     X3CPID        IFNE      'A'
     C     X3CPID        ANDNE     'E'
     C     X3CPID        ANDNE     'J'
     C     X3CPID        ANDNE     'K'
     C     X3CPID        ANDNE     'L'
     C                   ADD       1             RCBYP                          ADD 01/05/98
     C                   GOTO      NEXT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      X3ZIPA        XXZIPA
     C                   MOVE      X3CPT4        XXCPT4
     C*
     C*  Verify that R&C record is not already in file
     C     RCKY          CHAIN     PRC01                              51
     C     *IN51         IFEQ      '1'
     C                   Z-ADD     XXCO#         RCCO#
     C                   MOVE      X3ZIPA        RCZIPA
     C                   MOVEL     X3CPT4        RCCPT4
     C                   Z-ADD     XXEFDT        RCEFDT
     C                   MOVEL     X3DESC        RCDSC1
     C                   MOVE      *BLANKS       RCDSC2
     C*
     C*** BEGIN DEL *** 11/14/96 **************************************
     C**********           MOVE X3P80     XXPERC
     C*** END DEL *** 11/14/96 ****************************************
     C*
     C*** BEGIN ADD *** 11/14/96 **************************************
     C*
     C*  Obtain Percentage Amount
     C*
     C                   SELECT
     C     XXPCT         WHENEQ    '50'
     C                   MOVE      X3P50         XXPERC
     C     XXPCT         WHENEQ    '60'
     C                   MOVE      X3P60         XXPERC
     C     XXPCT         WHENEQ    '70'
     C                   MOVE      X3P70         XXPERC
     C     XXPCT         WHENEQ    '80'
     C                   MOVE      X3P80         XXPERC
     C     XXPCT         WHENEQ    '85'
     C                   MOVE      X3P85         XXPERC
     C     XXPCT         WHENEQ    '90'
     C                   MOVE      X3P90         XXPERC
     C     XXPCT         WHENEQ    '95'
     C                   MOVE      X3P95         XXPERC
     C                   ENDSL
     C*
     C*** END ADD *** 11/14/96 ****************************************
     C*
     C*** BEG ADD *** 01/05/99 *************************
     C     QQDATA        IFEQ      'H'
     C     XXPERC        ANDLT     1
     C                   Z-ADD     1             RCMRA
     C                   ELSE
     C*** END ADD *** 01/05/99 *************************
     C                   Z-ADD     XXPERC        RCMRA
     C                   ENDIF                                                  ADD 01/05/99
     C                   Z-ADD     *ZEROS        RCFOLU
     C                   Z-ADD     *ZEROS        RCANAM
     C                   Z-ADD     *ZEROS        RCPROF
     C*********  X3BRPT    IFNE *BLANKS                    DEL 08/10/98
     C     X3BRPT        IFEQ      '*'                                          ADD 08/10/98
     C                   MOVE      'Y'           RCRPT
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVE      'N'           RCRPT
     C                   END                                                    END  NE *BLANKS
     C                   MOVE      'N'           RCSURG
     C                   MOVE      'N'           RCSUPP
     C                   MOVE      'N'           RCFSR
     C                   MOVE      'A'           RCSTAT
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C                   Z-ADD     DSTIME        RCTIME
     C                   MOVEL     Q1USER        RCIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCIDT
     C*
     C*** BEGIN ADD *** 04/27/94 ****************************************
     C                   MOVE      *BLANKS       RC5A1
     C                   MOVE      *BLANKS       RC5A2
     C                   MOVE      *BLANKS       RC5A3
     C                   MOVE      *BLANKS       RC5A4
     C                   MOVE      *BLANKS       RC5A5
     C*
     C                   MOVE      *BLANKS       RC1A1
     C                   MOVE      *BLANKS       RC1A2
     C                   MOVE      *BLANKS       RC1A3
     C                   MOVE      *BLANKS       RC1A4
     C                   MOVE      *BLANKS       RC1A5
     C*
     C                   Z-ADD     *ZEROS        RC52N1
     C                   Z-ADD     *ZEROS        RC52N2
     C                   Z-ADD     *ZEROS        RC52N3
     C                   Z-ADD     *ZEROS        RC52N4
     C                   Z-ADD     *ZEROS        RC52N5
     C*
     C                   Z-ADD     *ZEROS        RC31N1
     C                   Z-ADD     *ZEROS        RC31N2
     C                   Z-ADD     *ZEROS        RC31N3
     C                   Z-ADD     *ZEROS        RC31N4
     C                   Z-ADD     *ZEROS        RC31N5
     C*
     C                   Z-ADD     *ZEROS        RC92N1
     C                   Z-ADD     *ZEROS        RC92N2
     C                   Z-ADD     *ZEROS        RC92N3
     C                   Z-ADD     *ZEROS        RC92N4
     C                   Z-ADD     *ZEROS        RC92N5
     C*** END ADD ***** 04/27/94 ****************************************
     C*
     C                   WRITE     PRC01
     C                   ADD       1             RCRCDS                         ADD 12/18/96
     C                   END                                                    END 1 *IN51
     C*
     C                   END                                                    END  EQ '30'
     C                   END                                                    END  EQ '20'
     C*
     C* ========
     C*  Read next MDR record
     C*
     C     NEXT          TAG                                                    ADD 06/24/98
     C*
     C* ========
     C* End of Program
     C*
     CLR                 EXCEPT    PRT
     C*
     F*** BEGIN ADD *** 12/18/96 ***
     OQSYSPRT   E            PRT            1 06
     O                       DSDATE        Y      8
     O                                           76 'PAGE:'
     O                       PAGE          Z     80
     O                                           60 'MEDICODE LOAD REPORT'
     O          E            PRT            3
     O                       DSTIME               8 '  :  :  '
     O                                           80 'FS900R1'
     O          E            PRT            2
     O                                           27 '  CLIENT NUMBER  . . . :'
     O                       XXCO#         Z     42
     O          E            PRT            2
     O                                           27 '  RCDS LOADED FOR FMRCP:'
     O                       RCRCDS        J     42
     O          E            PRT            2
     O                                           27 'RCDS BYPASSED FOR FMRCP:'
     O                       RCBYP         J     42
     O          E            PRT            3
     O                                           27 '  RCDS LOADED FOR FMGZP:'
     O                       GZRCDS        J     42
     O          E            PRT            2
     O                                           18 'PERCENTILE =    %'
     O                       XXPCT               16
     F*** END ADD *** 12/18/96 ***
