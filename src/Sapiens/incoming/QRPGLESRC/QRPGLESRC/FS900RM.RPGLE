     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*    PROGRAM: FS900RM
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
     H******************************************************************
     H*
     H*  CALLED FROM: FS900RM AND FS900RD
     H*  CALLS TO   : NONE
     H*
     H*****************************************************************
     H*  REVISIONS:                                                    *
     H*                                                                *
     H*  04/24/12 C:280 SUPPORT: 79604                                 *
     H*                 CHANGE FROM MOVE TO Z-ADD                      *
     H*                                                                *
     H*  09/17/10 C:257 PROJECT: MC345                                 *
     H*                 LOAD U&C HCPC AND MEDICAL                      *
     H*                                                                *
     H*  05/18/07 C:53 SUPPORT 57931                                   *
     H*                CORRECT DATA LOAD IN RC1A2                      *
     H*                                                                *
     H*  10/24/06 C:79 SUPPORT 55441                                   *
     H*                CLEAR FMR1P FIELDS BEFORE LOADING               *
     H*                                                                *
     H*  11/30/04 C:79 PROJECT 25MNU                                   *
     H*                HCPCS MISSING G,H CODES                         *
     H*                                                                *
     H*  01/15/04 C:62 PROJECT 114MGP                                  *
     H*                WRITE TO FMR1P                                  *
     H*                                                                *
     H*  08/07/02 C:62 PROJECT FS244                                   *
     H*                ADD EFFECTIVE DATE TO FMGZP PROCESSING          *
     H*
     H*  04/04/01 C:62 PROJECT: FS225
     H*                ASSIGN 'Y' TO RCFSR.
     H*
     H*  08/10/98 C:53 PROJECT: MCMNT
     H*                CORRECT THE READ OF BY REPORT FLAG
     H*
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
     FFSMMTMP   IF   E             DISK
     FFMGZL1    IF   E           K DISK
     F                                     RENAME(PGZ01:PGZ02)
     FFMGZL     O    E           K DISK
     FFMRCL     UF A E           K DISK
     FFMRCL3    IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     F*** BEGIN ADD *** 01/15/04 **************************************
     FFMR1L     UF A E           K DISK
     F*** END   ADD *** 01/15/04 **************************************
     FFSAXTMPL  IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D CVZ             S              1    DIM(87)                              COVERED ZIP CODES
     D CVF             S              1    DIM(3)                               COVERED FROM ZIP COD
     D CVT             S              1    DIM(3)                               COVERED TO ZIP CODE
     D*** BEG ADD *** 05/18/07 **************************************
     D AZ              S              1    DIM(26) CTDATA PERRCD(1)             A - Z
     D*** RECORD FORMAT 20
     D                 DS
     D  X2REC                  1    133
     D  X2TYPE                 1      2
     D  X2ZIPA                 3      5
     D  X2DESC                 6     45
     D  X2CVZP                47    133
     D*** RECORD FORMAT 30
     D                 DS
     D  X3REC                  1    133
     D  X3TYPE                 1      2
     D  X3ZIPA                 3      5
     D  X3CPT4                 6     10
     D  X3CPID                 6      6
     D  X3BRPT                11     11
     D  X3DESC                12     46
     D  X3FIL1                47     55
     D  X3#CLM                56     61
     D  X3FIL2                62     72
     D  X3P50                 73     78
     D  X3P60                 79     84
     D  X3P70                 85     90
     D  X3P75                 91     96
     D  X3P80                 97    102
     D  X3P85                103    108
     D  X3P90                109    114
     D  X3P95                115    120
     D  X3FIL3               121    133
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END ADD *** 05/18/07 **************************************
     IPRC02
     I              RCCO#                       R2CO#
     I              RCCPT4                      R2CPT4
     I              RCEFDT                      R2EFDT
     I              RCDSC1                      R2DSC1
     I              RCDSC2                      R2DSC2
     I              RCMRA                       R2MRA
     I              RCFOLU                      R2FOLU
     I              RCANAM                      R2ANAM
     I              RCPROF                      R2PROF
     I              RCRPT                       R2RPT
     I              RCSURG                      R2SURG
     I              RCSUPP                      R2SUPP
     I              RCFSR                       R2FSR
     I              RCSTAT                      R2STAT
     I              RCUSER                      R2USER
     I              RCPGM                       R2PGM
     I              RCTIME                      R2TIME
     I              RCDT                        R2DT
     I              RCIUSR                      R2IUSR
     I              RCIDT                       R2IDT
     I              RCZIPA                      R2ZIPA
     I              RC5A1                       R25A1
     I              RC5A2                       R25A2
     I              RC5A3                       R25A3
     I              RC5A4                       R25A4
     I              RC5A5                       R25A5
     I              RC1A1                       R21A1
     I              RC1A2                       R21A2
     I              RC1A3                       R21A3
     I              RC1A4                       R21A4
     I              RC1A5                       R21A5
     I              RC52N1                      R252N1
     I              RC52N2                      R252N2
     I              RC52N3                      R252N3
     I              RC52N4                      R252N4
     I              RC52N5                      R252N5
     I              RC31N1                      R231N1
     I              RC31N2                      R231N2
     I              RC31N3                      R231N3
     I              RC31N4                      R231N4
     I              RC31N5                      R231N5
     I              RC92N1                      R292N1
     I              RC92N2                      R292N2
     I              RC92N3                      R292N3
     I              RC92N4                      R292N4
     I              RC92N5                      R292N5
     I              RCCONF                      R2CONF

     C*
     C*  Reasonable & Customary Master File
     C     RCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C     RCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C*** BEGIN ADD *** 01/15/04 **************************************
     C*
     C* UCR PERCENTILE FILE
     C*
     C     R1KY1         KLIST
     C                   KFLD                    RCCO#
     C                   KFLD                    RCZIPA
     C                   KFLD                    RCCPT4
     C                   KFLD                    RCEFDT
     C*
     C     R1KY2         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2ZIPA
     C                   KFLD                    R2CPT4
     C                   KFLD                    R2EFDT
     C*** END   ADD *** 01/15/04 **************************************
     C*
     C*  GEOGRAPHIC ZIP AREA LOGICAL
     C*
     C     GZKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXFRZP
     C*
     C*  Crosswalk Table
     C*
     C     AXKY          KLIST
     C                   KFLD                    KKSCPT
     C*
     C*  Entry List
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQTAPE           10
     C                   PARM                    QQSEL             1
     C                   PARM                    QQPCT             2
     C                   PARM                    QQLABL           10
     C                   PARM                    QQPGM            10
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
     C     *LIKE         DEFINE    GZFRZP        XXFRZP
     C     *LIKE         DEFINE    RCCPT4        XXCPT4
     C     *LIKE         DEFINE    RCCPT4        KKSCPT
     C     *LIKE         DEFINE    RCEFDT        XXEFDT
     C*** BEGIN ADD *** 01/15/04 **************************************
     C     *LIKE         DEFINE    R1CV50        XXCV50
     C     *LIKE         DEFINE    R1CV60        XXCV60
     C     *LIKE         DEFINE    R1CV70        XXCV70
     C     *LIKE         DEFINE    R1CV75        XXCV75
     C     *LIKE         DEFINE    R1CV80        XXCV80
     C     *LIKE         DEFINE    R1CV85        XXCV85
     C     *LIKE         DEFINE    R1CV90        XXCV90
     C     *LIKE         DEFINE    R1CV95        XXCV95
     C*** BEGIN ADD *** 10/24/06 **************************************
     C     *LIKE         DEFINE    R1MR50        XXMR50
     C     *LIKE         DEFINE    R1MR60        XXMR60
     C     *LIKE         DEFINE    R1MR70        XXMR70
     C     *LIKE         DEFINE    R1MR75        XXMR75
     C     *LIKE         DEFINE    R1MR80        XXMR80
     C     *LIKE         DEFINE    R1MR85        XXMR85
     C     *LIKE         DEFINE    R1MR90        XXMR90
     C     *LIKE         DEFINE    R1MR95        XXMR95
     C*** END   ADD *** 10/24/06 **************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
     C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
     C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
     C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
     C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
     C                   PARM      *BLANK        ##KCIF            1            Keep Claimant info
     C                   PARM      *BLANK        ##CURS            1            PBILLS entry cursor
     C                   PARM      *BLANK        ##PCIB            1            Prov Check in Batch
     C                   PARM      *BLANK        ##CLT#            3            Client Number
     C                   PARM      *BLANK        ##UCPF            1            UCR Percentile
     C*** END   ADD *** 01/15/04 **************************************
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   MOVE      QQPCT         XXPCT             2
     C     XXINIT        IFEQ      *BLANK
     C                   Z-ADD     0             RCRCDS            7 0
     C                   Z-ADD     0             GZRCDS            7 0
     C                   MOVE      'X'           XXINIT            1
     C                   ENDIF
     C*
     C                   Z-ADD     010101        XXEFDT
     C                   Z-ADD     0             XXPERC            6 0
     C                   Z-ADD     0             W                 7 0
     C                   Z-ADD     0             X                 2 0
     C                   Z-ADD     0             Y                 2 0
     C                   Z-ADD     0             Z                 2 0
     C*
     C*** Read the Tempfile - MDR Record
     C*
     C                   READ      PMM01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     MMRTYP        IFEQ      '10'
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C     MMRTYP        IFEQ      '20'
     C                   MOVEL     MMRTYP        X2REC
     C                   MOVE      MMREST        X2REC
     C                   EXSR      REC20
     C                   ENDIF
     C*
     C     MMRTYP        IFEQ      '30'
     C                   MOVEL     MMRTYP        X3REC
     C                   MOVE      MMREST        X3REC
     C                   EXSR      REC30
     C                   ENDIF
     C*
     C* ========
     C*  Read next MDR record
     C*
     C     NEXT          TAG                                                    ADD 06/24/98
     C*
     C                   READ      PMM01                                  99
     C*
     C                   ENDDO
     C*
     C* ========
     C* End of Program
     C*
     C                   SETON                                        LR
     CLR                 EXCEPT    PRT
     C*
     C*================================================================
     C* REC20 - Record format '20' - Geographic Zip Area
     C*================================================================
     C     REC20         BEGSR
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
     C                   ENDSR
     C*
     C*================================================================
     C* REC30 - Record format '30' - Procedure Code
     C*================================================================
     C     REC30         BEGSR
     C*
     C     QQSEL         IFEQ      'H'
     C*
     C*** BEG ADD *** 11/30/04 ****************************************
     C*
     C* SKIP CPT AND DENTAL CODES
     C*
     C     X3CPID        IFGE      '0'
     C     X3CPID        ANDLE     '9'
     C     X3CPID        OREQ      'D'
     C*** END ADD *** 11/30/04 ****************************************
     C*
     C*** BEG DEL *** 11/30/04 ****************************************
     C********** X3CPID    IFNE 'A'
     C********** X3CPID    ANDNE'E'
     C********** X3CPID    ANDNE'J'
     C********** X3CPID    ANDNE'K'
     C********** X3CPID    ANDNE'L'
     C*** END DEL *** 11/30/04 ****************************************
     C                   GOTO      END30
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      X3ZIPA        XXZIPA
     C                   MOVE      X3CPT4        XXCPT4
     C                   MOVE      X3CPT4        KKSCPT
     C*** BEGIN ADD *** 01/15/04 **************************************
     C                   Z-ADD     *ZEROS        XXCV50
     C                   Z-ADD     *ZEROS        XXCV60
     C                   Z-ADD     *ZEROS        XXCV70
     C                   Z-ADD     *ZEROS        XXCV75
     C                   Z-ADD     *ZEROS        XXCV80
     C                   Z-ADD     *ZEROS        XXCV85
     C                   Z-ADD     *ZEROS        XXCV90
     C                   Z-ADD     *ZEROS        XXCV95
     C*** END   ADD *** 01/15/04 **************************************
     C*
     C*  Verify that R&C record is not already in file
     C     RCKY          CHAIN     PRC01                              51
     C*** BEG CHG *** 09/17/10 ****************************************
     C********   *IN51     IFEQ '1'
     C     *IN51         IFEQ      '0'
     C                   EXSR      UPDFIL
     C*** END CHG *** 09/17/10 ****************************************
     C                   ELSE
     C                   CLEAR                   PRC01
     C*
     C*  Load the ANES amount if it CROSSWALKS
     C*
     C     AXKY          CHAIN     PAX01                              55
     C     *IN55         IFEQ      *OFF
     C     AXSTAT        ANDEQ     *BLANKS
     C                   MOVEL     AXACPT        XXCPT4
     C     RCKY1         CHAIN     PRC02                              56
     C     *IN56         IFEQ      *OFF
     C                   Z-ADD     R2ANAM        RCANAM
     C                   MOVEL     R21A2         RC1A2
     C                   Z-ADD     R2CONF        RCCONF
     C                   ADD       1             W
     C*** BEGIN ADD *** 01/15/04 **************************************
     C*
     C     ##UCPF        IFEQ      'Y'
     C     R1KY2         CHAIN(N)  PR101                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     R1CV50        XXCV50
     C                   Z-ADD     R1CV60        XXCV60
     C                   Z-ADD     R1CV70        XXCV70
     C                   Z-ADD     R1CV75        XXCV75
     C                   Z-ADD     R1CV80        XXCV80
     C                   Z-ADD     R1CV85        XXCV85
     C                   Z-ADD     R1CV90        XXCV90
     C                   Z-ADD     R1CV95        XXCV95
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/15/04 **************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     XXCO#         RCCO#
     C                   MOVE      X3ZIPA        RCZIPA
     C                   MOVEL     X3CPT4        RCCPT4
     C                   Z-ADD     XXEFDT        RCEFDT
     C                   MOVEL     X3DESC        RCDSC1
     C                   MOVE      *BLANKS       RCDSC2
     C*
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
     C*
     C                   Z-ADD     XXPERC        RCMRA
     C                   Z-ADD     *ZEROS        RCFOLU
     C                   Z-ADD     *ZEROS        RCPROF
     C*** BEG ADD *** 09/17/10 ****************************************
     C                   MOVEL     X3FIL1        XXHCPC            2
     C                   SELECT
     C     XXHCPC        WHENEQ    'NU'
     C*** BEG CHG *** 04/24/12 ****************************************
     C***                  MOVE XXPERC    RCMRA
     C                   Z-ADD     XXPERC        RCMRA
     C*** END CHG *** 04/24/12 ****************************************
     C     XXHCPC        WHENEQ    'RR'
     C*** BEG CHG *** 04/24/12 ****************************************
     C***                  MOVE XXPERC    RCANAM
     C                   Z-ADD     XXPERC        RCANAM
     C*** END CHG *** 04/24/12 ****************************************
     C     XXHCPC        WHENEQ    'UE'
     C*** BEG CHG *** 04/24/12 ****************************************
     C***                  MOVE XXPERC    RC92N2
     C                   Z-ADD     XXPERC        RC92N2
     C*** END CHG *** 04/24/12 ****************************************
     C                   ENDSL
     C*
     C*** END ADD *** 09/17/10 ****************************************
     C     X3BRPT        IFEQ      '*'                                          ADD 08/10/98
     C                   MOVE      'Y'           RCRPT
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVE      'N'           RCRPT
     C                   END                                                    END  NE *BLANKS
     C                   MOVE      'N'           RCSURG
     C                   MOVE      'N'           RCSUPP
     C*** BEGIN CHG *** 04/04/01 **************************************
     C**********           MOVE 'N'       RCFSR
     C                   MOVE      'Y'           RCFSR
     C*** END   CHG *** 04/04/01 **************************************
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
     C*** BEG ADD *** 05/18/07 **************************************
     C                   MOVEL     RCCPT4        XCPA              1
     C                   MOVEL     RCCPT4        XCP#              4 0
     C                   Z-ADD     1             A                 2 0
     C     XCPA          LOOKUP    AZ(A)                                  99
     C     *IN99         IFEQ      *ON
     C     XCP#          IFGE      0001
     C     XCP#          ANDLE     9999
     C     RC1A2         ANDEQ     *BLANKS
     C                   MOVE      'U'           RC1A2
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 05/18/07 **************************************
     C*
     C                   WRITE     PRC01
     C                   ADD       1             RCRCDS                         ADD 12/18/96
     C*** BEGIN ADD *** 01/15/04 **************************************
     C*
     C* WRITE TO PERCENTILE FILE
     C*
     C     ##UCPF        IFEQ      'Y'
     C                   EXSR      WUPR1P
     C                   ENDIF
     C*** END   ADD *** 01/15/04 **************************************
     C                   END                                                    END 1 *IN51
     C*
     C     END30         ENDSR
     C*** BEGIN ADD *** 09/17/10 **************************************
     C*================================================================
     C* UPDFIL - Update FMRCP record
     C*================================================================
     C     UPDFIL        BEGSR
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
     C                   MOVEL     X3FIL1        XXHCPC            2
     C                   SELECT
     C     XXHCPC        WHENEQ    'NU'
     C*** BEG CHG *** 04/24/12 ****************************************
     C***                  MOVE XXPERC    RCMRA
     C                   Z-ADD     XXPERC        RCMRA
     C*** END CHG *** 04/24/12 ****************************************
     C     XXHCPC        WHENEQ    'RR'
     C*** BEG CHG *** 04/24/12 ****************************************
     C***                  MOVE XXPERC    RCANAM
     C                   Z-ADD     XXPERC        RCANAM
     C*** END CHG *** 04/24/12 ****************************************
     C     XXHCPC        WHENEQ    'UE'
     C*** BEG CHG *** 04/24/12 ****************************************
     C***                  MOVE XXPERC    RC92N2
     C                   Z-ADD     XXPERC        RC92N2
     C*** END CHG *** 04/24/12 ****************************************
     C                   ENDSL
     C*
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
     C                   UPDATE    PRC01
     C*
     C     UPFDIL        ENDSR
     C*** END   ADD *** 09/17/10 **************************************
     C*** BEGIN ADD *** 01/15/04 **************************************
     C*================================================================
     C* WUPR1P - Write/Update FMR1P record and after processing
     C*================================================================
     C     WUPR1P        BEGSR
     C*
     C                   CLEAR                   PR101
     C     R1KY1         CHAIN     PR101                              99
     C*
     C*** BEGIN ADD *** 10/24/06 **************************************
     C                   CLEAR                   R1MR50
     C                   CLEAR                   R1MR60
     C                   CLEAR                   R1MR70
     C                   CLEAR                   R1MR75
     C                   CLEAR                   R1MR80
     C                   CLEAR                   R1MR85
     C                   CLEAR                   R1MR90
     C                   CLEAR                   R1MR95
     C*** END   ADD *** 10/24/06 **************************************
     C                   MOVE      X3P50         R1MR50
     C                   MOVE      X3P60         R1MR60
     C                   MOVE      X3P70         R1MR70
     C                   MOVE      X3P75         R1MR75
     C                   MOVE      X3P80         R1MR80
     C                   MOVE      X3P85         R1MR85
     C                   MOVE      X3P90         R1MR90
     C                   MOVE      X3P95         R1MR95
     C*
     C                   MULT      100           R1MR50
     C                   MULT      100           R1MR60
     C                   MULT      100           R1MR70
     C                   MULT      100           R1MR75
     C                   MULT      100           R1MR80
     C                   MULT      100           R1MR85
     C                   MULT      100           R1MR90
     C                   MULT      100           R1MR95
     C*
     C                   Z-ADD     XXCV50        R1CV50
     C                   Z-ADD     XXCV60        R1CV60
     C                   Z-ADD     XXCV70        R1CV70
     C                   Z-ADD     XXCV75        R1CV75
     C                   Z-ADD     XXCV80        R1CV80
     C                   Z-ADD     XXCV85        R1CV85
     C                   Z-ADD     XXCV90        R1CV90
     C                   Z-ADD     XXCV95        R1CV95
     C*
     C                   MOVEL     RCSTAT        R1STAT
     C                   MOVEL     Q1USER        R1USER
     C                   MOVEL     Q1PGM         R1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R1DT
     C                   MOVEL     DSTIME        R1TIME
     C*
     C     *IN99         IFEQ      *OFF
     C                   UPDATE    PR101
     C                   ELSE
     C                   Z-ADD     RCCO#         R1CO#
     C                   MOVEL(P)  RCZIPA        R1ZIPA
     C                   MOVEL(P)  RCCPT4        R1PRCD
     C                   Z-ADD     RCEFDT        R1EFDT
     C                   WRITE     PR101
     C                   FEOD      FMR1L
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 01/15/04 **************************************
     C*
     OQSYSPRT   E            PRT            1 06
     O                       DSDATE        Y      8
     O                                           76 'PAGE:'
     O                       PAGE          Z     80
     O                                           30 'MEDICODE LOAD REPORT'
     O          E            PRT            1
     O                       DSTIME               8 '  :  :  '
     O                                           80 'FS900R1'
     O          E            PRT            2
     O                                           27 'CLIENT NUMBER  . . . :'
     O                       XXCO#         Z     40
     O          E            PRT            2
     O                                           27 'RCDS LOADED FOR FMRCP:'
     O                       RCRCDS        J     41
     O          E            PRT            2
     O                                           27 'RCDS CHANGED FOR CRX:'
     O                       W             J     41
     O          E            PRT            2
     O                                           27 'RCDS LOADED FOR FMGZP:'
     O                       GZRCDS        J     41
     O          E            PRT            2
     O                                           18 'PERCENTILE =    %'
     O                       XXPCT               16
**
A
B
C
D
E
F
G
H
I
J
K
L
M
N
O
P
Q
R
S
T
U
V
W
X
Y
Z
