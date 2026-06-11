     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: FS900RA
     H*    DESCRIPTION: Convert MDR's UCR Payment File
     H*    CREATED: 02/27/01 C:117 PROJECT: FS225
     H*                            CREATED ANES MODULE
     H*                            MOVED MEDICODE PGMS TO BASE
     H*****************************************************************
     H*
     H*  CALLED FROM: FS900CA
     H*  CALLS TO   : NONE
     H*
     H*****************************************************************
     H*  REVISIONS:                                                    *
     H*                                                                *
     H*  06/13/16 C:79 PROJECT 126MFO                                  *
     H*                Allow update of FMRCP                           *
     H*                                                                *
     H*  03/26/04 C:79 PROJECT 114MGP                                  *
     H*                SET RCFSR = Y SO ANES WILL USE RULES            *
     H*                                                                *
     H*  01/15/04 C:62 PROJECT 114MGP                                  *
     H*                WRITE TO FMR1P                                  *
     H*                                                                *
     H*  08/07/02 C:62 PROJECT FS244                                   *
     H*                ADD EFFECTIVE DATE TO FMGZP PROCESSING          *
     H*                                                                *
     H******************************************************************
     H*  INDICATORS:                                                   *
     H*  50    CHAIN ON PRC01                                          *
     H*  51    CHAIN ON PGZ01                                          *
     H*  52    CHAIN ON PGZ02                                          *
     H*  60    READ/DOWEQ ON MDR01                                     *
     H*  99    RESTART                                                 *
     H******************************************************************
     H*  FILES:                                                        *
     H*  FSAXTEMP (PAX01): None                                        *
     H*  FSAZTEMP (PAZ01): None                                        *
     H*  FSARTEMP (PAR01): None                                        *
     H*  FSAFTEMP (PAF01): None                                        *
     H*  FSACTEMP (PAC01): None                                        *
     H*  FMGZP    (PGZ01): CO#/ZIPA                                    *
     H*  FMGZL1   (PGZ02): CO#/ZIPA/FRZP                               *
     H*  FMRCP    (PRC01): CO#/ZIPA/CPT4/EFDT                          *
     H******************************************************************
     FFMGZL1    IF   E           K DISK
     F                                     RENAME(PGZ01:PGZ02)
     FFMGZL     O    E           K DISK
     F*** Beg Chg *** 06/13/16 *************************************
     F******FMRCL     O    E           K DISK
     FFMRCL     UF A E           K DISK
     F*** End Chg *** 06/13/16 *************************************
     F*** BEGIN ADD *** 01/15/04 **************************************
     FFMR1L     UF A E           K DISK
     F*** END   ADD *** 01/15/04 **************************************
     FFSAZTEMP  IF   E             DISK
     FFSACTMPL  IF   E           K DISK
     FFSARTMPL  IF   E           K DISK
     FFSAFTMPL  IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D CVZ             S              1    DIM(87)                              COVERED ZIP CODES
     D CVF             S              1    DIM(3)                               COVERED FROM ZIP COD
     D CVT             S              1    DIM(3)                               COVERED TO ZIP CODE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Reasonable & Customary Master File
     C     RCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C*  Reasonable & Customary Master File
     C     RCKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C*** BEGIN ADD *** 01/15/04 **************************************
     C*
     C* UCR PERCENTILE FILE
     C*
     C     R1KY          KLIST
     C                   KFLD                    RCCO#
     C                   KFLD                    RCZIPA
     C                   KFLD                    RCCPT4
     C                   KFLD                    RCEFDT
     C*** END   ADD *** 01/15/04 **************************************
     C*
     C*  GEOGRAPHIC ZIP AREA LOGICAL
     C*
     C     GZKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKFRZP
     C*
     C     GZKY1         KLIST
     C                   KFLD                    KKCO#
     C*
     C*  ANES Relative Value Table
     C*
     C     ARKY          KLIST
     C                   KFLD                    KKRID
     C                   KFLD                    KKCPT4
     C*
     C*  ANES Geo Factor Table
     C*
     C     AFKY          KLIST
     C                   KFLD                    KKZIPA
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
     C     *LIKE         DEFINE    GZCO#         KKCO#
     C     *LIKE         DEFINE    GZZIPA        KKZIPA
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    ARRID         KKRID
     C     *LIKE         DEFINE    RCCPT4        KKCPT4
     C     *LIKE         DEFINE    RCEFDT        KKEFDT
     C*** BEGIN ADD *** 01/15/04 **************************************
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
     C*
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      QQPCT         XXPCT             2
     C                   Z-ADD     0010101       KKEFDT
     C                   Z-ADD     0             GZRCDS           10 0
     C                   Z-ADD     0             RCRCDS           10 0
     C*
     C*  Read through the FSAZTEMP file to populate the FMGZP file
     C*
     C                   READ      PAZ01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C*** BEGIN DEL *** 08/07/02 **************************************
     C**********           EXSR CLRGZR
     C*** END   DEL *** 08/07/02 **************************************
     C*
     C                   EXSR      UPDGZP
     C*
     C                   READ      PAZ01                                  99
     C*
     C                   ENDDO
     C*
     C*  Read through the FMGZP file to populate keyfields that will
     C*  ultimately be updating FMRCP file
     C*
     C                   MOVE      *HIVAL        KKZIPA
     C                   MOVE      *HIVAL        KKFRZP
     C     GZKY          SETGT     PGZ02
     C     GZKY1         READPE    PGZ02                                  98
     C     *IN98         DOWEQ     *OFF
     C*** Beg Add *** 06/12/16 ********************************************
     C                   eval      kkZipA = GZzipA
     C*** End Add *** 06/12/16 ********************************************
     C*
     C*  Read through the FSACTEMP file (CPT4 file)
     C*
     C     *LOVAL        SETLL     PAC01
     C                   READ      PAC01                                  97
     C     *IN97         DOWEQ     *OFF
     C*
     C     ACCPT4        IFGE      '00100'
     C     ACCPT4        ANDLE     '01999'
     C*
     C*** BEGIN DEL *** 08/07/02 **************************************
     C**********           EXSR CLRRCR
     C*** END   DEL *** 08/07/02 **************************************
     C*
     C                   EXSR      UPDRCP
     C*
     C                   ENDIF
     C*
     C                   READ      PAC01                                  97
     C                   ENDDO
     C*
     C                   READP     PGZ02                                  98
     C                   ENDDO
     C*
     C* End of Program
     C*
     C                   SETON                                        LR
     CLR                 EXCEPT    PRT
     C*** BEGIN DEL *** 08/07/02 **************************************
     C*================================================================
     C* CLRGZP - Clear the FMGZP record before writing it
     C*================================================================
     C********** CLRGZR    BEGSR
     C*
     C**********           CLEARGZCO#
     C**********           CLEARGZZIPA
     C**********           CLEARGZDESC
     C**********           CLEARGZFRZP
     C**********           CLEARGZTOZP
     C**********           CLEARGZSTAT
     C**********           CLEARGZUSER
     C**********           CLEARGZPGM
     C**********           CLEARGZTIME
     C**********           CLEARGZDT
     C**********           CLEARGZIUSR
     C**********           CLEARGZIDT
     C**********           CLEARGZCOVA
     C**********           CLEARGZRVS#
     C*
     C**********           ENDSR
     C*================================================================
     C* CLRRCP - Clear the FMRCP record before writing it
     C*================================================================
     C********** CLRRCR    BEGSR
     C*
     C**********           CLEARRCCO#
     C**********           CLEARRCCPT4
     C**********           CLEARRCEFDT
     C**********           CLEARRCDSC1
     C**********           CLEARRCANAM
     C**********           CLEARRCRPT
     C**********           CLEARRCSURG
     C**********           CLEARRCSUPP
     C**********           CLEARRCFSR
     C**********           CLEARRCSTAT
     C**********           CLEARRCUSER
     C**********           CLEARRCPGM
     C**********           CLEARRCTIME
     C**********           CLEARRCDT
     C**********           CLEARRCIUSR
     C**********           CLEARRCIDT
     C**********           CLEARRCZIPA
     C**********           CLEARRC1A2
     C**********           CLEARRCCONF
     C*
     C**********           ENDSR
     C*** END   DEL *** 08/07/02 **************************************
     C*================================================================
     C* GETCF - Get conversion factor depending on percentage
     C*================================================================
     C     GETCF         BEGSR
     C*
     C     XXPCT         IFEQ      '25'
     C                   Z-ADD     AF25P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '30'
     C                   Z-ADD     AF30P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '40'
     C                   Z-ADD     AF40P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '50'
     C                   Z-ADD     AF50P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '60'
     C                   Z-ADD     AF60P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '70'
     C                   Z-ADD     AF70P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '75'
     C                   Z-ADD     AF75P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '80'
     C                   Z-ADD     AF80P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '85'
     C                   Z-ADD     AF85P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '90'
     C                   Z-ADD     AF90P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     XXPCT         IFEQ      '95'
     C                   Z-ADD     AF95P         RCCONF
     C                   GOTO      GETCFE
     C                   ENDIF
     C*
     C     GETCFE        ENDSR
     C*================================================================
     C* UPDGZP - Update the FMGZP file
     C*================================================================
     C     UPDGZP        BEGSR
     C*** BEGIN ADD *** 08/07/02 **************************************
     C                   CLEAR                   PGZ01
     C*** END   ADD *** 08/07/02 **************************************
     C*
     C                   MOVEL     QQCO#         GZCO#
     C                   MOVEL     AZGZA         GZZIPA
     C                   MOVEL     AZDESC        GZDESC
     C                   MOVEL     AZSTRT        GZFRZP
     C                   MOVEL     AZEND         GZTOZP
     C*** BEGIN ADD *** 08/07/02 **************************************
     C                   Z-ADD     KKEFDT        GZEFDT
     C*** END   ADD *** 08/07/02 **************************************
     C                   MOVEL     'A'           GZSTAT
     C                   MOVEL     Q1USER        GZUSER
     C                   MOVEL     'FS900RA'     GZPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C                   Z-ADD     DSTIME        GZTIME
     C                   MOVEL     Q1USER        GZIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZIDT
     C                   MOVEL     AZCOVA        GZCOVA
     C                   MOVEL     AZRID         GZRVS#
     C*
     C                   ADD       1             GZRCDS
     C*
     C                   WRITE     PGZ01
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* UPDRCP - Update the FMRCP file
     C*================================================================
     C     UPDRCP        BEGSR
     C*** BEGIN ADD *** 08/07/02 **************************************
     C*** Beg Add *** 06/13/16 ****************************************
     C                   eval      kkCpt4 = ACcpt4
     C     rcKy1         Chain     pRC01                              99
     C                   if        *in99 = *on
     C*** End Add *** 06/13/16 ****************************************
     C                   CLEAR                   PRC01
     C*** Beg Add *** 06/13/16 ****************************************
     C                   Endif
     C*** End Add *** 06/13/16 ****************************************
     C*** END   ADD *** 08/07/02 **************************************
     C*
     C                   MOVEL     QQCO#         RCCO#
     C                   MOVEL     ACCPT4        RCCPT4
     C                   Z-ADD     KKEFDT        RCEFDT
     C                   MOVEL     ACDESC        RCDSC1
     C                   MOVEL     GZRVS#        KKRID
     C*** Beg Del *** 06/13/16 ****************************************
     C***********        MOVEL     ACCPT4        KKCPT4
     C*** End Del *** 06/13/16 ****************************************
     C     ARKY          CHAIN     PAR01                              53
     C  N53              Z-ADD     ARRELV        RCANAM
     C                   MOVEL     'N'           RCRPT
     C                   MOVEL     'N'           RCSURG
     C                   MOVEL     'N'           RCSUPP
     C*** BEG CHG *** 03/26/04 ****************************************
     C******************** MOVEL'N'       RCFSR
     C                   MOVEL     'Y'           RCFSR
     C*** END CHG *** 03/26/04 ****************************************
     C                   MOVEL     'A'           RCSTAT
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     'FS900RA'     RCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C                   Z-ADD     DSTIME        RCTIME
     C*** Beg Chg *** 06/13/16 ****************************************
     C**********         MOVEL     Q1USER        RCIUSR
     C   99              MOVEL     Q1USER        RCIUSR
     C*** End Chg *** 06/13/16 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** Beg Chg *** 06/13/16 ****************************************
     C*********          MOVEL     SRDTO         RCIDT
     C   99              MOVEL     SRDTO         RCIDT
     C*** End Chg *** 06/13/16 ****************************************
     C                   MOVEL     GZZIPA        RCZIPA
     C                   MOVEL     'Y'           RC1A2
     C*
     C*  The following populates RCCONF
     C*
     C                   MOVEL     GZZIPA        KKZIPA
     C     AFKY          CHAIN     PAF01                              54
     C  N54              EXSR      GETCF
     C*
     C                   ADD       1             RCRCDS
     C*
     C*** Beg Add *** 06/13/16 ****************************************
     C                   if        *in99 = *on
     C*** End Add *** 06/13/16 ****************************************
     C                   WRITE     PRC01
     C*** Beg Add *** 06/13/16 ****************************************
     C                   Else
     C                   Update    PRC01
     C                   Endif
     C*** End Add *** 06/13/16 ****************************************
     C*** BEGIN ADD *** 01/15/04 **************************************
     C*
     C* WRITE TO PERCENTILE FILE
     C*
     C     ##UCPF        IFEQ      'Y'
     C                   EXSR      WUPR1P
     C                   ENDIF
     C*** END   ADD *** 01/15/04 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/15/04 **************************************
     C*================================================================
     C* WUPR1P - Write/Update FMR1P record and after processing
     C*================================================================
     C     WUPR1P        BEGSR
     C*
     C                   CLEAR                   PR101
     C     R1KY          CHAIN     PR101                              99
     C*
     C                   Z-ADD     *ZEROS        R1MR50
     C                   Z-ADD     *ZEROS        R1MR60
     C                   Z-ADD     *ZEROS        R1MR70
     C                   Z-ADD     *ZEROS        R1MR75
     C                   Z-ADD     *ZEROS        R1MR80
     C                   Z-ADD     *ZEROS        R1MR85
     C                   Z-ADD     *ZEROS        R1MR90
     C                   Z-ADD     *ZEROS        R1MR95
     C*
     C     *IN54         IFEQ      *OFF
     C                   Z-ADD     AF50P         R1CV50
     C                   Z-ADD     AF60P         R1CV60
     C                   Z-ADD     AF70P         R1CV70
     C                   Z-ADD     AF75P         R1CV75
     C                   Z-ADD     AF80P         R1CV80
     C                   Z-ADD     AF85P         R1CV85
     C                   Z-ADD     AF90P         R1CV90
     C                   Z-ADD     AF95P         R1CV95
     C                   ENDIF
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
     O                                           80 'FS900RA'
     O          E            PRT            2
     O                                           27 'CLIENT NUMBER  . . . :'
     O                       QQCO#               40
     O          E            PRT            2
     O                                           27 'RCDS LOADED FOR FMRCP:'
     O                       RCRCDS        J     40
     O          E            PRT            2
     O                                           27 'RCDS LOADED FOR FMGZP:'
     O                       GZRCDS        J     40
     O          E            PRT            2
     O                                           18 'PERCENTILE =    %'
     O                       XXPCT               16
