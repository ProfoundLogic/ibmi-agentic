     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H* PROGRAM: F7090R01
     H* DESCRIPTION: CASE TRANSFER/DELETION PRE-CHECK PROGRAM
     H*
     H* DESCRIPTION: VERIFIES DATA IN FILES FOR THE CASE AND WRITE
     H*              THE FILE NAME TO THE WORK FILE TO PROCESS.
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  05/31/02 C:62  Support: 114MDC
     H*                 ADDED FDCHP FOR CASE TRANSFER/DELETION.
     H*
     H*  04/28/00 C:62  Support: AR001
     H*                 ADDED NMIMP FOR CASE TRANSFER/DELETION.
     H*
     H*  08/05/99 C:31  Support: 19079
     H*                 Transfer issue
     H*                 1. Add FDOSP
     H*
     H*  02/12/99 C:94 PROJECT: SUP16576
     H*                CHANGE LOGICAL FOR UMPCP FROM UMPCL10 TO
     H*                UMPCL11
     H*
     H*****************************************************************
     H*
     F*  Physical file definition (to load KFIELDS)
     FFMCMP     IF   E             DISK
     F*  Physical file definition (for refreshing subfile record)
     FFAT3P     IF   E             DISK
     F*
     FFAT3LQ    UF A E           K DISK
     F*
     F*** A/R FILES **************************************************
     F*
     FNMARLA    IF   E           K DISK
     F*** BEGIN ADD *** 04/28/00 **************************************
     FNMIML2    IF   E           K DISK
     F*** END   ADD *** 04/28/00 **************************************
     F*
     F*** CASE MANAGEMENT FILES **************************************
     F*
     FFADAL     IF   E           K DISK
     F*
     FFAMSL     IF   E           K DISK
     F*** BEGIN ADD *** 08/05/99 **************************************
     F*
     FFDOSL     IF   E           K DISK
     F*** END ADD *** 08/05/99 ****************************************
     F*
     FFAPPL1    IF   E           K DISK
     F*
     FFATXL     IF   E           K DISK
     F*
     FFAWHL     IF   E           K DISK
     F*
     FFATRL     IF   E           K DISK
     F*
     FFATDL     IF   E           K DISK
     F*
     FFATIL     IF   E           K DISK
     F*
     FNAMBL     IF   E           K DISK
     F*
     FNASBL     IF   E           K DISK
     F*
     FNDTML     IF   E           K DISK
     F*
     F*** NETWORK FILES **********************************************
     F*
     FNATRL     IF   E           K DISK
     F                                     RENAME(PTR01:PTR02)
     F*
     F*** UR FILES****************************************************
     F*
     FNAPBL     IF   E           K DISK
     F*
     F*** BEG CHG *** 02/12/99 ***************************************
     FUMPCL11   IF   E           K DISK
     F**********UMPCL10 IF  E           K        DISK
     F*** END CHG *** 02/12/99 ***************************************
     F*
     FUMCHL1    IF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     F*
     FUDPOL     IF   E           K DISK
     F*** BEGIN ADD *** 05/31/02 **************************************
     FFDCHL1    IF   E           K DISK
     F                                     RENAME(PCH01:PCH03)
     F*** END   ADD *** 05/31/02 **************************************
     F*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     T3KY          KLIST
     C                   KFLD                    KKFILE
     C*
     C     GENKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    TQFILE        KKFILE
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     @@RRN         CHAIN(N)  PCM01                              99
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
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
     C*** BEGIN ADD *** 05/31/02 **************************************
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
     C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
     C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
     C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C*** IS MC A/R INSTALLED
     C*
     C     ##AR          IFEQ      'Y'
     C                   MOVE      'A'           INSTA             1
     C                   ENDIF
     C*
     C*** IS NETWORK INSTALLED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   MOVE      'N'           INSTN             1
     C                   ENDIF
     C*
     C*** IS UR INSTALLED
     C*
     C     ##UR          IFEQ      'Y'
     C                   MOVE      'U'           INSTU             1
     C                   ENDIF
     C*
     C*** IS MED INSTALLED
     C*
     C     ##MED         IFEQ      'Y'
     C                   MOVE      'C'           INSTC             1
     C                   ENDIF
     C*** BEGIN ADD *** 05/31/02 **************************************
     C*
     C*** IS CODEREVIEW INSTALLED
     C*
     C     ##CRVW        IFEQ      'Y'
     C                   MOVE      'R'           INSTR             1
     C                   ENDIF
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C                   EXSR      READ
     C     *IN95         DOWNE     *ON
     C*
     C                   EXSR      SUBSET
     C*
     C                   EXSR      READ
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* AFILES - FILES THAT ARE A/R FILES
     C*================================================================
     C     AFILES        BEGSR
     C                   SELECT
     C*
     C     T3FILE        WHENEQ    'NMARP'
     C     GENKY         CHAIN     PAR01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*** BEGIN ADD *** 04/28/00 **************************************
     C     T3FILE        WHENEQ    'NMIMP'
     C     GENKY         CHAIN     PIM01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*** END   ADD *** 04/28/00 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CFILES - FILES THAT ARE CASE MANAGEMENT FILES
     C*================================================================
     C     CFILES        BEGSR
     C                   SELECT
     C*
     C     T3FILE        WHENEQ    'FADAP'
     C     GENKY         CHAIN     PDA01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAMSP'
     C     GENKY         CHAIN     PMS01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*** BEGIN ADD *** 08/05/99 **************************************
     C*
     C     T3FILE        WHENEQ    'FDOSP'
     C     GENKY         CHAIN     POS01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*** END ADD *** 08/05/99 ****************************************
     C*
     C     T3FILE        WHENEQ    'FAPPP'
     C     GENKY         CHAIN     PPP01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FATXP'
     C     GENKY         CHAIN     PTX01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAWHP'
     C     GENKY         CHAIN     PWH01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FATRP'
     C     GENKY         CHAIN     PTR01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FATDP'
     C     GENKY         CHAIN     PTD01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FATIP'
     C     GENKY         CHAIN     PTI01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'NAMBP'
     C     GENKY         CHAIN     PMB01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'NASBP'
     C     GENKY         CHAIN     PSB01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'NDTMP'
     C     GENKY         CHAIN     PTM01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* NFILES - FILES THAT ARE NETWORK FILES
     C*================================================================
     C     NFILES        BEGSR
     C                   SELECT
     C*
     C     T3FILE        WHENEQ    'NATRP'
     C     GENKY         CHAIN     PTR02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/31/02 **************************************
     C*================================================================
     C* RFILES - FILES THAT ARE CODEREVIEW FILES
     C*================================================================
     C     RFILES        BEGSR
     C*
     C                   SELECT
     C     T3FILE        WHENEQ    'FDCHP'
     C     GENKY         CHAIN     PCH03                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END   ADD *** 05/31/02 **************************************
     C*================================================================
     C* UFILES - FILES THAT ARE UR FILES
     C*================================================================
     C     UFILES        BEGSR
     C                   SELECT
     C*
     C     T3FILE        WHENEQ    'NAPBP'
     C     GENKY         CHAIN     PPB01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'UMPCP'
     C     GENKY         CHAIN     PPC01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'UMCHP'
     C     GENKY         CHAIN     PCH02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'UDPOP'
     C     GENKY         CHAIN     PPO01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBFL - Move data base fields to File fields in Qtemp
     C*================================================================
     C     MVDBFL        BEGSR
     C*
     C                   MOVEL(P)  T3FILE        KKFILE
     C     T3KY          CHAIN     PT302                              99
     C                   MOVEL(P)  T3FILE        TQFILE
     C                   MOVEL(P)  T3DESC        TQDESC
     C                   MOVEL(P)  T3NOTE        TQNOTE
     C     *IN99         IFEQ      *ON
     C                   WRITE     PT302
     C                   ELSE
     C                   UPDATE    PT302
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C                   READ      PT301                                  95
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   SETOFF                                       3099
     C*
     C                   SELECT
     C     T3PROD        WHENEQ    INSTA
     C                   EXSR      AFILES
     C     *IN30         IFEQ      *ON
     C                   EXSR      MVDBFL
     C                   ENDIF
     C*
     C     T3PROD        WHENEQ    INSTC
     C                   EXSR      CFILES
     C     *IN30         IFEQ      *ON
     C                   EXSR      MVDBFL
     C                   ENDIF
     C*
     C     T3PROD        WHENEQ    INSTN
     C                   EXSR      NFILES
     C     *IN30         IFEQ      *ON
     C                   EXSR      MVDBFL
     C                   ENDIF
     C*
     C     T3PROD        WHENEQ    INSTU
     C                   EXSR      UFILES
     C     *IN30         IFEQ      *ON
     C                   EXSR      MVDBFL
     C                   ENDIF
     C*** BEGIN ADD *** 05/31/02 **************************************
     C*
     C     T3PROD        WHENEQ    INSTR
     C                   EXSR      RFILES
     C     *IN30         IFEQ      *ON
     C                   EXSR      MVDBFL
     C                   ENDIF
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
