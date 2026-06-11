     H*****************************************************************
     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*****************************************************************
     H/TITLE N96DSR - DS discount Code - Medicare Hospital
     H*****************************************************************
     H*                                                               *
     H*  Programmer: C:198              Project: 114MWS               *
     H*  Created: 08/12/10                                            *
     H*****************************************************************
     H*  Revisions:
     H*
     H*  07/18/23  C:342 Project: NY060
     H*                  NY Compliance: CARC/RARC Update
     H*
     H*  05/30/16  C:309  Project: FS497
     H*                   Changed field ##si size to 2A
     H*
     H*  12/07/10  C:079 PROJ: 114MWS PRISS 32
     H*                  Get EOB Code Description Correctly
     H*
     H*
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALF    UF   E           K DISK    infds(cafds)
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDMGL6    UF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFTABL     IF   E           K DISK    usropn
     FFTEBLMC   IF   E           K DISK    rename(peb01:pebmc) prefix(mc:2)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 ds
     D  qqcmd                  1     56
     D  qqst                  40     41
     D*
     DCAFDS            DS
     DCARRN                  397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D  KKFL03         S              1    INZ(' ')
     D kkkey           S                   like(ebkey)
     Dqca              s             56    dim(2) ctdata perrcd(1)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@FDAT            8
     C                   PARM                    @@DSEQ            5
     C                   PARM                    @@CODE
     C                   PARM                    @@CEDT            8
     C                   PARM                    @@SYS             1
     C                   PARM                    @@FL1             1
     C                   PARM                    @@FL2             1
     C                   PARM                    @@FL3             1
     C                   PARM                    @@FL4             1
     C                   PARM                    @@FL5             1
     C                   PARM                    @@DS1            10
     C                   PARM                    @@DS2            10
     C                   PARM                    @@DS3            10
     C                   PARM                    @@DS4            10
     C                   PARM                    @@DS5            10
     C                   PARM                    @@RTCD            1
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
     C* BILLING CODE TABLE FILE
     C*
     C     BDKY          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C* CASE DETAIL RECORDS
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C* EOMB PHYS/HCFA DETAIL FILE
     C*
     C     DUKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C* EOMB TRANSACTION FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C* EOMB HOSP/UB HEADER FILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TABKEY           10
     C*
     C*  EOMB DETAIL EOB/NOTES TRANSACTION FILE
     C*
     C     DEKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    EORCN
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C*  PROV BILLS SPECIFIC ERRORS
     C*
     C     MGKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    CADSEQ
     C*
     C     ebky          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    kkkey
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@DSEQ        KKDSEQ
     C                   MOVEL(P)  @@CODE        KKCODE
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   Z-ADD     *ZEROS        XXMMRA            9 2
     C                   Z-ADD     0             HLADDT            7 7
     C                   Z-ADD     0             HLANMN           10 7
     C                   Z-ADD     0             HLANTV            3 0
     C                   Z-ADD     0             XXANTV            5 2
     C                   Z-ADD     0             XXWORK           10 7
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      '0'
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   exsr      OpnOverride
     C
     C*
     C                   SELECT
     C     EOSYS         WHENEQ    'H'
     C  N99DUKY          CHAIN     PDU01                              99
     C  N99EUKY          CHAIN     PEU01                              99
     C                   ENDSL                                                  End select
     C*
     C* BE SURE CLAIM STATUS CODE IS NOT "C" FOR CAP ON EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              99
     C     *IN99         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C* FOR EACH DETAIL RECORD READ, FIND THE DISCOUNT MASTER RECORD
     C* THAT APPLYS TO THAT DATE OF SERVICE.
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C*
     C     BDKY          SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  99
     C*
     C* VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN99         IFEQ      '0'
     C     BDCODE        IFNE      KKCODE
     C     BDSTAT        OREQ      'I'
     C                   GOTO      NXTBD
     C                   ENDIF                                                  BDCODE/BDSTAT
     C                   ENDIF                                                  *IN99 OFF
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE
     C*
     C                   MOVEL(P)  EUBLTY        XXBLTY            2
     C                   SETOFF                                       90
     C*
     C     XXBLTY        IFEQ      '13'
     C     XXBLTY        OREQ      '14'
     C     XXBLTY        OREQ      '23'
     C     XXBLTY        OREQ      '33'
     C     XXBLTY        OREQ      '74'
     C     XXBLTY        OREQ      '75'
     C     XXBLTY        OREQ      '84'
     C                   MOVEL(P)  'O'           KKFL03
     C                   ELSE
     C     XXBLTY        IFEQ      '83'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL(P)  'I'           KKFL03
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF INPATIENT/PER DIEM, THEN PAY PER DIEM FLAT RATE.            LE
     C*
     C     KKFL03        IFEQ      'I'
     C*
     C                   Z-ADD     *zeros        CAPAID
     C     CACHRG        SUB       CAPAID        CANDIS
     C     CANDIS        SUB       CAMRA         CANDIS
     C*
     C                   ELSE
     C*
     C* IF OUTPATIENT/% OF CHARGES, THEN PAY PERCENTAGE OF CHARGED.
     C                   move      carrn         ##rrn
     C*
     C                   call      'FA762R'
     C                   parm                    ##rrn             9 0
     C                   parm      'N'           ##call            1
     C***** Beg Chg *** 05/30/16 *********************************
     C*****              parm                    ##si              1
     C                   parm                    ##si              2
     C***** End Chg *** 05/30/16 *********************************
     C                   parm                    ##eob             3
     C                   parm                    ##msg             7
     C                   parm                    ##modr            1
     C                   parm                    ##alow            9 2
     C                   parm                    ##rtcd            1
     C                   parm                    ##rsch            9 2
     C                   parm                    ##outl            9 2
     C*
     C                   eval      capaid = caunts * ##alow *bdpc02
     C*
     C     CAPAID        IFGE      CACHRG
     C                   Z-ADD     CACHRG        CAPAID
     C                   endif
     C*
     C     cachrg        SUB       capaid        CANDIS
     C     candis        SUB       camra         CANDIS
     C*
     C*
     C                   if        capaid = 0
     C                   movel     'F'           @@rtcd
     C                   endif
     C*
     C                   ENDIF                                                  kkfl03 = 'I'
     C*
     C                   Z-ADD     ##rsch        CArsch
     C                   Z-ADD     ##outl        CAoutl
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C*
     C                   EXSR      REMMSG
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        CANDIS
     C*
     C                   ENDIF                                                  *in99 = *off
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
     C     CANDIS        IFNE      *ZEROS
     C*
     C* GET NEXT DETAIL SEQUENCE NUMBER
     C*
     C                   EXSR      WRTEOB
     C*
     C                   UPDATE    PCA01
     C*
     C                   ENDIF                                                   CANDIS > 0
     C*
     C                   ENDIF                                                   CLM STS CD NE 'C'
     C                   ENDIF                                                   END *IN99
     C*
     C     TERM          TAG
     C*
     C                   exsr      ClsOverride
     C*
     C                   MOVE      '1'           *INLR
    C*================================================================
    C*  clsOverride - Close and Delter Override FTABL
    C*================================================================
     C     clsOverride   begsr
     C*
     C                   movel(p)  qca(2)        qqcmd
     C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length           15 5
     C*
     C                   close     ftabl
     C*
     C                   endsr
    C*================================================================
    C*  opnOverride - Open and Override FTABL
    C*================================================================
     C     opnOverride   begsr
     C*
     C* open up FTEBPxx to calculate find eob code desc
     C                   movel(p)  qca(1)        qqcmd
     C                   movel     eoast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length           15 5
     C*
     C                   open      ftabl
     C*
     C                   endsr
    C*================================================================
     C*  Remove  - Remove adjudication messages/EOMB Codes  R E M M S G
    C*================================================================
     C     REMMSG        BEGSR
     C*
     C*
     C     DEKY          CHAIN     PDE01                              99
     C                   dow       *in99 = *off
     C                   if        deesrc = 'A'
     C     MGKY          CHAIN     PMG01                              99
     C                   dow       *in99 = *off
     C     MGSRC         IFEQ      'S'
     C                   MOVE      MGMSG#        XXMSG#            5
     C     XXMSG#        IFNE      '00003'
     C     MGDSEQ        ANDEQ     DEDSEQ
     C                   DELETE    PMG01
     C                   ENDIF
     C                   ENDIF                                                  SRC = 'S'
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C                   ENDIF                                                  deesrc = 'A'
     C*
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C* Remove adjudication EOMB codes from Detail EOB/NOTES
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C     DEDSEQ        ANDEQ     CADSEQ
     C                   DELETE    PDE01
     C                   ENDIF                                                  ESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C                   ENDSR
    C*================================================================
     C*  WRTEOB  - Write EOMB Codes
    C*================================================================
     C     wrteob        BEGSR
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PDE01
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C                   IF        KKFL03 = 'I'
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      '394'         DEEOB
     C                   MOVEL     '394'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   ELSE
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      ##EOB         DEEOB
     C                   MOVEL     ##EOB         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   ENDIF
     C*** Get EOB desc from FTEBPMC or FTABL
     C*** BEG CHG *** 12/07/10 *********************************************
     C*****************  movel     ##eob         kkkey
     C**** Beg Chg *** 07/18/23 **************************************
     C****               movel     deeob         kkkey
     C                   movel     deeobcd       kkkey
     C**** End Chg *** 07/18/23 **************************************
     C*** END CHG *** 12/07/10 *********************************************
     C*                  if        @@mode = 'N'
     C                   if        eodsty = 'DR' or eodsty = 'DS'
     C     ebky          chain     pebmc                              99
     C  N99              MOVEL(P)  mcdesc        DENOT1
     C                   else
     C     ebky          chain     peb01
     C  N99              MOVEL(P)  ebdesc        DENOT1
     C                   endif
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'N'           DEESRC
     C                   Z-ADD     CARCN         DERCN
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   WRITE     PDE01
     C                   endsr
     C*
**  TABLE QCA
OVRDBF FILE(FTEBP)   TOFILE(*LIBL/FTEBP  )
DLTOVR FILE(FTEBP)
