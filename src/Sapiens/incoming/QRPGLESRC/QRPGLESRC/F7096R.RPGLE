     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7096R - CASE TRANSFER/DELETION PROGRAM
     H*****************************************************************
     H* DESCRIPTION: CASE TRANSFER/DELETION - WRITE ALL FILES
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20  C:295 Project : SY420                              *
     H*                  Mask SSN (Recompile only)                   *
     h*                                                               *
     H*  01/17/01 C:62  PROJECT: MC098 25227
     H*                 CORRECT THE TRANSFER OF FDOSP
     H*
     H*  04/28/00 C:62  PROJECT: AR001
     H*                 TRANSFER/DELETE NMIMP.
     H*
     H*  02/10/00 C:62  PROJECT: MC151
     H*                 -REMOVE FILE TRANSFERS AND PLACE THEM IN
     H*                  F7096R01 (NAPBP, NATRP AND NMARP).
     H*                 -ADDED CALL TO FA704R TO CHANGE DEMOGRAPHIC
     H*                  INFORMATION IN OTHER CASES AND EOMBS.
     H*                 -CHANGED UPDATE OF NDTMP TO ONLY UPDATE THE
     H*                  MSEQ NUMBER, ALSO FIXED IT SO THAT IT WOULD
     H*                  WORK.
     H*
     H*  02/02/00 C:53  PROJECT: MC221
     H*                 REMOVE UPDATE TO EOUSER.
     H*
     H*  11/19/99 C:31  Project: MC151
     H*                 Claim Transfer/Deletion
     H*                 1. Delay heading print until record processed
     H*
     H*  08/05/99 C:31  Support: 19079
     H*                 Transfer issue
     H*                 1. Add FDOSP
     H*                 2. Change FP703R call to FA703R
     H*
     H*  02/08/98  C:94  PROJECT: 16448
     H*                  CHANGE PROGRAM TO NOT RESEQUENCE FAMSP IF
     H*                  CHANGING INFORMATION TO CASE *IN98 = OFF
     H*                  ADD CALL TO SC900R01 IN THE MIDDLE OF
     H*                  UPDATING FAMSP SO THAT DARTS WILL HAVE THE
     H*                  CORRECT KEY
     H*               2. ADD UPDATE TO NDTMP INSIDE OF FAMSP SO THAT
     H*                  SEQ# IS UPDATE ACCORDINGLY.
     H*
     H*  11/30/98  C:94  PROJECT: SUP15246
     H*                  CHANGED API FOR TO INFORMATION ON REPORT
     H*
     H*****************************************************************
     H*
     H* INDICATORS:
     H*
     H* *IN50 = *ON  - FILE IS TO BE TRANSFERRED/DELETED
     H* *IN51 = *ON  - T1STAT = 'T' (TRANSFER)
     H* *IN51 = *OFF - T1STAT = 'D' (DELETE) IN SOME PLACES (ELSE).
     H* *IN53 = *ON  - T1STAT = 'T' (TRANSFER)
     H* *IN60 = *ON  - T1STAT = 'D' (DELETE) IN OTHER PLACES.
     H* *IN98 = *OFF - @@FLAG = 'X' (SINGLE CASE TRANSFER)
     H* *IN98 = *ON  - @@FLAG <> 'X' (DOUBLE CASE TRANSFER)
     H*
     H*****************************************************************
     H*
     FFACTP     IF   E             DISK
     F*
     FFAT1L     IF   E           K DISK
     F*
     FFAT3L     IF   E           K DISK
     F*
     FFMCDL     UF   E           K DISK
     F*
     F*** BEGIN CHG *** 02/10/00 **************************************
     F**********FMCMP   IF  E                    DISK
     FFMCMP     UF   E             DISK
     F*** END   CHG *** 02/10/00 **************************************
     F*** BEGIN DEL *** 02/10/00 **************************************
     F**********  PCM01                             KRENAMEPCM02
     F*
     F**********FMCML1  UF  E           K        DISK
     F*** END   DEL *** 02/10/00 **************************************
     F*
     FFMCHL1    UF   E           K DISK
     F*
     FNMARLA    UF   E           K DISK
     F*** BEGIN ADD *** 04/28/00 **************************************
     F*
     FNMIML2    UF   E           K DISK
     F*** END   ADD *** 04/28/00 **************************************
     F*
     FFADAL     UF   E           K DISK
     F*
     FFAMSL8    UF   E           K DISK
     F*
     FFAMSL3    IF   E           K DISK
     F                                     RENAME(PMS01:PMS02)
     F*
     FFAPPL1    UF   E           K DISK
     F*
     FFATXL     UF   E           K DISK
     F*
     FFAWHL     UF   E           K DISK
     F*
     FFATRL     UF   E           K DISK
     F*
     FFATDL     UF   E           K DISK
     F*
     FFATIL     UF   E           K DISK
     F*
     FFAERL     UF   E           K DISK
     F*
     F*** BEGIN DEL *** 02/10/00 **************************************
     F**********NATRL   UF  E           K        DISK
     F**********  PTR01                             KRENAMEPTR02
     F*
     F**********NAPBL   UF  E           K        DISK
     F*** END   DEL *** 02/10/00 **************************************
     F*
     FFDEOL     UF   E           K DISK
     F*
     FFDE1L1    UF   E           K DISK
     F*
     FFADCL     UF   E           K DISK
     F*
     FFDCALD    UF   E           K DISK
     F*
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*
     FFALTL     UF   E           K DISK
     F*
     FUMPCL11   UF   E           K DISK
     F*
     FUMCHL1    UF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     F*
     FUDPOL     UF   E           K DISK
     F*
     F*** BEG ADD *** 02/08/99 ***************************************
     F*
     FSMUPL2    IF   E           K DISK
     F*
     FNDTML6    UF   E           K DISK
     F*
     F*** END ADD *** 02/08/99 ***************************************
     F*** BEGIN ADD *** 08/05/99 **************************************
     FFDOSL2    UF   E           K DISK
     F*** END ADD *** 08/05/99 ****************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*** BEGIN DEL *** 02/10/00 **************************************
     D**********PTR02
     D**********    TRCO#                           T2CO#
     D**********    TRSUB#                          T2SUB#
     D**********    TRFND                           T2FND
     D**********    TRFYR                           T2FYR
     D**********    TRCASE                          T2CASE
     D**********    TRUSER                          T2USER
     D**********    TRTIME                          T2TIME
     D**********    TRPGM                           T2PGM
     D**********    TRDT                            T2DT
     D*** END   DEL *** 02/10/00 **************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     IPCH02
     I              CHCO#                       C2CO#
     I              CHSUB#                      C2SUB#
     I              CHFND                       C2FND
     I              CHFYR                       C2FYR
     I              CHCASE                      C2CASE
     I              CHUSER                      C2USER
     I              CHTIME                      C2TIME
     I              CHPGM                       C2PGM
     I              CHDT                        C2DT
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C********             PARM           @1RRN   90       FROM FACTP
     C                   PARM                    @1RRN            15 5          FROM FACTP
     C                   PARM                    @@TD              1            'T' OR 'D'
     C********             PARM           @1KEY   90       FROM T1KEY
     C                   PARM                    @1KEY            15 5          FROM T1KEY
     C                   PARM                    @@FLAG            1
     C********             PARM           @1RRN3  90
     C                   PARM                    @1RRN3           15 5
     C********             PARM           @1RRN2  90
     C                   PARM                    @1RRN2           15 5
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*** BEG ADD *** 02/08/99 ***************************************
     C*
     C     MUKY          KLIST
     C                   KFLD                    KKUSER
     C*
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    MSMSCD
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           KFLD           MSSEQ#
     C                   KFLD                    SVSEQ#
     C*** END   CHG *** 02/10/00 **************************************
     C*** END ADD *** 02/08/99 ***************************************
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CDKY1         KLIST
     C                   KFLD                    K1SS#
     C*
     C     GENKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     GENKY1        KLIST
     C                   KFLD                    K1CO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1CASE
     C*
     C     T1KY          KLIST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKFILE
     C*
     C     T3KY          KLIST
     C                   KFLD                    KKFILE
     C*
     C     EOBKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     FENKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKDCS#
     C*** BEGIN ADD *** 08/05/99 **************************************
     C*
     C     OSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           KFLD           KKMSEQ
     C                   KFLD                    SVSEQ#
     C*** END   CHG *** 02/10/00 **************************************
     C     *LIKE         DEFINE    OSMSCD        KKMSCD
     C     *LIKE         DEFINE    OSSEQ#        KKMSEQ
     C*** BEGIN ADD *** 02/10/00 **************************************
     C     *LIKE         DEFINE    MSSEQ#        SVSEQ#
     C*** END   ADD *** 02/10/00 **************************************
     C*** END ADD *** 08/05/99 ****************************************
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMPGM         SVFILE
     C     *LIKE         DEFINE    CMPGM         OOFILE
     C     *LIKE         DEFINE    TRTX#         XXTX#
     C     *LIKE         DEFINE    T1FILE        KKFILE
     C     *LIKE         DEFINE    T1KEY         KKKEY
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMCO#         K1CO#
     C     *LIKE         DEFINE    CMSUB#        K1SUB#
     C     *LIKE         DEFINE    CMFND         K1FND
     C     *LIKE         DEFINE    CMFYR         K1FYR
     C     *LIKE         DEFINE    CMCASE        K1CASE
     C     *LIKE         DEFINE    CMSS#         K1SS#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    DCFEIN        KKFEIN
     C     *LIKE         DEFINE    DCMOD         KKMOD
     C     *LIKE         DEFINE    DCDCS#        KKDCS#
     C     *LIKE         DEFINE    DCFEIN        K1FEIN
     C     *LIKE         DEFINE    DCMOD         K1MOD
     C     *LIKE         DEFINE    DCDCS#        K1DCS#
     C*** BEG ADD *** 02/08/99 ***************************************
     C     *LIKE         DEFINE    UPUSER        KKUSER
     C     *LIKE         DEFINE    TMMSEQ        KKSEQ#
     C*** END ADD *** 02/08/99 ***************************************
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C*** BEG ADD *** 02/15/99 ***************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @#WCIF            1            WC INST
     C                   PARM                    @#MCIF            1            MC INST
     C                   PARM                    @#WCIN            1            WC INTERFACE
     C                   PARM                    @#AR              1            MC AR INST
     C                   PARM                    @#NTWK            1            NETWORK INST
     C                   PARM                    @#UR              1            PRECERT INST
     C                   PARM                    @#MED             1            MEDMGMT INST
     C                   PARM                    @#EDI             1            EDI INST
     C                   PARM                    @#WCDK            1            WC DARTS KEY
     C                   PARM                    @#RHAB            1            REHAB INST
     C                   PARM                    @#MSS             1            MS SUPPORT
     C                   PARM                    @#FSCH            1            FEE SCHEDULE
     C                   PARM                    @#PRVB            1            PROVIDER BILLS
     C*** END ADD *** 02/15/99 ***************************************
     C                   MOVE      *ON           *INOA
     C*
     C                   Z-ADD     @1KEY         @@KEY             9 0
     C                   Z-ADD     @1RRN         @@RRN             9 0
     C                   Z-ADD     @1RRN2        @@RRN2            9 0
     C                   Z-ADD     @1RRN3        @@RRN3            9 0
     C*
     C** Get standard Print headings
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** @@RRN2    CHAINPCM02               N99
     C     @@RRN2        CHAIN(N)  PCM01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          OOHD1            78
     C                   MOVEL     #1L2          OOHD2            78
     C*
     C*** BEG ADD *** 11/30/98 ***************************************
     C     @@FLAG        IFNE      'X'
     C*** END ADD *** 11/30/98 ***************************************
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** @@RRN3    CHAINPCM02               N99
     C     @@RRN3        CHAIN(N)  PCM01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          OOHD3            78
     C                   MOVEL     #1L2          OOHD4            78
     C*** BEG ADD *** 11/30/98 ***************************************
     C                   ENDIF
     C*** END ADD *** 11/30/98 ***************************************
     C*
     C     @@RRN         CHAIN     PCT01                              99
     C                   Z-ADD     CTFCO#        KKCO#
     C                   Z-ADD     CTFSUB        KKSUB#
     C                   Z-ADD     CTFFND        KKFND
     C                   Z-ADD     CTFFYR        KKFYR
     C                   Z-ADD     CTFCAS        KKCASE
     C                   MOVEL(P)  CTFSS#        KKSS#
     C                   Z-ADD     CTTCO#        K1CO#
     C                   Z-ADD     CTTSUB        K1SUB#
     C                   Z-ADD     CTTFND        K1FND
     C                   Z-ADD     CTTFYR        K1FYR
     C                   Z-ADD     CTTCAS        K1CASE
     C                   MOVEL(P)  CTTSS#        K1SS#
     C*** BEG ADD *** 11/30/98 ****************************************
     C     @@FLAG        IFEQ      'X'
     C                   MOVE      K1CO#         #1CO#
     C                   MOVE      K1SUB#        #1SUB#
     C                   MOVE      K1FND         #1FND
     C                   MOVE      K1FYR         #1FYR
     C                   MOVE      K1CASE        #1CASE
     C                   MOVEL(P)  CTTSS#        #1SS#
     C                   MOVEL(P)  CTTLSN        #1LSN
     C                   MOVEL(P)  CTTFSN        #1FSN
     C                   MOVEL(P)  CTTMID        #1MID
     C                   Z-ADD     CTTACD        #1DOI
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA721R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1LSN            30
     C                   PARM                    #1FSN            15
     C                   PARM                    #1MID             1
     C                   PARM                    #1SS#             9
     C                   PARM                    #1DOI             7 0
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          OOHD3            78
     C                   MOVEL     #1L2          OOHD4            78
     C                   ENDIF
     C*** END ADD *** 11/30/98 ****************************************
     C*
     C                   Z-ADD     @@KEY         KKKEY
     C*
     C     @@FLAG        IFEQ      'X'
     C                   MOVE      *OFF          *IN98
     C                   ELSE
     C                   MOVE      *ON           *IN98
     C                   ENDIF
     C*
     C                   SETOFF                                       53
     C                   Z-ADD     *ZEROS        XXCNT
     C*
     C                   EXSR      MOVEFI
     C*
     C*                    EXCPTTOT
     C*** BEG ADD *** 02/08/99 ***************************************
     C*
     C                   MOVE      *BLANK        QQRTCD
     C*
     C     @#WCIF        IFEQ      'Y'
     C     @#WCIN        ANDEQ     'Y'
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'Y'
     C                   MOVE      'Y'           ALLOW             1
     C                   ELSE
     C                   MOVE      'N'           ALLOW
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      'Y'           ALLOW
     C                   ENDIF
     C*
     C     ALLOW         IFEQ      'Y'
     C*
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      CTFCO#        #1CO#
     C                   MOVE      CTFSUB        #1SUB#
     C                   MOVE      CTFFND        #1FND
     C                   MOVE      CTFFYR        #1FYR
     C                   MOVE      CTFCAS        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    @#WCIN            1
     C                   PARM                    @#WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY1           40
TEMP C                   PARM                    @@PGM            10
     C*
     C                   MOVE      CTTCO#        #1CO#
     C                   MOVE      CTTSUB        #1SUB#
     C                   MOVE      CTTFND        #1FND
     C                   MOVE      CTTFYR        #1FYR
     C                   MOVE      CTTCAS        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    @#WCIN            1
     C                   PARM                    @#WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY2           40
TEMP C                   PARM                    @@PGM            10
     C*
     C                   CALL      'SC900C'
     C                   PARM      ##KEY1        QFKEY            40
     C                   PARM      ##KEY2        QTKEY            40
     C                   PARM      *BLANKS       QQQUE            10
     C                   PARM      'B'           QQMODE            1
     C                   PARM      ##RTCD        QQRTCD            1
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 02/08/99 ***************************************
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* MOVEFI - FILES THAT ARE BEING UPDATED
     C*================================================================
     C     MOVEFI        BEGSR
     C*
     C*** FILE FMCDP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FMCDP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C*
     C     *IN50         IFEQ      *ON
     C*
     C     *IN98         IFEQ      *OFF
     C     CDKY          CHAIN     PCD01                              99
     C                   ELSE
     C     CDKY1         CHAIN     PCD01                              99
     C                   ENDIF                                                  @@FLAG = X
     C*
     C     *IN99         IFEQ      *OFF
     C     *IN51         IFEQ      *ON
     C     *IN98         ANDEQ     *OFF
     C                   MOVEL     CTTSS#        CDSS#
     C                   MOVEL     CTTLSN        CDLSN
     C                   MOVEL     CTTFSN        CDFSN
     C                   MOVEL     CTTMID        CDMID
     C                   MOVEL     CTTAD1        CDADR1
     C                   MOVEL     CTTAD2        CDADR2
     C                   MOVEL     CTTCTY        CDCTY
     C                   MOVEL     CTTST         CDST
     C                   MOVEL     CTTZIP        CDZIP
     C                   MOVEL     CTTDOB        CDDTOB
     C                   MOVEL     CTTSEX        CDSEX
     C                   ADD       1             OOCNT             4 0
     C                   ADD       1             XXCNT             6 0
     C                   UPDATE    PCD01                                        FMCDP
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   CALL      'FA704R'
     C                   PARM      CDSS#         ##SS#             9
     C                   PARM      CDLSN         ##LSN            30
     C                   PARM      CDFSN         ##FSN            15
     C                   PARM      CDMID         ##MID             1
     C*** END   ADD *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN51
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  *IN50
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE FMCMP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FMCMP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C*
     C     *IN50         IFEQ      *ON
     C*
     C     *IN98         IFEQ      *OFF
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPCM01
     C********** GENKY     READEPCM01                    99
     C     @@RRN2        CHAIN     PCM01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY1    SETLLPCM01
     C********** GENKY1    READEPCM01                    99
     C     @@RRN3        CHAIN(N)  PCM01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** *IN99     DOWEQ*OFF
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*** BEGIN ADD *** 02/10/00 **************************************
     C     *IN98         IFEQ      *OFF
     C*** END   ADD *** 02/10/00 **************************************
     C                   Z-ADD     CTTCO#        CMCO#
     C                   Z-ADD     CTTSUB        CMSUB#
     C                   Z-ADD     CTTFND        CMFND
     C                   Z-ADD     CTTFYR        CMFYR
     C                   Z-ADD     CTTCAS        CMCASE
     C                   MOVEL(P)  CTTSS#        CMSS#
     C                   Z-ADD     CTTACD        CMACDT
     C                   Z-ADD     CTTEMP        CMEMP#
     C                   Z-ADD     CTTDIV        CMDIV
     C                   MOVEL(P)  CTTLSN        CMLSN
     C                   MOVEL(P)  CTTFSN        CMFSN
     C                   MOVEL(P)  CTTMID        CMMID
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 02/10/00 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVEL     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C*** FILE FMCHP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FMCHP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C*
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PCH01
     C     GENKY         READE     PCH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        CHCO#
     C                   Z-ADD     CTTSUB        CHSUB#
     C                   Z-ADD     CTTFND        CHFND
     C                   Z-ADD     CTTFYR        CHFYR
     C                   Z-ADD     CTTCAS        CHCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CHUSER
     C                   MOVEL     Q1PGM         CHPGM
     C                   MOVEL     DSTIME        CHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHDT
     C*
     C                   UPDATE    PCH01
     C*
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPCH01                97
     C                   DELETE    PCH01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PCH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*** BEGIN ADD *** 04/28/00 **************************************
     C*
     C*** FILE NMIMP
     C*
     C                   MOVEL(P)  'NMIMP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PIM01
     C     GENKY         READE     PIM01                                  99
     C     *IN99         DOWEQ     *OFF
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        IMCO#
     C                   Z-ADD     CTTSUB        IMSUB#
     C                   Z-ADD     CTTFND        IMFND
     C                   Z-ADD     CTTFYR        IMFYR
     C                   Z-ADD     CTTCAS        IMCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        IMUSER
     C                   MOVEL     Q1PGM         IMPGM
     C                   MOVEL     DSTIME        IMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IMDT
     C*
     C                   UPDATE    PIM01
     C*
     C                   ELSE
     C*
     C     GENKY         DELETE    PIM01                              97
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PIM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*** END   ADD *** 04/28/00 **************************************
     C*
     C*** FILE NMARP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'NMARP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PAR01
     C     GENKY         READE     PAR01                                  99
     C     *IN99         DOWEQ     *OFF
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        ARCO#
     C                   Z-ADD     CTTSUB        ARSUB#
     C                   Z-ADD     CTTFND        ARFND
     C                   Z-ADD     CTTFYR        ARFYR
     C                   Z-ADD     CTTCAS        ARCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVEL     DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   UPDATE    PAR01
     C*
     C                   ELSE
     C*
     C     GENKY         DELETE    PAR01                              97
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE FADAP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FADAP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PDA01
     C     GENKY         READE     PDA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        DACO#
     C                   Z-ADD     CTTSUB        DASUB#
     C                   Z-ADD     CTTFND        DAFND
     C                   Z-ADD     CTTFYR        DAFYR
     C                   Z-ADD     CTTCAS        DACASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DAUSER
     C                   MOVEL     Q1PGM         DAPGM
     C                   MOVEL     DSTIME        DATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DADT
     C*
     C                   UPDATE    PDA01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDA01                97
     C                   DELETE    PDA01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PDA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE FAMSP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FAMSP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY1        SETGT     PMS02
     C     GENKY1        READPE    PMS02                                  99
     C*
     C     *IN99         IFEQ      '0'
     C   98              Z-ADD     MSSEQ#        HLSEQ#
     C                   ELSE
     C                   Z-ADD     0             HLSEQ#            5 0
     C                   ENDIF
     C*
     C     GENKY         SETLL     PMS01
     C     GENKY         READE     PMS01                                  99
     C********** GENKY     CHAINPMS01                99
     C     *IN99         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 02/08/99 ***************************************
     C     *IN98         IFEQ      *ON
     C     *IN51         ANDEQ     *ON
     C*
     C* OLD INFORMATION
     C*
     C                   MOVE      MSCO#         @@CO#
     C                   MOVE      MSSUB#        @@SUB#
     C                   MOVE      MSFND         @@FND
     C                   MOVE      MSFYR         @@FYR
     C                   MOVE      MSCASE        @@CASE
     C*** BEGIN CHG *** 08/05/99 **************************************
     C********             MOVE MSMSCD    @@MSCD
     C                   MOVE      MSMSCD        @@MSCD            4
     C********             MOVE MSSEQ#    @@SEQ#
     C                   MOVE      MSSEQ#        @@SEQ#            5
     C*** END CHG *** 08/05/99 ****************************************
     C                   MOVE      MSSEQ#        KKSEQ#
     C*** BEGIN ADD *** 08/05/99 **************************************
     C                   MOVE      MSMSCD        KKMSCD
     C                   MOVE      MSSEQ#        KKMSEQ
     C*** END ADD *** 08/05/99 ****************************************
     C*
     C* NEW INFORMATION
     C*
     C                   MOVE      CTTCO#        #@CO#
     C                   MOVE      CTTSUB        #@SUB#
     C                   MOVE      CTTFND        #@FND
     C                   MOVE      CTTFYR        #@FYR
     C                   MOVE      CTTCAS        #@CASE
     C*** BEGIN CHG *** 08/05/99 **************************************
     C********             MOVE MSMSCD    #@MSCD
     C                   MOVE      MSMSCD        #@MSCD            4
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ENDIF
     C*** END ADD *** 02/08/99 ***************************************
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        MSCO#
     C                   Z-ADD     CTTSUB        MSSUB#
     C                   Z-ADD     CTTFND        MSFND
     C                   Z-ADD     CTTFYR        MSFYR
     C                   Z-ADD     CTTCAS        MSCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1PGM         MSPGM
     C                   MOVEL     DSTIME        MSTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C*
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   Z-ADD     MSSEQ#        SVSEQ#
     C*** END   ADD *** 02/10/00 **************************************
     C*** BEG CHG *** 02/08/99 ***************************************
     C   98              ADD       1             HLSEQ#
     C   98              Z-ADD     HLSEQ#        MSSEQ#
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             MOVE HLSEQ#    #@SEQ#
     C   98              MOVE      HLSEQ#        #@SEQ#            5
     C*** END CHG *** 08/05/99 ****************************************
     C***98*****           ADD  1         HLSEQ#
     C***98*****           Z-ADDHLSEQ#    MSSEQ#
     C*** END CHG *** 02/08/99 ***************************************
     C*
     C                   UPDATE    PMS01
     C*
     C*** BEG ADD *** 02/08/99 ***************************************
     C     *IN98         IFEQ      *ON
     C*
     C* GET THE OLD KEY FOR THE DARTS
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C********             CALL 'FP703R'
     C                   CALL      'FA703R'
     C*** END CHG *** 08/05/99 ****************************************
     C                   PARM      'K'           @@RTCD            1
     C                   PARM      @#WCIN        @@WCIN            1
     C                   PARM      @#WCDK        @@WCDK            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C*** BEGIN DEL *** 08/05/99 **************************************
     C********             PARM           @@MSCD  4
     C********             PARM           @@SEQ#  5
     C*** END DEL *** 08/05/99 ****************************************
     C                   PARM                    OLDKEY           40            OLD KEY
     C*
     C* GET THE NEW KEY FOR THE DARTS
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C********             CALL 'FP703R'
     C                   CALL      'FA703R'
     C*** END CHG *** 08/05/99 ****************************************
     C                   PARM      'K'           #@RTCD            1
     C                   PARM      @#WCIN        #@WCIN            1
     C                   PARM      @#WCDK        #@WCDK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM                    #@FYR             3
     C                   PARM                    #@CASE            7
     C*** BEGIN CHG *** 08/05/99 **************************************
     C********             PARM           @@MSCD  4
     C********             PARM           @@SEQ#  5
     C*** END CHG *** 08/05/99 ****************************************
     C                   PARM                    NEWKEY           40            NEW KEY
     C*
     C                   MOVEL(P)  Q1USER        KKUSER
     C     MUKY          CHAIN     PUP01                              99
     C                   MOVE      UPOUTQ        ##OUTQ           10
     C                   MOVE      'D'           ##MODE            1
     C*
     C                   CALL      'SC900R01'
     C                   PARM      OLDKEY        QQFKEY           40
     C                   PARM      NEWKEY        QQTKEY           40
     C                   PARM      ##OUTQ        QQQUE            10
     C                   PARM      ##MODE        QQMODE            1
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/05/99 **************************************
     C**************************************************************
     C*** FILE FDOSP
     C**************************************************************
     C                   MOVE      ' '           UPNDT1
     C                   MOVEL(P)  'FDOSP'       KKFILE
     C                   EXSR      UPDATE
     C*
     C     *IN50         IFEQ      *ON
     C*** BEGIN ADD *** 01/17/01 **************************************
     C                   MOVEL(P)  MSMSCD        KKMSCD
     C*** END   ADD *** 01/17/01 **************************************
     C*** BEGIN CHG *** 01/17/01 **************************************
     C********** OSKY      SETLLPOS01
     C********** OSKY      READEPOS01                    99
     C     OSKY          CHAIN     POS01                              99
     C*** END   CHG *** 01/17/01 **************************************
     C*
     C     *IN99         DOWEQ     *OFF
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        OSCO#
     C                   Z-ADD     CTTSUB        OSSUB#
     C                   Z-ADD     CTTFND        OSFND
     C                   Z-ADD     CTTFYR        OSFYR
     C                   Z-ADD     CTTCAS        OSCASE
     C                   MOVE      MSMSCD        OSMSCD
     C                   MOVE      MSSEQ#        OSSEQ#
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        OSUSER
     C                   MOVEL     Q1PGM         OSPGM
     C                   MOVEL     DSTIME        OSTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OSDT
     C*
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   MOVE      'Y'           UPNDT1            1
     C                   UPDATE    POS01
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN60         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   MOVE      'Y'           UPNDT1            1
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** OSKY      DELETPOS01                97
     C                   DELETE    POS01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN51 = OFF
     C*
     C     OSKY          READE     POS01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*** END ADD *** 08/05/99 ****************************************
     C*
     C**************************************************************
     C*** FILE NDTMP
     C**************************************************************
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C**********           MOVE ' '       UPNDT
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C                   MOVE      *BLANKS       KKFILE
     C                   MOVEL(P)  'NDTMP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     TMKY          SETLL     PTM01
     C     TMKY          READE     PTM01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C*** BEGIN DEL *** 02/10/00 **************************************
     C**********           Z-ADDCTTCO#    TMCO#
     C**********           Z-ADDCTTSUB    TMSUB#
     C**********           Z-ADDCTTFND    TMFND
     C**********           Z-ADDCTTFYR    TMFYR
     C**********           Z-ADDCTTCAS    TMCASE
     C*** END   DEL *** 02/10/00 **************************************
     C   98              Z-ADD     HLSEQ#        TMMSEQ
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        TMUSER
     C                   MOVEL     Q1PGM         TMPGM
     C                   MOVEL     DSTIME        TMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C**********           MOVE 'Y'       UPNDT   1
     C*** END   DEL *** 02/10/00 **************************************
     C                   UPDATE    PTM01
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN60         IFEQ      *ON
     C*** BEGIN DEL *** 02/10/00 **************************************
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C**********           MOVE 'Y'       UPNDT   1
     C********** TMKY      DELETPTM01                97
     C                   UPDATE    PTM01
     C*** END   DEL *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN51 = OFF
     C*
     C     TMKY          READE     PTM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C*** END ADD *** 02/08/99 ***************************************
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPMS01                97
     C                   DELETE    PMS01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C*** BEG ADD *** 02/08/99 ***************************************
     C*
     C                   MOVE      *BLANKS       KKFILE
     C                   MOVEL     'FAMSP'       KKFILE
     C                   EXSR      UPDATE
     C*** END ADD *** 02/08/99 ***************************************
     C     GENKY         READE     PMS01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C*** BEG ADD *** 02/08/99 ***************************************
     C*
     C*** BEGIN ADD *** 08/05/99 **************************************
     C     UPNDT1        IFEQ      'Y'
     C                   MOVEL(P)  'FDOSP'       KKFILE
     C                   EXSR      PRINT
     C                   ENDIF
     C*** END ADD *** 08/05/99 ****************************************
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** UPNDT     IFEQ 'Y'
     C**********           MOVE *BLANKS   KKFILE
     C**********           MOVE 'NDTMP'   KKFILE
     C**********           EXSR PRINT
     C**********           ENDIF
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C                   MOVEL(P)  'FAMSP'       KKFILE
     C*** END ADD *** 02/08/99 ***************************************
     C                   EXSR      PRINT
     C*
     C*** FILE FAPPP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FAPPP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PPP01
     C     GENKY         READE     PPP01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        PPCO#
     C                   Z-ADD     CTTSUB        PPSUB#
     C                   Z-ADD     CTTFND        PPFND
     C                   Z-ADD     CTTFYR        PPFYR
     C                   Z-ADD     CTTCAS        PPCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        PPUSER
     C                   MOVEL     Q1PGM         PPPGM
     C                   MOVEL     DSTIME        PPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPDT
     C*
     C                   UPDATE    PPP01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPPP01                97
     C                   DELETE    PPP01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PPP01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*
     C*** FILE FATXP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FATXP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PTX01
     C     GENKY         READE     PTX01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        TXCO#
     C                   Z-ADD     CTTSUB        TXSUB#
     C                   Z-ADD     CTTFND        TXFND
     C                   Z-ADD     CTTFYR        TXFYR
     C                   Z-ADD     CTTCAS        TXCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        TXUSER
     C                   MOVEL     Q1PGM         TXPGM
     C                   MOVEL     DSTIME        TXTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TXDT
     C*
     C                   UPDATE    PTX01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPTX01                97
     C                   DELETE    PTX01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF
     C*
     C     GENKY         READE     PTX01                                  99
     C*
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE FAWHP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FAWHP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PWH01
     C     GENKY         READE     PWH01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        WHCO#
     C                   Z-ADD     CTTSUB        WHSUB#
     C                   Z-ADD     CTTFND        WHFND
     C                   Z-ADD     CTTFYR        WHFYR
     C                   Z-ADD     CTTCAS        WHCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        WHUSER
     C                   MOVEL     Q1PGM         WHPGM
     C                   MOVEL     DSTIME        WHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WHDT
     C*
     C                   UPDATE    PWH01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPWH01                97
     C                   DELETE    PWH01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PWH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE FATRP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FATRP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     *IN98         IFEQ      *ON
     C     GENKY1        SETGT     PTR01
     C     GENKY1        READPE    PTR01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     TRTX#         XXTX#
     C                   ELSE
     C                   Z-ADD     0             XXTX#
     C                   ENDIF
     C                   ENDIF
     C*
     C     GENKY         SETLL     PTR01
     C     GENKY         READE     PTR01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             O2CNT             9 0
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        TRCO#
     C                   Z-ADD     CTTSUB        TRSUB#
     C                   Z-ADD     CTTFND        TRFND
     C                   Z-ADD     CTTFYR        TRFYR
     C                   Z-ADD     CTTCAS        TRCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        TRUSER
     C                   MOVEL     Q1PGM         TRPGM
     C                   MOVEL     DSTIME        TRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRDT
     C                   ADD       1             XXTX#
     C   98              Z-ADD     XXTX#         TRTX#
     C*
     C*** FILE FATDP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FATDP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PTD01
     C     GENKY         READE     PTD01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        TDCO#
     C                   Z-ADD     CTTSUB        TDSUB#
     C                   Z-ADD     CTTFND        TDFND
     C                   Z-ADD     CTTFYR        TDFYR
     C                   Z-ADD     CTTCAS        TDCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        TDUSER
     C                   MOVEL     Q1PGM         TDPGM
     C                   MOVEL     DSTIME        TDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TDDT
     C   98              Z-ADD     XXTX#         TDTX#
     C*
     C                   UPDATE    PTD01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPTD01                97
     C                   DELETE    PTD01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF
     C*
     C     GENKY         READE     PTD01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE FATIP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FATIP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PTI01
     C     GENKY         READE     PTI01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        TICO#
     C                   Z-ADD     CTTSUB        TISUB#
     C                   Z-ADD     CTTFND        TIFND
     C                   Z-ADD     CTTFYR        TIFYR
     C                   Z-ADD     CTTCAS        TICASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        TIUSER
     C                   MOVEL     Q1PGM         TIPGM
     C                   MOVEL     DSTIME        TITIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TIDT
     C   98              Z-ADD     XXTX#         TITX#
     C*
     C                   UPDATE    PTI01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPTI01                97
     C                   DELETE    PTI01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PTI01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*****************************************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   UPDATE    PTR01                                        FATRP
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPTR01                97
     C                   DELETE    PTR01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             O2CNT
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PTR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   MOVEL(P)  'FATRP'       KKFILE
     C                   Z-ADD     O2CNT         OOCNT
     C                   EXSR      PRINT
     C                   Z-ADD     *ZEROS        O2CNT
     C*****************************************************************
     C*** FILE FAERP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FAERP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PER01
     C     GENKY         READE     PER01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        ERCO#
     C                   Z-ADD     CTTSUB        ERSUB#
     C                   Z-ADD     CTTFND        ERFND
     C                   Z-ADD     CTTFYR        ERFYR
     C                   Z-ADD     CTTCAS        ERCASE
     C                   MOVEL(P)  CTTSS#        ERSS#
     C                   Z-ADD     CTTACD        ERACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ERUSER
     C                   MOVEL     Q1PGM         ERPGM
     C                   MOVEL     DSTIME        ERTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C*
     C                   UPDATE    PER01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPER01                97
     C                   DELETE    PER01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PER01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*** BEGIN DEL *** 02/10/00 **************************************
     C*
     C*** FILE NATRP
     C*
     C**********           MOVE *BLANKS   KKFILE
     C*
     C**********           MOVEL'NATRP'   KKFILE    P
     C*
     C**********           EXSR UPDATE
     C********** *IN50     IFEQ *ON
     C*
     C********** GENKY     SETLLPTR02
     C********** GENKY     READEPTR02                    99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** *IN51     IFEQ *ON
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C**********           Z-ADDCTTCO#    T2CO#
     C**********           Z-ADDCTTSUB    T2SUB#
     C**********           Z-ADDCTTFND    T2FND
     C**********           Z-ADDCTTFYR    T2FYR
     C**********           Z-ADDCTTCAS    T2CASE
     C*
     C**********           TIME           DSDTTM
     C**********           MOVELQ1USER    T2USER
     C**********           MOVELQ1PGM     T2PGM
     C**********           MOVELDSTIME    T2TIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     T2DT
     C*
     C**********           UPDATPTR02
     C*
     C**********           ELSE
     C*
     C********** GENKY     DELETPTR02                97
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C*
     C**********           ENDIF                           *IN51 = ON
     C*
     C********** GENKY     READEPTR02                    99
     C**********           ENDDO                           *IN99 = OFF
     C*
     C**********           ENDIF                           *IN50 = ON
     C*
     C**********           EXSR PRINT
     C*
     C**********
     C*** FILE NAPBP
     C*
     C**********           MOVE *BLANKS   KKFILE
     C*
     C**********           MOVEL'NAPBP'   KKFILE    P
     C*
     C**********           EXSR UPDATE
     C********** *IN50     IFEQ *ON
     C*
     C********** GENKY     SETLLPPB01
     C********** GENKY     READEPPB01                    99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** *IN51     IFEQ *ON
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C**********           Z-ADDCTTCO#    PBCO#
     C**********           Z-ADDCTTSUB    PBSUB#
     C**********           Z-ADDCTTFND    PBFND
     C**********           Z-ADDCTTFYR    PBFYR
     C**********           Z-ADDCTTCAS    PBCASE
     C**********           MOVELCTTSS#    PBSS#     P
     C**********           Z-ADDCTTACD    PBACDT
     C*
     C**********           TIME           DSDTTM
     C**********           MOVELQ1USER    PBUSER
     C**********           MOVELQ1PGM     PBPGM
     C**********           MOVELDSTIME    PBTIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     PBDT
     C*
     C**********           UPDATPPB01
     C*
     C**********           ELSE
     C*
     C********** GENKY     DELETPPB01                97
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C*
     C**********           ENDIF                           *IN51 = ON
     C*
     C********** GENKY     READEPPB01                    99
     C**********           ENDDO                           *IN99 = OFF
     C*
     C**********           ENDIF                           *IN50 = ON
     C*
     C**********           EXSR PRINT
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** FILE FDEOP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FDEOP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PEO01
     C     GENKY         READE     PEO01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             O2CNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        EOCO#
     C                   Z-ADD     CTTSUB        EOSUB#
     C                   Z-ADD     CTTFND        EOFND
     C                   Z-ADD     CTTFYR        EOFYR
     C                   Z-ADD     CTTCAS        EOCASE
     C*
     C                   TIME                    DSDTTM
     C******************** MOVELQ1USER    EOUSER           DEL 02/02/00
     C                   MOVEL     Q1PGM         EOPGM
     C                   MOVEL     DSTIME        EOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   MOVEL(P)  EOEOB#        KKEOB#
     C*
     C*** FILE FDE1P
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FDE1P'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         SETLL     PE101
     C     EOBKY         READE     PE101                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        E1CO#
     C                   Z-ADD     CTTSUB        E1SUB#
     C                   Z-ADD     CTTFND        E1FND
     C                   Z-ADD     CTTFYR        E1FYR
     C                   Z-ADD     CTTCAS        E1CASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        E1USER
     C                   MOVEL     Q1PGM         E1PGM
     C                   MOVEL     DSTIME        E1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1DT
     C*
     C                   UPDATE    PE101
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** EOBKY     DELETPE101                97
     C                   DELETE    PE101
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     EOBKY         READE     PE101                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C****************************************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   UPDATE    PEO01                                        FDEOP
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPEO01                97
     C                   DELETE    PEO01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             O2CNT
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PEO01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   MOVEL(P)  'FDEOP'       KKFILE
     C                   Z-ADD     O2CNT         OOCNT
     C                   EXSR      PRINT
     C                   Z-ADD     *ZEROS        O2CNT
     C****************************************************************
     C*
     C*** FILE FADCP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FADCP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     *IN98         IFEQ      *ON
     C     GENKY1        SETGT     PDC01
     C     GENKY1        READPE(N) PDC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     DCDCS#        HLDDCS            5 0
     C                   ELSE
     C                   Z-ADD     0             HLDDCS
     C                   ENDIF
     C                   ENDIF
     C*
     C     GENKY         SETLL     PDC01
     C     GENKY         READE     PDC01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             O2CNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        DCCO#
     C                   Z-ADD     CTTSUB        DCSUB#
     C                   Z-ADD     CTTFND        DCFND
     C                   Z-ADD     CTTFYR        DCFYR
     C                   Z-ADD     CTTCAS        DCCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DCUSER
     C                   MOVEL     Q1PGM         DCPGM
     C                   MOVEL     DSTIME        DCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C                   MOVEL(P)  DCFEIN        KKFEIN
     C                   MOVEL(P)  DCMOD         KKMOD
     C                   MOVEL(P)  DCDCS#        KKDCS#
     C   98              ADD       1             HLDDCS
     C   98              Z-ADD     HLDDCS        DCDCS#
     C*
     C*** FILE FDCAP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FDCAP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     *IN51         IFEQ      *ON
     C*
     C*** GET LAST SEQUENCE NUMBER
     C*
     C                   Z-ADD     *ZEROS        HLDCS#            5 0
     C*          GENKY1    SETGTPDC01
     C*          GENKY1    REDPEPDC01               N    99
     C*          *IN99     IFEQ *OFF
     C                   Z-ADD     DCDCS#        HLDCS#
     C*                    ENDIF                           *IN99 = OFF
     C                   Z-ADD     HLDCS#        KKDCS#
     C***************************************************************
     C* NOW WE MUST CHAIN TO THE DETAIL CHARGE FILE WITH THE OLD CASE
     C* AND THEN GET THE NEXT AVAILABLE DSEQ FOR THE NEW CASE
     C*
     C     *IN98         IFEQ      '1'
     C     GENKY1        SETGT     PCA02
     C     GENKY1        READPE    PCA02                                  65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     CADSEQ        HLDSEQ            5 0
     C                   ELSE
     C                   Z-ADD     *ZEROS        HLDSEQ
     C                   END                                                    END *IN65
     C                   END                                                    END *IN98
     C*
     C                   ENDIF                                                  END *IN51
     C*
     C     FENKY         SETLL     PCA01
     C     FENKY         READE     PCA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C   98              ADD       1             HLDSEQ
     C*
     C     *IN51         IFEQ      *ON
     C*** BEGIN DEL *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   ADD       1             O3CNT             9 0
     C                   Z-ADD     CTTCO#        CACO#
     C                   Z-ADD     CTTSUB        CASUB#
     C                   Z-ADD     CTTFND        CAFND
     C                   Z-ADD     CTTFYR        CAFYR
     C                   Z-ADD     CTTCAS        CACASE
     C                   MOVEL(P)  CTTSS#        CASS#
     C                   Z-ADD     CTTACD        CAACDT
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   MOVEL     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C   98              Z-ADD     HLDSEQ        CADSEQ
     C   98              Z-ADD     HLDCS#        CADCS#
     C*
     C                   UPDATE    PCA01
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN60         IFEQ      *ON
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** FENKY     DELETPCA01                97
     C                   DELETE    PCA01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   ADD       OOCNT         O3CNT
     C                   ENDIF                                                  *IN60 = ON
     C*
     C     FENKY         READE     PCA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C                   FEOD      FDCALD
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C*                    EXSR PRINT
     C*****************************************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   UPDATE    PDC01                                        FADCP
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDC01                97
     C                   DELETE    PDC01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             O2CNT
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PDC01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   MOVEL(P)  'FADCP'       KKFILE
     C                   Z-ADD     O2CNT         OOCNT
     C                   EXSR      PRINT
     C                   Z-ADD     *ZEROS        O2CNT
     C*
     C                   MOVEL(P)  'FDCAP'       KKFILE
     C                   Z-ADD     O3CNT         OOCNT
     C*                    ADD  X2CNT     XXCNT
     C                   EXSR      PRINT
     C                   Z-ADD     *ZEROS        O2CNT
     C*** BEGIN DEL *** 02/10/00 **************************************
     C*****************************************************************
     C*
     C*** FILE FALTP
     C*
     C**********           MOVE *BLANKS   KKFILE
     C*
     C**********           MOVEL'FALTP'   KKFILE    P
     C*
     C**********           EXSR UPDATE
     C********** *IN50     IFEQ *ON
     C*
     C********** GENKY     SETLLPLT01
     C********** GENKY     READEPLT01                    99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** *IN51     IFEQ *ON
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C**********           Z-ADDCTTCO#    LTCO#
     C**********           Z-ADDCTTSUB    LTSUB#
     C**********           Z-ADDCTTFND    LTFND
     C**********           Z-ADDCTTFYR    LTFYR
     C**********           Z-ADDCTTCAS    LTCASE
     C**********           MOVELCTTSS#    LTSS#     P
     C**********           Z-ADDCTTACD    LTACDT
     C**********           MOVELCTTLSN    LTLNM     P
     C**********           MOVELCTTFSN    LTFNM     P
     C*
     C**********           TIME           DSDTTM
     C**********           MOVELQ1USER    LTUSER
     C**********           MOVELQ1PGM     LTPGM
     C**********           MOVELDSTIME    LTTIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     LTDT
     C*
     C**********           UPDATPLT01
     C*
     C**********           ELSE
     C********** GENKY     DELETPLT01                97
     C**********           ADD  1         OOCNT
     C**********           ADD  1         XXCNT
     C**********           ENDIF                           *IN51 = ON
     C*
     C********** GENKY     READEPLT01                    99
     C**********           ENDDO                           *IN99 = OFF
     C*
     C**********           ENDIF                           *IN50 = ON
     C*
     C**********           EXSR PRINT
     C*** END   DEL *** 02/10/00 **************************************
     C**********
     C*
     C*** FILE UMPCP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'UMPCP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PPC01
     C     GENKY         READE     PPC01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        PCCO#
     C                   Z-ADD     CTTSUB        PCSUB#
     C                   Z-ADD     CTTFND        PCFND
     C                   Z-ADD     CTTFYR        PCFYR
     C                   Z-ADD     CTTCAS        PCCASE
     C                   MOVEL(P)  CTTSS#        PCSS#
     C                   Z-ADD     CTTACD        PCACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        PCUSER
     C                   MOVEL     Q1PGM         PCPGM
     C                   MOVEL     DSTIME        PCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C*
     C                   UPDATE    PPC01
     C*
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPPC01                97
     C                   DELETE    PPC01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PPC01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE UMCHP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'UMCHP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PCH02
     C     GENKY         READE     PCH02                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        C2CO#
     C                   Z-ADD     CTTSUB        C2SUB#
     C                   Z-ADD     CTTFND        C2FND
     C                   Z-ADD     CTTFYR        C2FYR
     C                   Z-ADD     CTTCAS        C2CASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        C2USER
     C                   MOVEL     Q1PGM         C2PGM
     C                   MOVEL     DSTIME        C2TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C2DT
     C*
     C                   UPDATE    PCH02
     C*
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPCH02                97
     C                   DELETE    PCH02
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PCH02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE UDPOP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'UDPOP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PPO01
     C     GENKY         READE     PPO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        POCO#
     C                   Z-ADD     CTTSUB        POSUB#
     C                   Z-ADD     CTTFND        POFND
     C                   Z-ADD     CTTFYR        POFYR
     C                   Z-ADD     CTTCAS        POCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        POUSER
     C                   MOVEL     Q1PGM         POPGM
     C                   MOVEL     DSTIME        POTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PODT
     C*
     C                   UPDATE    PPO01
     C*
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPPO01                97
     C                   DELETE    PPO01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PPO01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*
     C****************************************************************
     C*
     C                   CALL      'F7096R01'
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      @@KEY         ##KEY             9 0
     C                   PARM      @@FLAG        ##FLAG            1
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*** BEGIN ADD *** 02/10/00 **************************************
     C*****************************************************************
     C*
     C*** FILE FALTP
     C*
     C                   MOVE      *BLANKS       KKFILE
     C*
     C                   MOVEL(P)  'FALTP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PLT01
     C     GENKY         READE     PLT01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   Z-ADD     CTTCO#        LTCO#
     C                   Z-ADD     CTTSUB        LTSUB#
     C                   Z-ADD     CTTFND        LTFND
     C                   Z-ADD     CTTFYR        LTFYR
     C                   Z-ADD     CTTCAS        LTCASE
     C                   MOVEL(P)  CTTSS#        LTSS#
     C                   Z-ADD     CTTACD        LTACDT
     C                   MOVEL(P)  CTTLSN        LTLNM
     C                   MOVEL(P)  CTTFSN        LTFNM
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        LTUSER
     C                   MOVEL     Q1PGM         LTPGM
     C                   MOVEL     DSTIME        LTTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C*
     C                   UPDATE    PLT01
     C*
     C                   ELSE
     C                   DELETE    PLT01
     C                   ADD       1             OOCNT
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     GENKY         READE     PLT01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   EXSR      PRINT
     C*** END   ADD *** 02/10/00 **************************************
     C*
     C     *IN53         IFEQ      *ON
     C*
     C     *IN98         IFEQ      *ON
     C                   ADD       1             O1CNT
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPCM01                97
     C     @@RRN2        DELETE    PCM01                              97
     C*** END   CHG *** 02/10/00 **************************************
     C                   ELSE
     C                   ADD       1             O1CNT             9 0
     C                   ADD       1             XXCNT
     C                   UPDATE    PCM01                                        FMCMP
     C                   ENDIF
     C*
     C                   ELSE                                                   *IN53 = OFF
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPCM01                97
     C                   DELETE    PCM01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             O1CNT
     C                   ADD       1             XXCNT
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** *IN98     IFEQ *OFF
     C********** GENKY     READEPCM01                    99
     C**********           ELSE
     C********** GENKY1    READEPCM01                    99
     C**********           ENDIF                           @@FLAG = X
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   MOVEL(P)  'FMCMP'       KKFILE
     C                   Z-ADD     O1CNT         OOCNT
     C                   EXSR      PRINT
     C*
     C****************************************************************
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATE - Determine if record should be put into file
     C*================================================================
     C     UPDATE        BEGSR
     C*
     C                   SETOFF                                       505160
     C                   Z-ADD     *ZEROS        OOCNT
     C*
     C     T1KY          CHAIN     PT101                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        50
     C     T1STAT        IFEQ      'T'
     C                   SETON                                        5153
     C                   ENDIF
     C*
     C     T1STAT        IFEQ      'D'
     C                   SETON                                        60
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVE      *OFF          *IN99
     C*
     C                   ENDSR
     C*================================================================
     C* PRINT SUBROUTINE
     C*================================================================
     C*
     C     PRINT         BEGSR
     C*** BEGIN DEL *** 11/19/99 **************************************
     C********   T3KY      CHAINPT301                99
     C********   *IN99     IFEQ *OFF
     C********             MOVELT3DESC    OODESC 35
     C********             ENDIF
     C********   *INOA     IFEQ '1'
     C********             MOVELKKFILE    OOFILE
     C********             TIME           DSDTTM
     C********             EXCPTHEAD
     C********             MOVE *BLANKS   SVFILE
     C********             MOVE '0'       *INOA
     C********             ENDIF
     C********             SETOF                       10
     C********   OOCNT     IFNE *ZEROS
     C********   KKFILE    IFNE SVFILE
     C********             MOVELKKFILE    SVFILE
     C********             MOVELKKFILE    OOFILE
     C********             EXCPTDETH
     C********             ENDIF
     C********             ENDIF
     C*** END DEL *** 11/19/99 ****************************************
     C*** BEGIN CHG *** 11/19/99 **************************************
     C     OOCNT         IFNE      *ZEROS
     C     *INOA         IFEQ      *ON
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF                                                  *INOA = *On
     C                   SETOFF                                         10
     C     KKFILE        IFNE      SVFILE
     C     T3KY          CHAIN     PT301                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     T3DESC        OODESC           35
     C                   ENDIF                                                  *IN99 = Off
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF                                                  FILE <> FILE
     C                   ENDIF
     C*** END CHG *** 11/19/99 ****************************************
     C                   ENDSR
     C*
     OQSYSPRT   E            HEAD           1 05
     O                       DSTIME              08 '  :  :  '
     O                                           76 'TRANSFER/DELETION REPORT'
     O                       DSDATE        Y    131
     O          E            HEAD           3
     O                       Q1PGM               10
     O                                          126 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           1
     O               60                           8 'DELETED:'
     O              N60                           5 'FROM:'
     O                       OOHD1               88
     O          E            HEAD           2
     O                       OOHD2               88
     O          E            HEAD           1
     O              N60                           3 'TO:'
     O              N60      OOHD3               88
     O          E            HEAD           2
     O              N60      OOHD4               88
     O*
     O          E            DETH        1  1
     O                                            5 'FILE:'
     O                       OOFILE         B    16
     O                       OODESC         B    57
     O                                           56 '==>'
     O*                        OOCNT ZB  61
     O*                10                61 'NO'
     O                                           71 'FILE PROCESSED'
     O*
     O*       E 22             TOT
     O*                                  27 'FINAL TOTAL'
     O*                        XXCNT Z   61
     O*                                  80 'RECORDS PROCESSED'
