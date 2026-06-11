     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXCXFER - CASE TRANSFER/DELETION PROGRAM
     H*****************************************************************
     H* DESCRIPTION: CASE TRANSFER/DELETION - WRITE ALL FILES
     H*
     H* 07/20/16  C:79  SUPPORT: 41844/136
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H* INDICATORS:
     H*
     H* *IN50 = *ON  - FILE IS TO BE TRANSFERRED/DELETED
     H* *IN51 = *ON  - T1STAT = 'T' (TRANSFER)
     H* *IN52 = *ON  - T1STAT = 'D' (DELETE)
     H* *IN53 = *ON  - T1STAT = 'T' (TRANSFER) REPORT ONLY
     H* *IN98 = *OFF - @@FLAG = 'X' (SINGLE CASE TRANSFER)
     H* *IN98 = *ON  - @@FLAG <> 'X' (DOUBLE CASE TRANSFER)
     H*
     H*****************************************************************
     H*
     FFMCMP     IF   E             DISK
     F*
     FFAT1P     IF   E             DISK
     F*
     FFACTP     IF   E             DISK
     F*
     FFAT3L     IF   E           K DISK
     F*
     FFAEOP     UF   E             DISK
     F                                     RENAME(PEO01:PEO02)
     FFAEOL     UF   E           K DISK
     F*
     F*** BEGIN CHG *** 02/10/00 **************************************
     F**********NAMBL   UF  E           K        DISK
     FNAMBL1    UF   E           K DISK
     F*** END   CHG *** 02/10/00 **************************************
     F*** BEGIN ADD *** 02/10/00 **************************************
     FNAPBL1    UF   E           K DISK
     FNATRL1    UF   E           K DISK
     FNMARLA    UF   E           K DISK
     FFALTL7    UF   E           K DISK
     F*** END   ADD *** 02/10/00 **************************************
     F*
     F*** BEGIN CHG *** 01/19/00 **************************************
     F**********NASBL   UF  E           K        DISK
     FNASBL1    UF   E           K DISK
     F*** END   CHG *** 01/19/00 **************************************
     F*
     F*** BEGIN CHG *** 01/19/00 **************************************
     F**********NDTML   UF  E           K        DISK
     FNDTML1    UF   E           K DISK
     F*** END   CHG *** 01/19/00 **************************************
     F*
     F*** BEG CHG *** 05/05/99 ****************************************
     F**********FAEAL   UF  E           K        DISK
     FFAEAL1    UF   E           K DISK
     F*** END CHG *** 05/05/99 ****************************************
     F*
     FFAEDL     UF   E           K DISK
     F*
     FFAEEL     UF   E           K DISK
     F*
     FFAEHL     UF   E           K DISK
     F*
     FFAEPL     UF   E           K DISK
     F*
     FFAESL     UF   E           K DISK
     F*
     FFAEUL     UF   E           K DISK
     F*
     FFAE1L     UF   E           K DISK
     F*
     FFAHBL     UF   E           K DISK
     F*
     FFDCALD    UF   E           K DISK
     F*
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     F*
     FFDCAL13   UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*
     FFADUL2    UF   E           K DISK
     F*
     F*** BEGIN ADD *** 02/10/00 **************************************
     F**********FDDAL   UF  E           K        DISK
     FFDDAL2    UF   E           K DISK
     F*** END   ADD *** 02/10/00 **************************************
     F*
     FFDDDL     UF   E           K DISK
     F*
     FFDDEL     UF   E           K DISK
     F*
     FFDDHL     UF   E           K DISK
     F*
     FFDDPL     UF   E           K DISK
     F*
     FFDDUL     UF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     F*** BEGIN CHG *** 02/10/00 **************************************
     F**********FDHCL3  UF  E           K        DISK
     FFDHCL     UF   E           K DISK
     F*** END   CHG *** 02/10/00 **************************************
     F*
     F*** BEGIN CHG *** 02/10/00 **************************************
     F**********FDMGL3  UF  E           K        DISK
     FFDMGL1    UF   E           K DISK
     F*** END   CHG *** 02/10/00 **************************************
     F*
     FFADCL     UF   E           K DISK
     F*** BEGIN ADD *** 05/31/02 **************************************
     FFDCHL1    UF   E           K DISK
     F*** END   ADD *** 05/31/02 **************************************
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     F*

     I*
     IPDU02
     I              DUCO#                       D2CO#
     I              DUSUB#                      D2SUB#
     I              DUFND                       D2FND
     I              DUFYR                       D2FYR
     I              DUCASE                      D2CASE
     I              DUACDT                      D2ACDT
     I              DUSS#                       D2SS#
     I              DUDSEQ                      D2DSEQ
     I              DUUSER                      D2USER
     I              DUTIME                      D2TIME
     I              DUPGM                       D2PGM
     I              DUDT                        D2DT
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@TD              1
     C                   PARM                    @@KEY             9 0
     C                   PARM                    @@FLAG            1
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@Update          1
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
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     GENKY         KLIST                                                  FROM CASE
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     GENKY1        KLIST                                                  TO CASE
     C                   KFLD                    K1CO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1CASE
     C*** BEGIN ADD *** 01/19/00 **************************************
     C*
     C     TMKY          KLIST                                                  TO CASE
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
     C*
     C     TMKY2         KLIST                                                  TO CASE
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 01/19/00 **************************************
     C*
     C     FENKY1        KLIST                                                  TO CASE
     C                   KFLD                    K1CO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1CASE
     C                   KFLD                    KFFEIN
     C                   KFLD                    KFMOD
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
     C*** BEGIN ADD *** 02/10/00 **************************************
     C     EOBKY1        KLIST
     C                   KFLD                    K1CO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1CASE
     C                   KFLD                    K1EOB#
     C*
     C     CAPKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    OLDSEQ
     C*
     C     CAPKY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    OLDSEQ
     C*** END   ADD *** 02/10/00 **************************************
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
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMPGM         SVFILE
     C     *LIKE         DEFINE    CMPGM         OOFILE
     C     *LIKE         DEFINE    T1FILE        KKFILE
     C     *LIKE         DEFINE    T1KEY         KKKEY
     C     *LIKE         DEFINE    MBSS#         KKSS#
     C     *LIKE         DEFINE    MBCO#         KKCO#
     C     *LIKE         DEFINE    MBSUB#        KKSUB#
     C     *LIKE         DEFINE    MBFND         KKFND
     C     *LIKE         DEFINE    MBFYR         KKFYR
     C     *LIKE         DEFINE    MBCASE        KKCASE
     C     *LIKE         DEFINE    MBSTAT        KKSTAT
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    MBSS#         K1SS#
     C     *LIKE         DEFINE    MBCO#         K1CO#
     C     *LIKE         DEFINE    MBSUB#        K1SUB#
     C     *LIKE         DEFINE    MBFND         K1FND
     C     *LIKE         DEFINE    MBFYR         K1FYR
     C     *LIKE         DEFINE    MBCASE        K1CASE
     C     *LIKE         DEFINE    DCFEIN        KKFEIN
     C     *LIKE         DEFINE    DCMOD         KKMOD
     C     *LIKE         DEFINE    DCDCS#        KKDCS#
     C*** BEGIN ADD *** 08/05/99 **************************************
     C     *LIKE         DEFINE    CADSEQ        OLDSEQ
     C     *LIKE         DEFINE    CADSEQ        WRKSEQ
     C*** END ADD *** 08/05/99 ****************************************
     C*** BEGIN ADD *** 01/19/00 **************************************
     C     *LIKE         DEFINE    TMSEQ#        KKSEQ#
     C     *LIKE         DEFINE    TMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    EABAMT        XXBAMT
     C     *LIKE         DEFINE    EABAMT        X1BAMT
     C     *LIKE         DEFINE    EAEOB#        K1EOB#
     C*** END   ADD *** 01/19/00 **************************************
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C                   MOVE      *ON           *INOA
     C*** BEGIN ADD *** 11/19/99 **************************************
     C     @@TD          COMP      'D'                                    53
     C*** END ADD *** 11/19/99 ****************************************
     C*
     C** Get standard Print headings
     C*
     C     @@RRN2        CHAIN(N)  PEO02                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
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
     C*** BEG ADD *** 11/30/98 ****************************************
     C     @@FLAG        IFNE      'X'
     C*** END ADD *** 11/30/98 ****************************************
     C     @@RRN3        CHAIN(N)  PCM01                              99
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
     C*** BEG ADD *** 11/30/98 ****************************************
     C                   ENDIF
     C*** END ADD *** 11/30/98 ****************************************
     C*
     C     @@RRN         CHAIN     PCT01                              99
     C                   Z-ADD     CTFCO#        KKCO#
     C                   Z-ADD     CTFSUB        KKSUB#
     C                   Z-ADD     CTFFND        KKFND
     C                   Z-ADD     CTFFYR        KKFYR
     C                   Z-ADD     CTFCAS        KKCASE
     C                   MOVEL(P)  CTFSS#        KKSS#
     C*
     C                   Z-ADD     CTTCO#        K1CO#
     C                   Z-ADD     CTTSUB        K1SUB#
     C                   Z-ADD     CTTFND        K1FND
     C                   Z-ADD     CTTFYR        K1FYR
     C                   Z-ADD     CTTCAS        K1CASE
     C                   MOVEL(P)  CTTSS#        K1SS#
     C*
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
     C                   Z-ADD     @@KEY         KKKEY
     C*
     C     @@FLAG        IFEQ      'X'
     C                   MOVE      *OFF          *IN98                          SINGLE CASE
     C                   ELSE
     C                   MOVE      *ON           *IN98                          DOUBLE CASE
     C                   ENDIF
     C*
     C                   EXSR      MOVEFI
     C*
     C*** BEG ADD *** 02/05/99 ***************************************
     C                   EXSR      PRINT
     C*** END ADD *** 02/05/99 ***************************************
     C                   CALL      'F7096R02'                           60
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      @@KEY         ##KEY             9 0
     C                   PARM      @@FLAG        ##FLAG            1
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* MOVEFI - FILES THAT ARE BEING UPDATED
     C*================================================================
     C     MOVEFI        BEGSR
     C*
     C**************************************************************
     C*** FILE FAEOP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAEOP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C*
     C     *IN50         IFEQ      *ON
     C*
     C     GENKY         SETLL     PEO01
     C     GENKY         READE     PEO01                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEGIN ADD *** 01/19/00 **************************************
     C*
     C* CHECK TO SEE IF THE SYSTEM TYPE IS 'M' = MEDICAL CASE
     C*
     C                   MOVEL     *OFF          YES               1
     C*
     C     EOSYS         IFEQ      'M'
     C*
     C* CHECK TO SEE IF THE CASE IS BEING TRANSFERRED INTO AN EXISTING
     C* CASE.
     C*
     C     *IN98         IFEQ      *ON
     C*
     C* IF SO, CHECK TO SEE IF THE EOMB HAS BEEN INVOICED
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOBKY         CHAIN(N)  PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     EABINV        ANDEQ     *ZEROS
     C*
     C* IF NOT INVOICED, THEN CHECK TO SEE IF THERE IS AN UNINVOICED
     C* 'M' TYPE ATTACHED TO THE 'TO CASE'.
     C*
     C     GENKY1        CHAIN(N)  PEA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     EASYS         IFEQ      'M'
     C     EABINV        ANDEQ     *ZEROS
     C*
     C* IF THERE IS THEN, DETERMINE THE NEXT T/M SEQ# TO USE
     C*
     C                   MOVEL     *HIVAL        KKSEQ#
     C*
     C     TMKY2         SETGT     PTM01
     C     TMKY          READPE(N) PTM01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     TMSEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C* THEN, TRANSFER ALL NDTMP RECORDS TO THAT EOMB
     C*
     C                   Z-ADD     *ZEROS        XXBAMT
     C*
     C     EOBKY         CHAIN     PTM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EACO#         TMCO#
     C                   Z-ADD     EASUB#        TMSUB#
     C                   Z-ADD     EAFND         TMFND
     C                   Z-ADD     EAFYR         TMFYR
     C                   Z-ADD     EACASE        TMCASE
     C                   Z-ADD     EAEOB#        TMEOB#
     C                   Z-ADD     XXSEQ#        TMSEQ#
     C                   ADD       1             XXSEQ#
     C*
     C* ACCUMULATE AMOUNTS TO ADD TO EABAMT
     C*
     C     TMHRS         MULT      TMHRRT        XXTMHR            7 2
     C     TMMILE        MULT      TMMIRT        XXTMMR            7 2
     C     XXTMHR        ADD       XXTMMR        X1BAMT
     C                   ADD       TMMSCG        X1BAMT
     C*
     C                   ADD       X1BAMT        XXBAMT
     C*
     C                   if        @@update = 'Y'
     C                   UPDATE    PTM01
     C                   endif
     C*
     C     EOBKY         READE     PTM01                                  99
     C*
     C                   ENDDO
     C*
     C* ADD AMOUNTS OF MOVED T/M LINE ITEMS TO EXISTING FAEAP RECORD
     C*
     C                   Z-ADD     EAEOB#        K1EOB#
     C*
     C     EOBKY1        CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   ADD       XXBAMT        EABAMT
     C*
     C                   if        @@update = 'Y'
     C                   UPDATE    PEA01
     C                   endif
     C*
     C                   ENDIF
     C*
     C* SET FLAG TO DENOTE THAT THE FAEOP RECORD AND FAEAP RECORDS
     C* ARE TO BE DELETED.
     C*
     C                   MOVEL     *ON           YES
     C*
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     GENKY1        READE(N)  PEA01                                  99
     C*
     C                   ENDDO
     C*
     C* OTHERWISE CONTINUE THE TRANSFER PROCESS AS NORMAL.
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/19/00 **************************************
     C*
     C*** GET LAST SEQUENCE NUMBER
     C*
     C     *IN98         IFEQ      *ON                                          DOUBLE CASE
     C     GENKY1        SETGT     PCA03
     C     GENKY1        READPE    PCA03                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        HLDSEQ            5 0
     C                   ELSE
     C                   Z-ADD     *ZEROS        HLDSEQ
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN98 = ON
     C*
     C                   Z-ADD     CTTCO#        EOCO#
     C                   Z-ADD     CTTSUB        EOSUB#
     C                   Z-ADD     CTTFND        EOFND
     C                   Z-ADD     CTTFYR        EOFYR
     C                   Z-ADD     CTTCAS        EOCASE
     C*** BEGIN ADD *** 08/05/99 **************************************
     C                   MOVEL(P)  CTTSS#        EOSS#
     C                   Z-ADD     CTTACD        EOACDT
     C                   MOVEL(P)  CTTLSN        EOLSN
     C                   MOVEL(P)  CTTFSN        EOFSN
     C                   MOVEL(P)  CTTMID        EOMID
     C*** END ADD *** 08/05/99 ****************************************
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
     C*** GET EOB # FOR EOBKY USED LATER **************************
     C*
     C                   MOVEL(P)  EOEOB#        KKEOB#
     C*
     C*** BEGIN ADD *** 02/10/00 **************************************
     C*
     C                   SELECT
     C**************************************************************
     C*** FILE NAMBP
     C**************************************************************
     C     EOSYS         WHENEQ    'M'
     C*
     C                   MOVEL(P)  'NAMBP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         CHAIN     PMB01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     *IN51         IFEQ      *ON
     C     YES           ANDEQ     *OFF
     C                   Z-ADD     CTTCO#        MBCO#
     C                   Z-ADD     CTTSUB        MBSUB#
     C                   Z-ADD     CTTFND        MBFND
     C                   Z-ADD     CTTFYR        MBFYR
     C                   Z-ADD     CTTCAS        MBCASE
     C                   MOVEL(P)  CTTSS#        MBSS#
     C                   Z-ADD     CTTACD        MBACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MBUSER
     C                   MOVEL     Q1PGM         MBPGM
     C                   MOVEL     DSTIME        MBTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBDT
     C*
     C                   ADD       1             CNT02             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PMB01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C     YES           OREQ      *ON
     C                   ADD       1             CNT02             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PMB01
     C                   ENDIF                                                  *IN52 = ON
     C                   ENDIF                                                  *IN99 = OFF
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE NAPBP
     C**************************************************************
     C     EOSYS         WHENEQ    'U'
     C*
     C                   MOVEL(P)  'NAPBP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         CHAIN     PPB01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        PBCO#
     C                   Z-ADD     CTTSUB        PBSUB#
     C                   Z-ADD     CTTFND        PBFND
     C                   Z-ADD     CTTFYR        PBFYR
     C                   Z-ADD     CTTCAS        PBCASE
     C                   MOVEL(P)  CTTSS#        PBSS#
     C                   Z-ADD     CTTACD        PBACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        PBUSER
     C                   MOVEL     Q1PGM         PBPGM
     C                   MOVEL     DSTIME        PBTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PBDT
     C*
     C                   ADD       1             CNT02             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PPB01
     C                   Endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT02             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PPB01
     C                   ENDIF                                                  *IN52 = ON
     C                   ENDIF                                                  *IN99 = OFF
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE NATRP
     C**************************************************************
     C     EOSYS         WHENEQ    'N'
     C*
     C                   MOVEL(P)  'NATRP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         CHAIN     PTR01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     *IN51         IFEQ      *ON
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
     C*
     C                   ADD       1             CNT02             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PTR01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT02             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PTR01
     C                   ENDIF                                                  *IN52 = ON
     C                   ENDIF                                                  *IN99 = OFF
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAHBP
     C**************************************************************
     C     EOSYS         WHENEQ    'H'
     C*
     C                   MOVEL(P)  'FAHBP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         CHAIN     PHB01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        HBCO#
     C                   Z-ADD     CTTSUB        HBSUB#
     C                   Z-ADD     CTTFND        HBFND
     C                   Z-ADD     CTTFYR        HBFYR
     C                   Z-ADD     CTTCAS        HBCASE
     C                   MOVEL(P)  CTTSS#        HBSS#
     C                   Z-ADD     CTTACD        HBACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        HBUSER
     C                   MOVEL     Q1PGM         HBPGM
     C                   MOVEL     DSTIME        HBTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBDT
     C*
     C                   ADD       1             CNT13             4 0
     C                   ADD       1             XXCNT
     C                   if        @@update = 'Y'
     C                   UPDATE    PHB01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT13             4 0
     C                   ADD       1             XXCNT
     C                   DELETE    PHB01
     C                   ENDIF                                                  *IN52 = ON
     C                   ENDIF                                                  *IN99 = OFF
     C                   ENDIF                                                  *IN50 = ON
     C*
     C                   ENDSL
     C**************************************************************
     C*** FILE FAE1P
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAE1P'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         CHAIN     PE101                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     *IN51         IFEQ      *ON
     C     YES           ANDEQ     *OFF
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
     C                   ADD       1             CNT12             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PE101
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C     YES           OREQ      *ON
     C                   ADD       1             CNT12             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PE101
     C                   ENDIF                                                  *IN52 = ON
     C                   ENDIF                                                  *IN99 = OFF
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAEAP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAEAP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         SETLL     PEA01
     C     EOBKY         READE     PEA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C     YES           ANDEQ     *OFF
     C                   Z-ADD     CTTCO#        EACO#
     C                   Z-ADD     CTTSUB        EASUB#
     C                   Z-ADD     CTTFND        EAFND
     C                   Z-ADD     CTTFYR        EAFYR
     C                   Z-ADD     CTTCAS        EACASE
     C                   MOVEL(P)  CTTSS#        EASS#
     C                   Z-ADD     CTTACD        EAACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   ADD       1             CNT05             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PEA01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C     YES           OREQ      *ON
     C                   ADD       1             CNT05             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PEA01
     C                   ENDIF                                                  *IN52 = ON
     C*
     C     EOBKY         READE     PEA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C**************************************************************
     C*** FILE FALTP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FALTP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         SETLL     PLT01
     C     EOBKY         READE     PLT01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C     YES           ANDEQ     *OFF
     C                   Z-ADD     CTTCO#        LTCO#
     C                   Z-ADD     CTTSUB        LTSUB#
     C                   Z-ADD     CTTFND        LTFND
     C                   Z-ADD     CTTFYR        LTFYR
     C                   Z-ADD     CTTCAS        LTCASE
     C                   MOVEL(P)  CTTSS#        LTSS#
     C                   Z-ADD     CTTACD        LTACDT
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
     C                   ADD       1             CNT05             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PLT01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C     YES           OREQ      *ON
     C                   ADD       1             CNT05             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PLT01
     C                   ENDIF                                                  *IN52 = ON
     C*
     C     EOBKY         READE     PLT01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C**************************************************************
     C*** FILE NMARP
     C**************************************************************
     C*
     C                   MOVEL(P)  'NMARP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C     EOBKY         CHAIN     PAR01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
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
     C                   ADD       1             CNT05             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PAR01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT05             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PAR01
     C                   ENDIF                                                  *IN52 = ON
     C*
     C     EOBKY         READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*** END   ADD *** 02/10/00 **************************************
     C*** BEGIN DEL *** 02/10/00 **************************************
     C**************************************************************
     C*** FILE NAMBP
     C**************************************************************
     C*
     C**********           MOVEL'NAMBP'   KKFILE    P
     C*
     C**********           EXSR UPDATE
     C********** *IN50     IFEQ *ON
     C*
     C********** GENKY     SETLLPMB01
     C********** GENKY     READEPMB01                    99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** *IN51     IFEQ *ON
     C**********           Z-ADDCTTCO#    MBCO#
     C**********           Z-ADDCTTSUB    MBSUB#
     C**********           Z-ADDCTTFND    MBFND
     C**********           Z-ADDCTTFYR    MBFYR
     C**********           Z-ADDCTTCAS    MBCASE
     C**********           MOVELCTTSS#    MBSS#     P
     C**********           Z-ADDCTTACD    MBACDT
     C*
     C**********           TIME           DSDTTM
     C**********           MOVELQ1USER    MBUSER
     C**********           MOVELQ1PGM     MBPGM
     C**********           MOVELDSTIME    MBTIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     MBDT
     C*
     C**********           ADD  1         CNT02   40
     C**********           ADD  1         XXCNT   60
     C**********           UPDATPMB01
     C*
     C**********           ENDIF                           *IN51 = ON
     C*
     C********** *IN52     IFEQ *ON
     C**********           ADD  1         CNT02   40
     C**********           ADD  1         XXCNT   60
     C********** GENKY     DELETPMB01                97
     C**********           ENDIF                           *IN52 = ON
     C*
     C********** GENKY     READEPMB01                    99
     C*
     C**********           ENDDO                           *IN99 = OFF
     C*
     C**********           ENDIF                           *IN50 = ON
     C*
     C*** END   DEL *** 02/10/00 **************************************
     C**************************************************************
     C*** FILE NASBP
     C**************************************************************
     C*
     C                   MOVEL(P)  'NASBP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPSB01
     C********** GENKY     READEPSB01                    99
     C********** *IN99     DOWEQ*OFF
     C     EOBKY         CHAIN     PSB01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        SBCO#
     C                   Z-ADD     CTTSUB        SBSUB#
     C                   Z-ADD     CTTFND        SBFND
     C                   Z-ADD     CTTFYR        SBFYR
     C                   Z-ADD     CTTCAS        SBCASE
     C                   MOVEL(P)  CTTSS#        SBSS#
     C                   Z-ADD     CTTACD        SBACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        SBUSER
     C                   MOVEL     Q1PGM         SBPGM
     C                   MOVEL     DSTIME        SBTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SBDT
     C*
     C                   ADD       1             CNT03             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PSB01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT03             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPSB01                97
     C                   DELETE    PSB01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPSB01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE NDTMP
     C**************************************************************
     C*
     C                   MOVEL(P)  'NDTMP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPTM01
     C********** GENKY     READEPTM01                    99
     C     EOBKY         CHAIN     PTM01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        TMCO#
     C                   Z-ADD     CTTSUB        TMSUB#
     C                   Z-ADD     CTTFND        TMFND
     C                   Z-ADD     CTTFYR        TMFYR
     C                   Z-ADD     CTTCAS        TMCASE
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
     C                   ADD       1             CNT04             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PTM01
     C                   endif
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT04             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPTM01                97
     C                   DELETE    PTM01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     READEPTM01                    99
     C     EOBKY         READE     PTM01                                  99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C**************************************************************
     C*** FILE FAEAP
     C**************************************************************
     C*
     C**********           MOVEL'FAEAP'   KKFILE    P
     C*
     C**********           EXSR UPDATE
     C********** *IN50     IFEQ *ON
     C*
     C********** GENKY     SETLLPEA01
     C********** GENKY     READEPEA01                    99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** *IN51     IFEQ *ON
     C*** BEGIN ADD *** 01/19/00 **************************************
     C********** YES       ANDEQ*OFF
     C*** END   ADD *** 01/19/00 **************************************
     C**********           Z-ADDCTTCO#    EACO#
     C**********           Z-ADDCTTSUB    EASUB#
     C**********           Z-ADDCTTFND    EAFND
     C**********           Z-ADDCTTFYR    EAFYR
     C**********           Z-ADDCTTCAS    EACASE
     C**********           MOVELCTTSS#    EASS#     P
     C**********           Z-ADDCTTACD    EAACDT
     C*
     C**********           TIME           DSDTTM
     C**********           MOVELQ1USER    EAUSER
     C**********           MOVELQ1PGM     EAPGM
     C**********           MOVELDSTIME    EATIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EADT
     C*
     C**********           ADD  1         CNT05   40
     C**********           ADD  1         XXCNT   60
     C**********           UPDATPEA01
     C*
     C**********           ENDIF                           *IN51 = ON
     C*
     C********** *IN52     IFEQ *ON
     C*** BEGIN ADD *** 01/19/00 **************************************
     C********** YES       OREQ *ON
     C*** END   ADD *** 01/19/00 **************************************
     C**********           ADD  1         CNT05   40
     C**********           ADD  1         XXCNT   60
     C********** GENKY     DELETPEA01                97
     C**********           ENDIF                           *IN52 = ON
     C*
     C********** GENKY     READEPEA01                    99
     C**********           ENDDO                           *IN99 = OFF
     C*
     C**********           ENDIF                           *IN50 = ON
     C*
     C*** END   DEL *** 02/10/00 **************************************
     C**************************************************************
     C*** FILE FAEDP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAEDP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPED01
     C********** GENKY     READEPED01                    99
     C********** *IN99     DOWEQ*OFF
     C     EOBKY         CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        EDCO#
     C                   Z-ADD     CTTSUB        EDSUB#
     C                   Z-ADD     CTTFND        EDFND
     C                   Z-ADD     CTTFYR        EDFYR
     C                   Z-ADD     CTTCAS        EDCASE
     C                   MOVEL(P)  CTTSS#        EDSS#
     C                   Z-ADD     CTTACD        EDACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EDUSER
     C                   MOVEL     Q1PGM         EDPGM
     C                   MOVEL     DSTIME        EDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EDDT
     C*
     C                   ADD       1             CNT06             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PED01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT06             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPED01                97
     C                   DELETE    PED01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPED01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAEEP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAEEP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPEE01
     C********** GENKY     READEPEE01                    99
     C     GENKY         CHAIN     PEE01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        EECO#
     C                   Z-ADD     CTTSUB        EESUB#
     C                   Z-ADD     CTTFND        EEFND
     C                   Z-ADD     CTTFYR        EEFYR
     C                   Z-ADD     CTTCAS        EECASE
     C                   MOVEL(P)  CTTSS#        EESS#
     C                   Z-ADD     CTTACD        EEACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EEUSER
     C                   MOVEL     Q1PGM         EEPGM
     C                   MOVEL     DSTIME        EETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C*
     C                   ADD       1             CNT07             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PEE01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT07             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPEE01                97
     C                   DELETE    PEE01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     READEPEE01                    99
     C     EOBKY         READE     PEE01                                  99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAEHP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAEHP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPEH01
     C********** GENKY     READEPEH01                    99
     C********** *IN99     DOWEQ*OFF
     C     EOBKY         CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        EHCO#
     C                   Z-ADD     CTTSUB        EHSUB#
     C                   Z-ADD     CTTFND        EHFND
     C                   Z-ADD     CTTFYR        EHFYR
     C                   Z-ADD     CTTCAS        EHCASE
     C                   MOVEL(P)  CTTSS#        EHSS#
     C                   Z-ADD     CTTACD        EHACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EHUSER
     C                   MOVEL     Q1PGM         EHPGM
     C                   MOVEL     DSTIME        EHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C                   ADD       1             CNT08             4 0
     C                   ADD       1             XXCNT             6 0
     C*
     C                   if        @@update = 'Y'
     C                   UPDATE    PEH01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT08             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPEH01                97
     C                   DELETE    PEH01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPEH01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAEPP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAEPP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPEP01
     C********** GENKY     READEPEP01                    99
     C********** *IN99     DOWEQ*OFF
     C     EOBKY         CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C*** ENDD  CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        EPCO#
     C                   Z-ADD     CTTSUB        EPSUB#
     C                   Z-ADD     CTTFND        EPFND
     C                   Z-ADD     CTTFYR        EPFYR
     C                   Z-ADD     CTTCAS        EPCASE
     C                   MOVEL(P)  CTTSS#        EPSS#
     C                   Z-ADD     CTTACD        EPACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   MOVEL     DSTIME        EPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   ADD       1             CNT09             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PEP01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT09             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPEP01                97
     C                   DELETE    PEP01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPEP01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAESP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAESP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPES01
     C********** GENKY     READEPES01                    99
     C     EOBKY         CHAIN     PES01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        ESCO#
     C                   Z-ADD     CTTSUB        ESSUB#
     C                   Z-ADD     CTTFND        ESFND
     C                   Z-ADD     CTTFYR        ESFYR
     C                   Z-ADD     CTTCAS        ESCASE
     C                   MOVEL(P)  CTTSS#        ESSS#
     C                   Z-ADD     CTTACD        ESACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ESUSER
     C                   MOVEL     Q1PGM         ESPGM
     C                   MOVEL     DSTIME        ESTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESDT
     C*
     C                   ADD       1             CNT10             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PES01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT10             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPES01                97
     C                   DELETE    PES01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     READEPES01                    99
     C     EOBKY         READE     PES01                                  99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAEUP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FAEUP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPEU01
     C********** GENKY     READEPEU01                    99
     C********** *IN99     DOWEQ*OFF
     C     GENKY         CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        EUCO#
     C                   Z-ADD     CTTSUB        EUSUB#
     C                   Z-ADD     CTTFND        EUFND
     C                   Z-ADD     CTTFYR        EUFYR
     C                   Z-ADD     CTTCAS        EUCASE
     C                   MOVEL(P)  CTTSS#        EUSS#
     C                   Z-ADD     CTTACD        EUACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EUUSER
     C                   MOVEL     Q1PGM         EUPGM
     C                   MOVEL     DSTIME        EUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDT
     C*
     C                   ADD       1             CNT11             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PEU01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT11             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPEU01                97
     C                   DELETE    PEU01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPEU01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C**************************************************************
     C*** FILE FAE1P
     C**************************************************************
     C*
     C**********           MOVEL'FAE1P'   KKFILE    P
     C*
     C**********           EXSR UPDATE
     C********** *IN50     IFEQ *ON
     C*
     C********** EOBKY     SETLLPE101
     C********** EOBKY     READEPE101                    99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** *IN51     IFEQ *ON
     C**********           Z-ADDCTTCO#    E1CO#
     C**********           Z-ADDCTTSUB    E1SUB#
     C**********           Z-ADDCTTFND    E1FND
     C**********           Z-ADDCTTFYR    E1FYR
     C**********           Z-ADDCTTCAS    E1CASE
     C*
     C**********           TIME           DSDTTM
     C**********           MOVELQ1USER    E1USER
     C**********           MOVELQ1PGM     E1PGM
     C**********           MOVELDSTIME    E1TIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     E1DT
     C*
     C**********           ADD  1         CNT12   40
     C**********           ADD  1         XXCNT   60
     C**********           UPDATPE101
     C*
     C**********           ENDIF                           *IN51 = ON
     C*
     C********** *IN52     IFEQ *ON
     C**********           ADD  1         CNT12   40
     C**********           ADD  1         XXCNT   60
     C********** EOBKY     DELETPE101                97
     C**********           ENDIF                           *IN52 = ON
     C*
     C********** EOBKY     READEPE101                    99
     C*
     C**********           ENDDO                           *IN99 = OFF
     C*
     C**********           ENDIF                           *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FAHBP
     C**************************************************************
     C*
     C**********           MOVEL'FAHBP'   KKFILE    P
     C*
     C**********           EXSR UPDATE
     C********** *IN50     IFEQ *ON
     C*
     C********** GENKY     SETLLPHB01
     C********** GENKY     READEPHB01                    99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** *IN51     IFEQ *ON
     C**********           Z-ADDCTTCO#    HBCO#
     C**********           Z-ADDCTTSUB    HBSUB#
     C**********           Z-ADDCTTFND    HBFND
     C**********           Z-ADDCTTFYR    HBFYR
     C**********           Z-ADDCTTCAS    HBCASE
     C**********           MOVELCTTSS#    HBSS#     P
     C**********           Z-ADDCTTACD    HBACDT
     C*
     C**********           TIME           DSDTTM
     C**********           MOVELQ1USER    HBUSER
     C**********           MOVELQ1PGM     HBPGM
     C**********           MOVELDSTIME    HBTIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     HBDT
     C*
     C**********           ADD  1         CNT13   40
     C**********           ADD  1         XXCNT
     C**********           UPDATPHB01
     C*
     C**********           ENDIF                           *IN51 = ON
     C*
     C********** *IN52     IFEQ *ON
     C**********           ADD  1         CNT13   40
     C**********           ADD  1         XXCNT
     C********** GENKY     DELETPHB01                97
     C**********           ENDIF                           *IN52 = ON
     C*
     C********** GENKY     READEPHB01                    99
     C*
     C**********           ENDDO                           *IN99 = OFF
     C*
     C**********           ENDIF                           *IN50 = ON
     C*** END   DEL *** 02/10/00 **************************************
     C**********
     C*
     C**************************************************************
     C*** FILE FDCAP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FDCAP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C                   ADD       1             HLDSEQ                         NEXT SEQ. #
     C*
     C     EOBKY         SETLL     PCA02
     C     EOBKY         READE     PCA02                                  99
     C*** BEGIN DEL *** 02/10/00 **************************************
     C*** BEGIN ADD *** 08/05/99 **************************************
     C**********           Z-ADDCADSEQ    OLDSEQ           OLD SEQ. #
     C*** END ADD *** 08/05/99 ****************************************
     C*** END   DEL *** 02/10/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   Z-ADD     CADSEQ        OLDSEQ                         OLD SEQ. #
     C*** END   ADD *** 02/10/00 **************************************
     C*
     C                   Z-ADD     CTTCO#        CACO#
     C                   Z-ADD     CTTSUB        CASUB#
     C                   Z-ADD     CTTFND        CAFND
     C                   Z-ADD     CTTFYR        CAFYR
     C                   Z-ADD     CTTCAS        CACASE
     C                   MOVEL(P)  CTTSS#        CASS#
     C                   Z-ADD     CTTACD        CAACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   MOVEL     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C**************************************************************
     C*** FILE FADUP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FADUP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPDU01
     C********   GENKY1    REDPEPDU01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDDUDSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1  50
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C*
     C***98                Z-ADDHLDSEQ    HLSEQ1  50
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPDU01
     C********** GENKY     READEPDU01                    99
     C********** *IN99     DOWEQ*OFF
     C     CAPKY         CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C*
     C                   Z-ADD     CTTCO#        DUCO#
     C                   Z-ADD     CTTSUB        DUSUB#
     C                   Z-ADD     CTTFND        DUFND
     C                   Z-ADD     CTTFYR        DUFYR
     C                   Z-ADD     CTTCAS        DUCASE
     C                   MOVEL(P)  CTTSS#        DUSS#
     C                   Z-ADD     CTTACD        DUACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     DSTIME        DUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    DUDSEQ           SEQ. NUMBER
     C   98DUDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        DUDSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT15             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PDU01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT15             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDU01                97
     C                   DELETE    PDU01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPDU01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FDDAP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FDDAP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPDA01
     C********   GENKY1    REDPEPDA01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDDADSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C*
     C***98                Z-ADDHLDSEQ    HLSEQ1
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPDA01
     C********** GENKY     READEPDA01                    99
     C     CAPKY         CHAIN     PDA01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        DACO#
     C                   Z-ADD     CTTSUB        DASUB#
     C                   Z-ADD     CTTFND        DAFND
     C                   Z-ADD     CTTFYR        DAFYR
     C                   Z-ADD     CTTCAS        DACASE
     C                   MOVEL(P)  CTTSS#        DASS#
     C                   Z-ADD     CTTACD        DAACDT
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
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    DADSEQ           SEQ. NUMBER
     C   98DADSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        DADSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT16             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PDA01
     C                   endif
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT16             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDA01                97
     C                   DELETE    PDA01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     READEPDA01                    99
     C     CAPKY         READE     PDA01                                  99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C**************************************************************
     C*** FILE FDDDP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FDDDP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPDD01
     C********   GENKY1    REDPEPDD01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDDDDSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C***98                Z-ADDHLDSEQ    HLSEQ1
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPDD01
     C********** GENKY     READEPDD01                    99
     C********** *IN99     DOWEQ*OFF
     C     CAPKY         CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        DDCO#
     C                   Z-ADD     CTTSUB        DDSUB#
     C                   Z-ADD     CTTFND        DDFND
     C                   Z-ADD     CTTFYR        DDFYR
     C                   Z-ADD     CTTCAS        DDCASE
     C                   MOVEL(P)  CTTSS#        DDSS#
     C                   Z-ADD     CTTACD        DDACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   MOVEL     DSTIME        DDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    DDDSEQ           SEQ. NUMBER
     C   98DDDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        DDDSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT17             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PDD01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT17             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDD01                97
     C                   DELETE    PDD01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPDD01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C**************************************************************
     C*** FILE FDDEP
     C**************************************************************
     C*
     C                   MOVEL(P)  'FDDEP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPDE01
     C********   GENKY1    REDPEPDE01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDDEDSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C*
     C***98                Z-ADDHLDSEQ    HLSEQ1
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPDE01
     C********** GENKY     READEPDE01                    99
     C     CAPKY         CHAIN     PDE01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        DECO#
     C                   Z-ADD     CTTSUB        DESUB#
     C                   Z-ADD     CTTFND        DEFND
     C                   Z-ADD     CTTFYR        DEFYR
     C                   Z-ADD     CTTCAS        DECASE
     C                   MOVEL(P)  CTTSS#        DESS#
     C                   Z-ADD     CTTACD        DEACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     DSTIME        DETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    DEDSEQ           SEQ. NUMBER
     C   98DEDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        DEDSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT18             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PDE01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT18             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDE01                97
     C                   DELETE    PDE01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     READEPDE01                    99
     C     CAPKY         READE     PDE01                                  99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*** BEGIN ADD *** 05/31/02 **************************************
     C**************************************************************
     C*** FILE FDCHP
     C**************************************************************
     C                   MOVEL(P)  'FDCHP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C     CAPKY         CHAIN     PCH01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
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
     C   98CHDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        DHDSEQ
     C*
     C                   ADD       1             CNT25             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PCH01
     C                   endif
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT25             4 0
     C                   ADD       1             XXCNT             6 0
     C                   DELETE    PCH01
     C                   ENDIF                                                  *IN52 = ON
     C     CAPKY         READE     PCH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C                   ENDIF                                                  *IN50 = ON
     C*** END   ADD *** 05/31/02 **************************************
     C**************************************************************
     C*** FILE FDDHP
     C**************************************************************
     C                   MOVEL(P)  'FDDHP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPDH01
     C********   GENKY1    REDPEPDH01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDDHDSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C***98                Z-ADDHLDSEQ    HLSEQ1
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPDH01
     C********** GENKY     READEPDH01                    99
     C********** *IN99     DOWEQ*OFF
     C     CAPKY         CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        DHCO#
     C                   Z-ADD     CTTSUB        DHSUB#
     C                   Z-ADD     CTTFND        DHFND
     C                   Z-ADD     CTTFYR        DHFYR
     C                   Z-ADD     CTTCAS        DHCASE
     C                   MOVEL(P)  CTTSS#        DHSS#
     C                   Z-ADD     CTTACD        DHACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     DSTIME        DHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    DHDSEQ           SEQ. NUMBER
     C   98DHDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        DHDSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT19             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PDH01
     C                   endif
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT19             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDH01                97
     C                   DELETE    PDH01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPDH01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C****************************************************************
     C*** FILE FDDPP
     C****************************************************************
     C                   MOVEL(P)  'FDDPP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPDP01
     C********   GENKY1    REDPEPDP01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDDPDSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C***98                Z-ADDHLDSEQ    HLSEQ1
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPDP01
     C********** GENKY     READEPDP01                    99
     C********** *IN99     DOWEQ*OFF
     C     CAPKY         CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        DPCO#
     C                   Z-ADD     CTTSUB        DPSUB#
     C                   Z-ADD     CTTFND        DPFND
     C                   Z-ADD     CTTFYR        DPFYR
     C                   Z-ADD     CTTCAS        DPCASE
     C                   MOVEL(P)  CTTSS#        DPSS#
     C                   Z-ADD     CTTACD        DPACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   MOVEL     DSTIME        DPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    DPDSEQ           SEQ. NUMBER
     C   98DPDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        DPDSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT20             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PDP01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT20             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDP01                97
     C                   DELETE    PDP01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPDP01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C****************************************************************
     C*** FILE FDDUP
     C****************************************************************
     C                   MOVEL(P)  'FDDUP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPDU02
     C********   GENKY1    REDPEPDU02               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDD2DSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C***98                Z-ADDHLDSEQ    HLSEQ1
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPDU02
     C********** GENKY     READEPDU02                    99
     C********** *IN99     DOWEQ*OFF
     C     CAPKY         CHAIN     PDU02                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        D2CO#
     C                   Z-ADD     CTTSUB        D2SUB#
     C                   Z-ADD     CTTFND        D2FND
     C                   Z-ADD     CTTFYR        D2FYR
     C                   Z-ADD     CTTCAS        D2CASE
     C                   MOVEL(P)  CTTSS#        D2SS#
     C                   Z-ADD     CTTACD        D2ACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        D2USER
     C                   MOVEL     Q1PGM         D2PGM
     C                   MOVEL     DSTIME        D2TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D2DT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    D2DSEQ           SEQ. NUMBER
     C   98D2DSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        D2DSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT21             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PDU02
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT21             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDU02                97
     C                   DELETE    PDU02
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPDU02                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C***************************************************************
     C*** FILE FDHCP
     C***************************************************************
     C                   MOVEL(P)  'FDHCP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPHC01
     C********   GENKY1    REDPEPHC01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDHCDSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C***98                Z-ADDHLDSEQ    HLSEQ1           DEL 09/09
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPHC01
     C********** GENKY     READEPHC01                    99
     C********** *IN99     DOWEQ*OFF
     C     CAPKY1        CHAIN     PHC01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        HCCO#
     C                   Z-ADD     CTTSUB        HCSUB#
     C                   Z-ADD     CTTFND        HCFND
     C                   Z-ADD     CTTFYR        HCFYR
     C                   Z-ADD     CTTCAS        HCCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        HCUSER
     C                   MOVEL     Q1PGM         HCPGM
     C                   MOVEL     DSTIME        HCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    HCDSEQ           SEQ. NUMBER
     C   98HCDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        HCDSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT22             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PHC01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT22             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPHC01                97
     C                   DELETE    PHC01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN DEL *** 02/10/00 **************************************
     C********** GENKY     READEPHC01                    99
     C*** END   DEL *** 02/10/00 **************************************
     C*** BEGIN CHG *** 02/10/00 **************************************
     C**********           ENDDO                           *IN99 = OFF
     C                   ENDIF                                                  *IN99 = OFF
     C*** END   CHG *** 02/10/00 **************************************
     C*
     C                   ENDIF                                                  *IN50 = ON
     C***************************************************************
     C*** FILE FDMGP
     C***************************************************************
     C                   MOVEL(P)  'FDMGP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C     *IN50         IFEQ      *ON
     C*** BEGIN DEL *** 08/05/99 **************************************
     C****BEG*ADD *** 09/09/98 ***********************************
     C********   *IN98     IFEQ *ON
     C********   GENKY1    SETGTPMG01
     C********   GENKY1    REDPEPMG01               N    99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDMGDSEQ    HLSEQ1
     C********             ELSE
     C********             Z-ADD*ZEROS    HLSEQ1
     C********             ENDIF
     C********             ENDIF
     C****END*ADD *** 09/09/98 ************************************
     C*** END DEL *** 08/05/99 ****************************************
     C***98                Z-ADDHLDSEQ    HLSEQ1
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     SETLLPMG01
     C********** GENKY     READEPMG01                    99
     C     CAPKY         CHAIN     PMG01                              99
     C*** END   CHG *** 02/10/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     CTTCO#        MGCO#
     C                   Z-ADD     CTTSUB        MGSUB#
     C                   Z-ADD     CTTFND        MGFND
     C                   Z-ADD     CTTFYR        MGFYR
     C                   Z-ADD     CTTCAS        MGCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MGUSER
     C                   MOVEL     Q1PGM         MGPGM
     C                   MOVEL     DSTIME        MGTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*
     C*** BEGIN CHG *** 08/05/99 **************************************
     C***98***             ADD  1         HLSEQ1
     C***98***             Z-ADDHLSEQ1    MGDSEQ           SEQ. NUMBER
     C   98MGDSEQ        SUB       OLDSEQ        WRKSEQ
     C   98WRKSEQ        ADD       HLDSEQ        MGDSEQ
     C*** END CHG *** 08/05/99 ****************************************
     C*
     C                   ADD       1             CNT23             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PMG01
     C                   endif
     C*
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT23             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPMG01                97
     C                   DELETE    PMG01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     READEPMG01                    99
     C     CAPKY         READE     PMG01                                  99
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C*
     C***************************************************************
     C****** UPDATE FDCAP FOR EACH EOB # ****************************
     C***************************************************************
     C*
     C   98              Z-ADD     HLDSEQ        CADSEQ                         SEQ. NUMBER
     C*
     C                   MOVEL(P)  'FDCAP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             CNT14             4 0
     C                   ADD       1             XXCNT1            6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PCA02                                        FDCAP
     C                   endif
     C*
     C   98              ADD       1             HLDSEQ
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C                   ADD       1             CNT14             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** EOBKY     DELETPCA02                97
     C                   DELETE    PCA02
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C     EOBKY         READE     PCA02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDIF                                                  *IN50 = ON
     C***************************************************************
     C****** UPDATE FAEOP FOR EACH EOB # ****************************
     C***************************************************************
     C*
     C                   MOVEL(P)  'FAEOP'       KKFILE
     C*
     C                   EXSR      UPDATE
     C*
     C     *IN51         IFEQ      *ON
     C*** BEGIN ADD *** 01/19/00 **************************************
     C     YES           ANDEQ     *OFF
     C*** END   ADD *** 01/19/00 **************************************
     C                   ADD       1             CNT01             4 0
     C                   ADD       1             XXCNT             6 0
     C                   if        @@update = 'Y'
     C                   UPDATE    PEO01                                60      FAEOP
     C                   endif
     C                   ENDIF                                                  *IN51 = ON
     C*
     C     *IN52         IFEQ      *ON
     C*** BEGIN ADD *** 01/19/00 **************************************
     C     YES           OREQ      *ON
     C*** END   ADD *** 01/19/00 **************************************
     C                   ADD       1             CNT01             4 0
     C                   ADD       1             XXCNT             6 0
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPEO01                97
     C                   DELETE    PEO01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF                                                  *IN52 = ON
     C*
     C     GENKY         READE     PEO01                                  60
     C                   ENDDO                                                  *IN60 = OFF
     C*
     C*
     C                   ENDIF                                                  *IN50 = ON
     C***************************************************************
     C*** THIS NEXT SECTION IS FOR A PENDED BILL
     C**************************************************************
     C     GENKY         SETLL     PDC01
     C*** BEG CHG *** 10/19/98 *************************************
     C     GENKY         READE     PDC01                                  61
     C********** GENKY     READEPDC01               N    61
     C*** END CHG *** 10/19/98 *************************************
     C     *IN61         DOWEQ     *OFF
     C*
     C     DCSTAT        IFEQ      'P'
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        HLDCS#            5 0
     C     GENKY1        SETGT     PDC01
     C     GENKY1        READPE(N) PDC01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DCDCS#        HLDCS#
     C                   ENDIF                                                  *IN99 = OFF
     C                   ADD       1             HLDCS#
     C                   ENDIF                                                  *IN98 = ON
     C***************************************************************
     C* NOW WE MUST CHAIN TO THE DETAIL CHARGE FILE WITH THE OLD CASE
     C* AND THEN GET THE NEXT AVAILABLE DSEQ FOR THE NEW CASE
     C*
     C     *IN98         IFEQ      '1'
     C     GENKY1        SETGT     PCA03
     C     GENKY1        READPE    PCA03                                  65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     CADSEQ        HLDSEQ
     C                   ELSE
     C                   Z-ADD     *ZEROS        HLDSEQ
     C                   END                                                    END *IN65
     C                   END                                                    END *IN98
     C*
     C     FENKY         SETLL     PCA01
     C     FENKY         READE     PCA01                                  66
     C     *IN66         DOWEQ     '0'
     C*
     C   98              ADD       1             HLDSEQ
     C*
     C* NOW THAT WE HAVE THE DSEQ WE MUST CHAIN TO THE FADUL RECORD
     C* AND UPDATE THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C     GENKY         SETLL     PDU01
     C     GENKY         READE     PDU01                                  67
     C     *IN67         DOWEQ     '0'
     C*
     C     *IN51         IFEQ      *ON
     C                   MOVE      CTTCO#        DUCO#
     C                   MOVE      CTTSUB        DUSUB#
     C                   MOVE      CTTFND        DUFND
     C                   MOVE      CTTFYR        DUFYR
     C                   MOVE      CTTCAS        DUCASE
     C                   MOVE      CTTSS#        DUSS#
     C                   MOVE      CTTACD        DUACDT
     C                   MOVE      Q1USER        DUUSER
     C                   MOVE      Q1PGM         DUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C   98              Z-ADD     HLDSEQ        DUDSEQ
     C                   ADD       1             CNT24             4 0
     C                   ADD       1             XXCNT
     C                   UPDATE    PDU01
     C                   ELSE
     C                   ADD       1             CNT15             4 0
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDU01                97
     C                   DELETE    PDU01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C     GENKY         READE     PDU01                                  67
     C                   END                                                    *IN67 = OFF
     C*
     C* NOW THAT WE HAVE THE DSEQ WE MUST CHAIN TO THE FDHCL RECORD
     C* AND UPDATE THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C     GENKY         SETLL     PHC01
     C     GENKY         READE     PHC01                                  68
     C     *IN68         DOWEQ     '0'
     C*
     C     *IN51         IFEQ      *ON
     C                   MOVE      CTTCO#        HCCO#
     C                   MOVE      CTTSUB        HCSUB#
     C                   MOVE      CTTFND        HCFND
     C                   MOVE      CTTFYR        HCFYR
     C                   MOVE      CTTCAS        HCCASE
     C                   MOVE      Q1USER        HCUSER
     C                   MOVE      Q1PGM         HCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        HCTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C   98              Z-ADD     HLDSEQ        HCDSEQ
     C                   ADD       1             CNT22             4 0
     C                   ADD       1             XXCNT
     C                   UPDATE    PHC01
     C                   ELSE
     C                   ADD       1             CNT22             4 0
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPHC01                97
     C                   DELETE    PHC01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C     GENKY         READE     PHC01                                  68
     C                   END                                                    *IN68 = OFF
     C*
     C* NOW THAT WE HAVE THE DSEQ WE MUST CHAIN TO THE FDCAL RECORD
     C* AND UPDATE THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C                   MOVE      CTTCO#        CACO#
     C                   MOVE      CTTSUB        CASUB#
     C                   MOVE      CTTFND        CAFND
     C                   MOVE      CTTFYR        CAFYR
     C                   MOVE      CTTCAS        CACASE
     C                   MOVE      CTTSS#        CASS#
     C                   MOVE      CTTACD        CAACDT
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C   98              Z-ADD     HLDCS#        CADCS#
     C   98              Z-ADD     HLDSEQ        CADSEQ
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             CNT14
     C                   ADD       1             XXCNT
     C                   UPDATE    PCA01
     C                   ELSE
     C                   ADD       1             CNT14
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** FENKY     DELETPCA01                97
     C                   DELETE    PCA01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C*
     C     FENKY         READE     PCA01                                  66
     C                   ENDDO                                                  *IN66 = OFF
     C*
     C*
     C* NOW THAT WE HAVE UP DATED ALL THE OTHER FILE WE MUST UPDATE
     C* THE FADCP RECORD WITH THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C                   MOVE      CTTCO#        DCCO#
     C                   MOVE      CTTSUB        DCSUB#
     C                   MOVE      CTTFND        DCFND
     C                   MOVE      CTTFYR        DCFYR
     C                   MOVE      CTTCAS        DCCASE
     C                   MOVE      Q1USER        DCUSER
     C                   MOVE      Q1PGM         DCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DCTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C   98              Z-ADD     HLDCS#        DCDCS#
     C     *IN51         IFEQ      *ON
     C                   ADD       1             CNT24             4 0
     C                   ADD       1             XXCNT
     C                   UPDATE    PDC01
     C                   ELSE
     C                   ADD       1             CNT24             4 0
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDC01                97
     C                   DELETE    PDC01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C*
     C                   MOVE      DCFEIN        KKFEIN
     C                   MOVE      DCMOD         KKMOD
     C                   MOVE      DCDCS#        KKDCS#
     C*
     C* IF DCSTAT IS NOT PENDED THEN MAKE SURE TO CASE DOES NOT HAVE
     C* CURRENT CHARGES FOR IT WITH SAME PROVIDER AS FROM CASE
     C*
     C                   ELSE                                                   DCSTAT NE P
     C                   Z-ADD     DCFEIN        KFFEIN            9 0
     C                   Z-ADD     DCMOD         KFMOD             3 0
     C                   MOVE      '0'           *IN99
     C*
     C     FENKY1        SETLL     PDC01
     C     FENKY1        READE(N)  PDC01                                  58
     C     *IN58         IFEQ      '0'
     C     DCSTAT        ANDEQ     ' '
     C                   MOVE      '1'           *IN99
     C     GENKY         READE(N)  PDC01                                  58
     C                   ENDIF
     C*
     C     *IN98         IFEQ      '1'
     C                   Z-ADD     *ZEROS        HLDCS#            5 0
     C     GENKY1        SETGT     PDC01
     C     GENKY1        READPE(N) PDC01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     DCDCS#        HLDCS#
     C     GENKY1        READPE(N) PDC01                                  60
     C                   ENDDO                                                  END DO 60
     C                   ADD       1             HLDCS#
     C                   ENDIF                                                  END *IN98
     C*
     C* NOW WE MUST CHAIN TO THE DETAIL CHARGE FILE WITH THE OLD CASE
     C* AND THEN GET THE NEXT AVAILABLE DSEQ FOR THE NEW CASE
     C*
     C     *IN98         IFEQ      '1'
     C     GENKY1        SETGT     PCA03
     C     GENKY1        READPE    PCA03                                  65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     CADSEQ        HLDSEQ
     C                   ELSE
     C                   Z-ADD     *ZEROS        HLDSEQ
     C                   END
     C                   END                                                    END *IN98
     C*
     C     FENKY         SETLL     PCA01
     C     FENKY         READE     PCA01                                  66
     C     *IN66         DOWEQ     '0'
     C*
     C   98              ADD       1             HLDSEQ
     C*
     C* NOW THAT WE HAVE THE DSEQ WE MUST CHAIN TO THE FADUL RECORD
     C* AND UPDATE THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C     GENKY         SETLL     PDU01
     C     GENKY         READE     PDU01                                  67
     C     *IN67         DOWEQ     '0'
     C     *IN51         IFEQ      *ON
     C                   MOVE      CTTCO#        DUCO#
     C                   MOVE      CTTSUB        DUSUB#
     C                   MOVE      CTTFND        DUFND
     C                   MOVE      CTTFYR        DUFYR
     C                   MOVE      CTTCAS        DUCASE
     C                   MOVE      CTTSS#        DUSS#
     C                   MOVE      CTTACD        DUACDT
     C                   MOVE      Q1USER        DUUSER
     C                   MOVE      Q1PGM         DUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C   98              Z-ADD     HLDSEQ        DUDSEQ
     C                   ADD       1             CNT15
     C                   ADD       1             XXCNT
     C                   UPDATE    PDU01
     C                   ELSE
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDU01                97
     C                   DELETE    PDU01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ADD       1             CNT15
     C                   ADD       1             XXCNT
     C                   ENDIF
     C     GENKY         READE     PDU01                                  67
     C                   ENDDO
     C*
     C* NOW THAT WE HAVE THE DSEQ WE MUST CHAIN TO THE FDHCL RECORD
     C* AND UPDATE THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C     GENKY         SETLL     PHC01
     C     GENKY         READE     PHC01                                  68
     C     *IN68         DOWEQ     '0'
     C     *IN51         IFEQ      *ON
     C                   MOVE      CTTCO#        HCCO#
     C                   MOVE      CTTSUB        HCSUB#
     C                   MOVE      CTTFND        HCFND
     C                   MOVE      CTTFYR        HCFYR
     C                   MOVE      CTTCAS        HCCASE
     C                   MOVE      Q1USER        HCUSER
     C                   MOVE      Q1PGM         HCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        HCTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C   98              Z-ADD     HLDSEQ        HCDSEQ
     C                   ADD       1             CNT22
     C                   ADD       1             XXCNT
     C                   UPDATE    PHC01
     C                   ELSE
     C                   ADD       1             CNT22
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPHC01                97
     C                   DELETE    PHC01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C     GENKY         READE     PHC01                                  68
     C                   ENDDO
     C*
     C* NOW THAT WE HAVE THE DSEQ WE MUST CHAIN TO THE FDCAL RECORD
     C* AND UPDATE THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C                   MOVE      CTTCO#        CACO#
     C                   MOVE      CTTSUB        CASUB#
     C                   MOVE      CTTFND        CAFND
     C                   MOVE      CTTFYR        CAFYR
     C                   MOVE      CTTCAS        CACASE
     C                   MOVE      CTTSS#        CASS#
     C                   MOVE      CTTACD        CAACDT
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C   98              Z-ADD     HLDCS#        CADCS#
     C   98              Z-ADD     HLDSEQ        CADSEQ
     C     *IN51         IFEQ      *ON
     C                   ADD       1             CNT14
     C                   ADD       1             XXCNT
     C                   UPDATE    PCA01
     C                   ELSE
     C                   ADD       1             CNT14
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** FENKY     DELETPCA01                97
     C                   DELETE    PCA01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C     FENKY         READE     PCA01                                  66
     C                   ENDDO
     C*
     C*
     C* NOW THAT WE HAVE UP DATED ALL THE OTHER FILE WE MUST UPDATE
     C* THE FADCP RECORD WITH THE FROM CASE WITH THE NEW CASE/DCS#/DSEQ
     C*
     C     GENKY         READE     PDC01                                  58
     C     *IN58         IFEQ      *OFF
     C                   MOVE      CTTCO#        DCCO#
     C                   MOVE      CTTSUB        DCSUB#
     C                   MOVE      CTTFND        DCFND
     C                   MOVE      CTTFYR        DCFYR
     C                   MOVE      CTTCAS        DCCASE
     C                   MOVE      Q1USER        DCUSER
     C                   MOVE      Q1PGM         DCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DCTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C   98              Z-ADD     HLDCS#        DCDCS#
     C     *IN51         IFEQ      *ON
     C                   ADD       1             CNT24
     C                   ADD       1             XXCNT
     C                   UPDATE    PDC01
     C                   ELSE
     C                   ADD       1             CNT24
     C                   ADD       1             XXCNT
     C*** BEGIN CHG *** 02/10/00 **************************************
     C********** GENKY     DELETPDC01                97
     C                   DELETE    PDC01
     C*** END   CHG *** 02/10/00 **************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  END DCSTAT=P
     C     GENKY         READE     PDC01                                  61
     C*
     C                   ENDDO                                                  END *IN61
     C*
     C*** BEG DEL *** 02/05/99 ***************************************
     C**********           EXSR PRINT
     C*** END DEL *** 02/05/99 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATE - Determine if record should be put into file
     C*================================================================
     C     UPDATE        BEGSR
     C*
     C                   Z-ADD     *ZEROS        OOCNT             4 0
     C                   SETON                                        50
     C*
     C*
     C     @@TD          IFEQ      'T'
     C                   SETON                                        51
     C                   ENDIF                                                  T1STAT = T
     C*
     C     @@TD          IFEQ      'D'
     C                   SETON                                        52
     C                   ENDIF                                                  T1STAT = D
     C*
     C                   MOVE      *OFF          *IN99
     C*
     C                   ENDSR
     C*================================================================
     C* PRINT SUBROUTINE
     C*================================================================
     C*
     C     PRINT         BEGSR
     C*
     C     *LOVAL        SETLL     PT301
     C                   READ      PT301                                  99
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL     T3DESC        OODESC           35
     C                   SELECT
     C*
     C     T3FILE        WHENEQ    'FAEOP'
     C                   MOVEL     'FAEOP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT01         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'NAMBP'
     C                   MOVEL     'NAMBP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT02         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'NASBP'
     C                   MOVEL     'NASBP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT03         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'NDTMP'
     C                   MOVEL     'NDTMP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT04         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEAP'
     C                   MOVEL     'FAEAP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT05         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEDP'
     C                   MOVEL     'FAEDP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT06         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEEP'
     C                   MOVEL     'FAEEP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT07         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEHP'
     C                   MOVEL     'FAEHP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT08         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEPP'
     C                   MOVEL     'FAEPP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT09         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAESP'
     C                   MOVEL     'FAESP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT10         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEUP'
     C                   MOVEL     'FAEUP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT11         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAE1P'
     C                   MOVEL     'FAE1P'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT12         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAHBP'
     C                   MOVEL     'FAHBP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT13         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDCAP'
     C                   MOVEL     'FDCAP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT14         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FADUP'
     C                   MOVEL     'FADUP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT15         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDAP'
     C                   MOVEL     'FDDAP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT16         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDDP'
     C                   MOVEL     'FDDDP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT17         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDEP'
     C                   MOVEL     'FDDEP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT18         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 05/31/02 **************************************
     C*
     C     T3FILE        WHENEQ    'FDCHP'
     C                   MOVEL     'FDCHP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT25         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C     T3FILE        WHENEQ    'FDDHP'
     C                   MOVEL     'FDDHP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT19         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDPP'
     C                   MOVEL     'FDDPP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT20         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDUP'
     C                   MOVEL     'FDDUP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT21         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDHCP'
     C                   MOVEL     'FDHCP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT22         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDMGP'
     C                   MOVEL     'FDMGP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT23         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FADCP'
     C                   MOVEL     'FADCP'       KKFILE
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   SETOFF                                         10
     C                   Z-ADD     CNT24         OOCNT
     C     OOCNT         IFNE      *ZEROS
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   READ      PT301                                  99
     C                   ENDDO
     C*
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
     O*** BEGIN CHG *** 11/19/99 **************************************
     O********        N52                 5 'FROM:'
     O              N53                           5 'FROM:'
     O********         52                 8 'DELETED:'
     O               53                           8 'DELETED:'
     O*** END CHG *** 11/19/99 ****************************************
     O                       OOHD1               88
     O          E            HEAD           2
     O                       OOHD2               88
     O          E            HEAD           1
     O*** BEGIN CHG *** 11/19/99 **************************************
     O********        N52                 3 'TO:'
     O              N53                           3 'TO:'
     O********        N52      OOHD3     88
     O              N53      OOHD3               88
     O*** END CHG *** 11/19/99 ****************************************
     O          E            HEAD           2
     O*** BEGIN CHG *** 11/19/99 **************************************
     O********        N52      OOHD4     88
     O              N53      OOHD4               88
     O*** END CHG *** 11/19/99 ****************************************
     O*
     O          E            DETH        1  1
     O                                            5 'FILE:'
     O                       OOFILE         B    16
     O                       OODESC         B    57
     O                                           56 '==>'
     O                                           71 'FILE PROCESSED'
