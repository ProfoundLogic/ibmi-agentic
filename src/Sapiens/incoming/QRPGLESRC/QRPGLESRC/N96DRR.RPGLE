     H*****************************************************************
     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*                                                               *
     H*****************************************************************
     H/TITLE N96DRR - DR discount code - Medicare Physician
     H*****************************************************************
     H*                                                               *
     H*  Programmer: C:198              Project: 114MWS               *
     H*  Created: 08/12/10                                            *
     H*****************************************************************
     H** Revisions:
     H*
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*
     H*
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK    INFDS(CAFDS)
     FFTABL     IF   E           K DISK    usropn
     FFTEBLMC   IF   E           K DISK    rename(peb01:pebmc) prefix(mc:2)
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDHL     IF   E           K DISK
     FFDMGL6    UF A E           K DISK
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     DCAFDS            DS
     DCARRN                  397    400B 0
     D*
     D                 ds
     D  qqcmd                  1     56
     D  qqst                  40     41
     D                 ds
     D kkkey           S                   like(ebkey)
     D @@rrn           S              9  0
     Dqca              s             56    dim(2) ctdata perrcd(1)
     C*  CASE DETAIL RECORDS
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXFDAT
     C                   KFLD                    XXDSEQ
     C*
     C*  EOMB TRANSACTION FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
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
     C*  EOMB PHYS/HCFA DETAIL FILE
     C*
     C     DHKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C     ebky          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    kkkey
     C*
     C* INITIALIZE SECTION
     C*
     C     *LIKE         DEFINE    BDAM01        XXAM01
     C     *LIKE         DEFINE    BDPC01        XXPC01
     C     *LIKE         DEFINE    BDDT          XXFDAT
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DHPRCD        KKPRCD
     C     *LIKE         DEFINE    DHPRCD        XXPRCD
     C     *LIKE         DEFINE    DHPOS         XXPOS
     C     *LIKE         DEFINE    DHUNTS        XXUNTS
     C     *LIKE         DEFINE    DHMOD1        XXMOD1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQCODE            3
     C                   PARM                    QQCEDT            8
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFL1             1
     C                   PARM                    QQFL2             1
     C                   PARM                    QQFL3             1
     C                   PARM                    QQFL4             1
     C                   PARM                    QQFL5             1
     C                   PARM                    QQDS1            10
     C                   PARM                    QQDS2            10
     C                   PARM                    QQDS3            10
     C                   PARM                    QQDS4            10
     C                   PARM                    QQDS5            10
     C                   PARM                    QQRTCD            1
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C*
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCEDT        XXEDAT            8 0
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   Z-ADD     *ZEROS        XXMMRA            9 2
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     EOKY          CHAIN     PEO01                              99
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
     C                   move      carrn         @@rrn
     C*
     C                   call      'FA763R'
     C                   parm                    @@rrn
     C                   parm      'N'           @@call            1
     C                   parm                    @@eob             3            return var
     C                   parm                    @@msg             7            return var
     C                   parm      'Y'           @@modr            1
     C                   parm      'M'           @@rnd             1
     C                   parm                    @@alw1            9 2          return var
     C                   parm                    @@alw2            9 2          return var
     C                   parm                    @@alw3            9 2          return var
     C                   parm                    @@rtcd            1            return var
     C*
     C* FIND OUT IF MULTI-LINE OR NOT
     C*
     C     *IN99         IFEQ      *OFF
     C                   CALL      'SA014R'
1    C                   PARM      CAAST         ##ST              2
2    C                   PARM      EOBTYP        ##BTYP            1
3    C                   PARM      CAFDAT        ##FDAT            7 0
4    C                   PARM      *BLANK        ##SPLT            1
     C                   ENDIF
     C*
     C*
     C     EOSYS         IFEQ      'P'
     C  N99DHKY          CHAIN     PDH01                              99
     C                   MOVEL(P)  DHPRCD        KKPRCD
     C                   MOVEL(P)  DHPRCD        XXPRCD
     C                   MOVEL(P)  DHPOS         XXPOS
     C                   MOVEL(P)  DHMOD1        XXMOD1
     C                   Z-ADD     DHUNTS        XXUNTS
     C                   ENDIF                                                  EOSYS = 'P'
     C*
     C* BE SURE CLAIM STATUS CODE IS NOT "C" FOR CAP ON EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C*
     C* FOR EACH DETAIL RECORD READ, FIND THE DISCOUNT MASTER RECORD
     C* THAT APPLYS TO THAT DATE OF SERVICE.
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  60
     C*
     C* VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN60         IFEQ      '0'
     C     BDCODE        IFNE      QQCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN60 OFF
     C*
     C* IF A VALID DISCOUNT RECORD IS NOT FOUND, ZERO THE DISCOUNT
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        CANDIS
     C                   GOTO      UPDTCA
     C                   END                                                    *IN60 ON
     C*
     C* Initially load XXMMRA with Carve Out Amount
     C                   eval      xxmmra = @@alw1
     C* IF A VALID DISCOUNT RECORD WAS FOUND GET discount percnt(BDPC01)
     C                   Z-ADD     BDPC01        XXPC01
     C                   eval(r)   xxmmra = xxmmra * bdpc01
     C*
     C* IF NO SPLIT OUT OF LINES THEN MULTIPLY BY UNITS
     C*
     C     ##SPLT        IFEQ      'N'
     C*** Need to multiply by units if necessary
     C                   if        caunts > 1 and casrc <> 'A'
     C                   if        cacpt4 >= '00100' and cacpt4 <= '01999'
     C                   else
     C                   eval      xxmmra = caunts * xxmmra
     C                   endif
     C                   endif
     C                   ENDIF
     C*
     C                   EXSR      PMNT
     C*
     C                   END                                                     END *IN60 EQ '0'
     C*
     C* UPDATE THE DETAIL CHARGE RECORD
     C*
     C     UPDTCA        TAG
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C     CANDIS        IFNE      *ZEROS
     C*
     C                   EXSR      REMMSG
     C*
     C                   if        @@eob <>  ' '
     C                   exsr      wrteob
     C                   endif
     C                   if        @@msg <>  ' '
     C                   exsr      wrtmsg
     C                   endif
     C*
     C***                if        candis < 0
     C***                eval      candis = 0
     C***                endif
     C* GET NEXT DETAIL SEQUENCE NUMBER
     C*
     C                   movel(p)  qca(2)        qqcmd
     C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length           15 5
     C*
     C                   close     ftabl
     C*
     C                   UPDATE    PCA01
     C*
     C                   END                                                     CLM STS CD NE 'C'
     C*
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C     wrtmsg        begsr
     C*
     C                   Z-ADD     CACO#         MGCO#
     C                   Z-ADD     CASUB#        MGSUB#
     C                   Z-ADD     CAFND         MGFND
     C                   Z-ADD     CAFYR         MGFYR
     C                   Z-ADD     CACASE        MGCASE
     C                   Z-ADD     CADCS#        MGDCS#
     C                   Z-ADD     CADSEQ        MGDSEQ
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      CAEOB#        MGEOB#
     C                   MOVEL     @@MSG         MGMSG#
     C                   MOVE      *BLANKS       MGMSFX
     C                   MOVE      *BLANKS       MGCSI
     C                   MOVE      'N'           MGSRC
     C                   MOVE      'A'           MGSTAT
     C                   MOVE      CAUSER        MGUSER
     C                   MOVE      CAPGM         MGPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        MGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVE      MGUSER        MGIUSR
     C                   MOVE      MGPGM         MGIPGM
     C                   Z-ADD     MGTIME        MGITIM
     C                   Z-ADD     MGDT          MGIDT
     C                   MOVE      CARCN         MGRCN
     C                   WRITE     PMG01
     C*
     C                   endsr
     C*******************************************************************************
     C     wrteob        begsr
     C*
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
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      @@EOB         DEEOB
     C                   MOVEL     @@EOB         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C* load denot1 from ftebpmc else form ftabl
     C                   movel     @@eob         kkkey
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
     C*
     C                   endsr
     *********************************************************************
     *** Payment - Compare CO and charged. Write message/EOMB code P M N T
     *********************************************************************
     C     PMNT          BEGSR
     C*
     C* Carve out < Charged amount
     C*
     C                   eval      candis = 0
     C*
     C     XXMMRA        IFLT      CACHRG
     C*
     C                   Z-ADD     XXMMRA        CAPAID
     C                   ELSE
     C                   Z-ADD     CACHRG        CAPAID
     C                   endif
     C*
     C     cachrg        SUB       capaid        CANDIS
     C     candis        SUB       camra         CANDIS
     C*
     C                   ENDSR
     *********************************************************************
     *** Remove  - Remove adjudication messages/EOMB Codes  R E M M S GN T
     *********************************************************************
     C     REMMSG        BEGSR
     C*
     C* Remove adjudication messages from Prov bills errors
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
     C***  DEEOB         ANDEQ     'W1'
     C                   DELETE    PDE01
     C                   ENDIF                                                  ESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C                   ENDSR
**  TABLE QCA
OVRDBF FILE(FTEBP)   TOFILE(*LIBL/FTEBP  )
DLTOVR FILE(FTEBP)
