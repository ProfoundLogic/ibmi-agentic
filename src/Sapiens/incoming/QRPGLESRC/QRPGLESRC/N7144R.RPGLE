     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N7144R - RE-BILL UPDATE PROGRAM
     H*****************************************************************
     H**   PROGRAM: N7144R                                           **
     H**   DESCRIPTION: UPDATES SEVERAL FILES FOR REBILL PURPOSES    **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  01/22/02 C:62 PROJECT:AR033                                 **
     H*                MODIFY FOR NETWORK PROCESSING.                **
     H*                                                              **
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**  99              RECORD TO BE DELETED                       **
     H** N99              RECORD NOT DELETED, ONLY UPDATED           **
     H*****************************************************************
     FFAEAL1    UF   E           K DISK
     F*
     F*** BEGIN DEL *** 01/22/02 **************************************
     F**********FAEAL13 IF  E           K        DISK
     F**********  PEA01                             KRENAMEPEA02
     F*** END   DEL *** 01/22/02 **************************************
     FFWEAP     UF   E           K DISK
     F                                     RENAME(PEA01:PEW01)
     FFNEAP     UF   E           K DISK
     F                                     RENAME(PEA01:PEN01)
     FFAEOL4    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FNDTML1    UF   E           K DISK
     F*
     FNMARL6    UF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FNMARP     O    E             DISK

     D*
     D DS              S             30    DIM(5) CTDATA PERRCD(1)              OFFSET DESC.
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*
     IPEW01
     I              EABAMT                      EWBAMT
     I              EABCAM                      EWBCAM
     I              EABDIF                      EWDIFL
     I              EACHIN                      EWCHIN
     IPEN01
     I              EANAMT                      ENNAMT
     I              EANCAM                      ENNCAM
     I              EANDIF                      ENDIFL
     I              EACHIN                      ENCHIN
     IPEO02
     I              EOCO#                       E2CO#
     I              EOFND                       E2FND
     I              EOFYR                       E2FYR
     I              EOCASE                      E2CASE
     I              EOTRDT                      E2TRDT
     I              EOFEIN                      E2FEIN
     I              EOMOD                       E2MOD
     I              EOAMT                       E2AMT
     I              EOFRDT                      EFFRDT
     I              EOTODT                      E2TODT
     I              EOTRCD                      E2TRCD
     I              EOPNAM                      E2PNAM
     I              EOADR1                      E2ADR1
     I              EOADR2                      E2ADR2
     I              EOCTY                       E2CTY
     I              EOST                        E2ST
     I              EOZIP                       E2ZIP
     I              EOEOB1                      E2EOB1
     I              EOEOB2                      E2EOB2
     I              EOEOB3                      E2EOB3
     I              EOCHK#                      E2CHK#
     I              EOEOB#                      E2EOB#
     I              EOSYS                       E2SYS
     I              EOFLG1                      E2FLG1
     I              EOFLG2                      E2FLG2
     I              EOFLG3                      E2FLG3
     I              EOFLG4                      E2FLG4
     I              EOFLG5                      E2FLG5
     I              EOFLG6                      E2FLG6
     I              EOFLG7                      E2FLG7
     I              EOFLG8                      E2FLG8
     I              EOFLG9                      E2FLG9
     I              EOAMTC                      E2AMTC
     I              EOSTAT                      E2STAT
     I              EOUSER                      E2USER
     I              EOPGM                       E2PGM
     I              EOTIME                      E2TIME
     I              EODT                        E2DT
     I              EOIUSR                      E2IUSR
     I              EOIDT                       E2IDT
     I              EOWCCK                      E2WCCK
     I              EODIA1                      E2DIA1
     I              EODIA2                      E2DIA2
     I              EODIA3                      E2DIA3
     I              EODIA4                      E2DIA4
     I              EOEMPL                      E2EMPL
     I              EODFC                       E2DFC
     I              EOFLIC                      E2FLIC
     I              EOTAPE                      E2TAPE
     I              EO#BLL                      E2#BLL
     I              EOCLSF                      E2CLSF
     I              EOAST                       E2AST
     I              EOSUB#                      E2SUB#
     I              EOAUDT                      E2AUDT
     I              EOBTFL                      E2BTFL
     I              EOHRFL                      E2HRFL
     I              EOBLFL                      E2BLFL
     I              EONLFL                      E2NLFL
     I              EODIFL                      E2DIFL
     I              EOINV#                      E2INV#
     I              EORMDS                      E2RMDS
     I              EORMK1                      E2RMK1
     I              EORMK2                      E2RMK2
     I              EORMK3                      E2RMK3
     I              EOCKCN                      E2CKCN
     I              EOOTHR                      E2OTHR
     I              EOSB01                      E2SB01
     I              EOSB02                      E2SB02
     I              EOSB03                      E2SB03
     I              EOSB04                      E2SB04
     I              EOSB05                      E2SB05
     I              EOSB06                      E2SB06
     I              EOSB07                      E2SB07
     I              EOSB08                      E2SB08
     I              EOSB09                      E2SB09
     I              EOSB10                      E2SB10
     I              EOBAMT                      E2BAMT
     I              EOZIPA                      E2ZIPA
     I              EOCERT                      E2CERT
     I              EOEXT                       E2EXT

     C*
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXINV#
     C                   KFLD                    XXEOB#
     C*
     C* KEY LIST FOR THE EOMB FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    HLFYR
     C                   KFLD                    HLCASE
     C*
     C* KEY LIST FOR THE T/M FILE
     C*
     C     TMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    HLFYR
     C                   KFLD                    HLCASE
     C                   KFLD                    XXMEOB
     C                   KFLD                    XXRCN
     C                   KFLD                    XXSEQ#
     C*
     C     TMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    HLFYR
     C                   KFLD                    HLCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*
     C*   KEY LIST FOR THE EOB MASTER FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXEOB#
     C*****
     C****KEY LIST FOR THE NETWORK MASTER FILE
     C*****
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** EAKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXINV#
     C**********           KFLD           XXEOB#
     C*** END   DEL *** 01/22/02 **************************************
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    HLFYR
     C                   KFLD                    HLCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*
     C*   PARAMETER LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO              3
     C                   PARM                    QQSUB             5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQINV             9
     C                   PARM                    QQDATE            8            INVOICE DATE
     C                   PARM                    QQMODE            1            SYSTEM TYPE
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
     C* INITIALIZATION SECTION
     C     *LIKE         DEFINE    EAEOB#        XXEOB#
     C     *LIKE         DEFINE    EAEOB#        XXMEOB
     C     *LIKE         DEFINE    EASYS         XXSYS
     C     *LIKE         DEFINE    EAFYR         HLFYR
     C     *LIKE         DEFINE    EACASE        HLCASE
     C     *LIKE         DEFINE    EABAMT        HLBAMT
     C     *LIKE         DEFINE    EARCN         XXRCN
     C*
     C                   Z-ADD     *ZEROS        XXSEQ#            5 0
     C*
     C                   MOVE      QQCO          XXCO#             3 0
     C                   MOVE      QQSUB         XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQINV         XXINV#            9 0
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   TIME                    DSDTTM           12 0
     C*
     C** IF QQMODE = "N" THEN DO NETWORK MAINLINE...ELSE DO PBR/HBA MAINLINE
     C     QQMODE        IFEQ      'N'
     C*********************************
     C* NETWORK RE-BILL MAINLINE CODE *
     C*********************************
     C                   READ      PEN01                                  60
     C**
     C     *IN60         DOWEQ     '0'
     C                   MOVE      EAEOB#        XXEOB#
     C                   MOVE      EASYS         XXSYS
     C                   Z-ADD     EAFYR         HLFYR
     C                   Z-ADD     EACASE        HLCASE
     C                   Z-ADD     EARCN         XXRCN
     C*** BEGIN CHG *** 01/22/02 **************************************
     C********** EAKEY     CHAINPEA02               N61
     C     EAKY1         CHAIN     PEA01                              61
     C*** END   CHG *** 01/22/02 **************************************
     C     *IN61         IFEQ      '0'
     C     ENNAMT        IFEQ      EANAMT
     C*** BEGIN ADD *** 10/20/99 **************************************
     C     ENNCAM        ANDEQ     EANCAM
     C     ENCHIN        ANDEQ     *BLANKS
     C*** END   ADD *** 10/20/99 **************************************
     C     ENDIFL        ANDEQ     *BLANKS
     C                   EXCEPT    RLSPEN
     C                   GOTO      NXTNET
     C*
     C                   ELSE
     C*
     C                   MOVEL     'Y'           ENCHIN
     C                   UPDATE    PEN01
     C*
     C                   END
     C                   END
     C*
     C** EODIFL INDICATES WHETHER THIS RECORD IS DELETED OR NOT
     C     ENDIFL        IFNE      *BLANKS
     C                   MOVE      '1'           *IN99
     C     ENNAMT        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   END
     C**
     C                   EXSR      SRNOFF
     C**
     C     NXTNET        TAG
     C                   READ      PEN01                                  60
     C                   END
     C*
     C                   ELSE
     C*********************************
     C* PBR/HBA RE-BILL MAINLINE CODE *
     C*********************************
     C                   READ      PEW01                                  60
     C**
     C     *IN60         DOWEQ     '0'
     C                   MOVE      EAEOB#        XXEOB#
     C                   MOVE      EASYS         XXSYS
     C                   Z-ADD     EAFYR         HLFYR
     C                   Z-ADD     EACASE        HLCASE
     C                   Z-ADD     EARCN         XXRCN
     C     EAKY1         CHAIN     PEA01                              62
     C     *IN62         IFEQ      '0'
     C     EWBAMT        IFEQ      EABAMT
     C*** BEGIN ADD *** 10/20/99 **************************************
     C     EWBCAM        ANDEQ     EABCAM
     C     EWCHIN        ANDEQ     *BLANKS
     C*** END   ADD *** 10/20/99 **************************************
     C     EWDIFL        ANDEQ     *BLANKS
     C                   EXCEPT    RLSPEW
     C                   GOTO      NXTRCD
     C*
     C                   ELSE
     C*
     C                   MOVEL     'Y'           EWCHIN
     C                   UPDATE    PEW01
     C*
     C                   END
     C                   END
     C*** END ADD *** 04/05/93 ****************************************
     C*
     C** EODIFL INDICATES WHETHER THIS RECORD IS DELETED OR NOT
     C     EWDIFL        IFNE      *BLANKS
     C                   MOVE      '1'           *IN99
     C     EWBAMT        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   END
     C**
     C*          *IN99     IFEQ *ON
     C                   EXSR      SROFF
     C*                    ENDIF
     C**
     C     NXTRCD        TAG
     C                   READ      PEW01                                  60
     C                   END
     C*********************************
     C**
     C                   END                                                    END EQ "N"
     C*********************************
     C* END OF PROGRAM                *
     C*********************************
     C                   MOVE      '1'           *INLR
     C**
     C*****************************************************************--*
     C*  WRITE OFFSET A/R RECORD, UPDATE OTHER FILES ( NOT NETWORK)
     C*****************************************************************--*
     C*
     C     SROFF         BEGSR
     C*
     C                   Z-ADD     0             XXAMT
     C     ARKEY2        CHAIN     PAR02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* UPDATE ORIGINAL A/R RECORD WITH 'O' STATUS FOR OFFSET
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C                   MOVE      'O'           ARSTAT                         RCRD OFFSET
     C                   UPDATE    PAR02
     C*** BEG ADD *** 02/24/99 **************************************
     C                   ADD       ARAMT         XXAMT             9 2
     C*** END ADD *** 02/24/99 **************************************
     C                   END                                                    ENDEQ I/B
     C* ACCUMULATE ALL AMOUNTS FOR THIS INV/EOB TO FIND VALUE
     C* TO WRITE OFFSETTING ENTRY
     C*
     C***************************************************************
     C*    N O T E  *************************************************
     C*
     C*    THE REASON THAT THIS HAD TO BE MOVED IS BECAUSE WITH
     C*    GROUP CHECK POSTING, WE BEGAN TO PUT EOB# INTO THE
     C*    PAYMENT AND ADJUSTMENT RECORDS IN NMARP.  THIS RESULTED
     C*    IN GETTING PAYMENT AND ADJUSTMENT RECORDS ADDED INTO THE
     C*    OFFSET RECORD'S AMOUNT.  BY MOVING THIS LINE, IT
     C*    WILL ONLY ALLOW 'I' TYPE OF RECORDS.
     C*
     C***************************************************************
     C*** BEG DEL *** 02/24/99 **************************************
     C**********           ADD  ARAMT     XXAMT   92
     C*** END DEL *** 02/24/99 **************************************
     C     ARKEY2        READE     PAR02                                  60
     C                   END                                                    END DOW *IN60
     C*
     C     XXAMT         IFNE      *ZERO
     C*
     C* OUTPUT OFFSET A/R RECORD WITH 'D' STATUS FOR DELETE
     C*
     C     ARSYS         IFEQ      'H'
     C                   MOVEL     DS(1)         ARDESC                         HBA OFFSET
     C                   ELSE
     C     ARSYS         IFEQ      'P'
     C                   MOVEL     DS(2)         ARDESC                         PBR OFFSET
     C                   ELSE
     C     ARSYS         IFEQ      'U'
     C                   MOVEL     DS(4)         ARDESC
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE
     C     ARSYS         IFEQ      'M'
     C                   MOVEL     DS(5)         ARDESC
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END
     C                   END
     C                   END
     C*
     C*** BEG ADD *** 02/24/99 ***************************************
     C                   MOVE      *BLANKS       ARTRCD
     C                   MOVE      'I'           ARTRTP
     C*** END ADD *** 02/24/99 ***************************************
     C                   Z-SUB     XXAMT         ARAMT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   MOVE      *ZERO         ARCK#
     C                   MOVE      'D'           ARSTAT                         DELT OFFSET
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   WRITE     PAR01                                        XXAMT NE 0
     C                   END
     C*
     C** IF EOB WAS DELETED FROM INVOICE IN F7146R, RESET STATUS
     C*
     C     *IN99         IFEQ      '1'
     C*
     C     XXSYS         IFEQ      'M'
     C*
     C* CHAIN TO EOMB FILE TO SEE IF EXISTING OPEN 'M' TYPE RECORD
     C*
     C                   Z-ADD     *ZEROS        XXMEOB
???  C     EOKY          CHAIN     PEO02                              40
 |   C     *IN40         DOWEQ     '0'
 |   C     E2BLFL        IFNE      'P'
 |   C     E2SYS         ANDEQ     'M'
 |   C                   Z-ADD     E2EOB#        XXMEOB
 |   C                   END                                                    END E2BLFL NE 'P'
\|/  C     EOKY          READE     PEO02                                  40
???  C                   END                                                    END DOWEQ '0'
     C*
     C* IF XXMEOB IS NOT ZEROS, OPEN 'M' RECORD FOR THIS CASE EXISTS AND WE MUST
     C* ADD THESE T/M CHARGES TO OPEN EOMB.
     C*
     C     XXMEOB        IFNE      *ZEROS
     C*
     C* GET THE NEXT AVAILABLE SEQUENCE NUMBER FOR OPEN 'M' RECORD
     C*
     C                   MOVE      *HIVAL        XXSEQ#
     C     TMKY          SETGT     PTM01
     C                   READP(N)  PTM01                                  60
     C     *IN60         IFEQ      '0'
     C     TMCO#         ANDEQ     XXCO#
     C     TMSUB#        ANDEQ     XXSUB#
     C     TMFND         ANDEQ     XXFND
     C     TMFYR         ANDEQ     HLFYR
     C     TMCASE        ANDEQ     HLCASE
     C     TMEOB#        ANDEQ     XXMEOB
     C     TMRCN         ANDEQ     XXRCN
     C                   Z-ADD     TMSEQ#        XXSEQ#
     C                   ELSE
     C                   Z-ADD     *ZERO         XXSEQ#
     C                   END
     C*
     C* CHAIN WITH DELETED EOMB NUMBER TO UPDATE T/M CHARGES TO NEW EOMB
     C*
     C     TMKY1         CHAIN     PTM01                              35
     C     *IN35         DOWEQ     '0'
     C*
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXMEOB        TMEOB#
     C                   Z-ADD     XXRCN         TMRCN
     C                   Z-ADD     XXSEQ#        TMSEQ#
     C                   MOVEL     Q1PGM         TMPGM
     C                   MOVEL     Q1USER        TMUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C                   Z-ADD     DSTIME        TMTIME
     C                   UPDATE    PTM01
     C*
     C     TMKY1         READE     PTM01                                  35
     C                   END
     C*
     C*
     C* RESET EOB RECORD TO PRE-BILLING STATUS
     C*
     C     EAKY1         CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** EASYS     IFEQ 'N'
WDW  C**********           MOVE 'H'       EANHRF           H/R STAT
     C**********           MOVE 'B'       EANBLF           BILLED STAT
     C**********           MOVE ' '       EANDIF           DELETE STAT
     C**********           MOVE *ZEROS    EANINV           INV#
     C**********           UPDATPEA01
     C**********           ELSE
     C*** END   DEL *** 01/22/02 **************************************
WDW  C                   MOVE      'H'           EABHRF                         H/R STAT
     C                   MOVE      'B'           EABBLF                         BILLED STAT
     C                   MOVE      ' '           EABDIF                         DELETE STAT
     C                   MOVE      *ZEROS        EABINV                         INV#
     C                   UPDATE    PEA01
     C*** BEGIN DEL *** 01/22/02 **************************************
     C**********           ENDIF
     C*** END   DEL *** 01/22/02 **************************************
     C                   END                                                    ENDEQ *IN60
     C*
     C                   GOTO      SROFFE
     C                   END                                                    END XXMEOB NE *ZEROS
     C*
     C                   END                                                    END XXSYS EQ 'M'
     C*
     C* RESET EOB RECORD TO PRE-BILLING STATUS
     C*
     C     EAKY1         CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** EASYS     IFEQ 'N'
     C**********           MOVE 'H'       EANHRF           H/R STAT
     C**********           MOVE ' '       EANBLF           BILLED STAT
     C**********           MOVE ' '       EANDIF           DELETE STAT
     C**********           MOVE *ZEROS    EANINV           INV#
     C**********           UPDATPEA01
     C**********           ELSE
     C*** END   DEL *** 01/22/02 **************************************
     C                   MOVE      'H'           EABHRF                         H/R STAT
     C                   MOVE      ' '           EABBLF                         BILLED STAT
     C                   MOVE      ' '           EABDIF                         DELETE STAT
     C                   MOVE      *ZEROS        EABINV                         INV#
     C                   UPDATE    PEA01
     C*** BEGIN DEL *** 01/22/02 **************************************
     C**********           ENDIF
     C*** END   DEL *** 01/22/02 **************************************
     C                   END                                                    ENDEQ *IN60
     C*
     C     *IN98         IFEQ      '1'
     C*
     C     EAKY1         CHAIN     PEA01                              61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     EWBAMT        EABAMT
     C*** BEGIN ADD *** 10/20/99 **************************************
     C                   Z-ADD     EWBCAM        EABCAM
     C*** END   ADD *** 10/20/99 **************************************
     C                   UPDATE    PEA01
     C                   END                                                    ENDEQ *IN61
     C                   END                                                    ENDEQ *IN98
     C*
     C*
     C                   ELSE                                                   ENDEQ *IN99
     C*
     C* OTHERWISE, UPDATE THE INVOICE AMT FROM THE REBILL WORK FILE
     C*
     C     EAKY1         CHAIN     PEA01                              61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     EWBAMT        EABAMT
     C*** BEGIN ADD *** 10/20/99 **************************************
     C                   Z-ADD     EWBCAM        EABCAM
     C*** END   ADD *** 10/20/99 **************************************
     C                   UPDATE    PEA01
     C                   END                                                    ENDEQ *IN61
     C                   END                                                    ENDEQ *IN99
     C*
     C     SROFFE        ENDSR
     C*
     C*****************************************************************--*
     C*  WRITE OFFSET A/R RECORD, UPDATE OTHER FILES ( FOR NETWORK)
     C*****************************************************************--*
     C*
     C     SRNOFF        BEGSR
     C*
     C                   Z-ADD     0             XXAMT
     C     ARKEY2        CHAIN     PAR02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* UPDATE ORIGINAL A/R RECORD WITH 'O' STATUS FOR OFFSET
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C                   MOVE      'O'           ARSTAT                         RCRD OFFSET
     C                   UPDATE    PAR02
     C                   ADD       ARAMT         XXAMT             9 2
     C                   END                                                    ENDEQ I/B
     C*
     C* ACCUMULATE ALL AMOUNTS FOR THIS INV/EOB TO FIND VALUE
     C* TO WRITE OFFSETTING ENTRY
     C*
     C***************************************************************
     C*    N O T E  *************************************************
     C*
     C*    THE REASON THAT THIS HAD TO BE MOVED IS BECAUSE WITH
     C*    GROUP CHECK POSTING, WE BEGAN TO PUT EOB# INTO THE
     C*    PAYMENT AND ADJUSTMENT RECORDS IN NMARP.  THIS RESULTED
     C*    IN GETTING PAYMENT AND ADJUSTMENT RECORDS ADDED INTO THE
     C*    OFFSET RECORD'S AMOUNT.  BY MOVING THIS LINE, IT
     C*    WILL ONLY ALLOW 'I' TYPE OF RECORDS.
     C*
     C***************************************************************
     C     ARKEY2        READE     PAR02                                  60
     C                   END                                                    END DOW *IN60
     C*
     C     XXAMT         IFNE      *ZERO
     C*
     C* OUTPUT OFFSET A/R RECORD WITH 'D' STATUS FOR DELETE
     C*
     C                   MOVE      *BLANKS       ARTRCD
     C                   MOVE      'I'           ARTRTP
     C                   MOVEL     DS(3)         ARDESC
     C                   Z-SUB     XXAMT         ARAMT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   MOVE      *ZERO         ARCK#
     C                   MOVE      'D'           ARSTAT                         DELT OFFSET
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   WRITE     PAR01
     C                   END                                                    XXAMT NE 0
     C*** BEGIN ADD *** 01/22/02 **************************************
     C*
     C** IF EOB WAS DELETED FROM INVOICE IN F7146R, RESET STATUS
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     EAKY1         CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'H'           EANHRF                         H/R STAT
     C                   MOVE      ' '           EANBLF                         BILLED STAT
     C                   MOVE      ' '           EANDIF                         DELETE STAT
     C                   MOVE      *ZEROS        EANINV                         INV#
     C                   MOVE      *ZEROS        EANIDT                         INV#
     C                   UPDATE    PEA01
     C                   ENDIF                                                  ENDEQ *IN99
     C*
     C                   ELSE
     C*
     C* OTHERWISE, UPDATE THE INVOICE AMT FROM THE REBILL WORK FILE
     C*
     C     EAKY1         CHAIN     PEA01                              61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     ENNAMT        EANAMT
     C                   Z-ADD     ENNCAM        EANCAM
     C                   UPDATE    PEA01
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/22/02 **************************************
     C                   ENDSR
     OPEW01     E            RLSPEW
     OPEN01     E            RLSPEN
** (DS)
OFFSET BILLING HBA
OFFSET BILLING PBR
OFFSET BILLING NETWORK
OFFSET BILLING UR
OFFSET BILLING MEDICAL MGMT
