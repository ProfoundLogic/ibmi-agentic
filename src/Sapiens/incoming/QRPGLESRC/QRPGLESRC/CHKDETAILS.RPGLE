     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE CHKDETAILS - CHECKS PBILLS DETAIL FILES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM CHECKS PBILLS DETAIL FILES        *
     H*                                                               *
     H*  CALLED FROM: CHKHEADERS                                      *
     H*  CALLS TO   : NONE                                            *
     H*                                                               *
     H*  CREATED: 08/03/01  PROGRAMMER: C:62  PROJECT: MC246          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/24/13 C:287 SY266_OVR                                      *
     H*                Change ACTGRP to "*CALLER"                     *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  GENERAL CHAIN/READ USAGE                                *
     H*****************************************************************
     FFAEOL4    IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFDCAL13   IF   E           K DISK
     FFDDAL1    IF   E           K DISK
     FFDDDL     IF   E           K DISK
     FFDDEL     IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDPL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFTABL     IF   E           K DISK    USROPN
     F                                     IGNORE(PPS01)
     F                                     IGNORE(PRC01)
     FFMPDL     IF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN
     FFTPSL     IF   E           K DISK    USROPN
     FFMRCL1    IF   E           K DISK
     FFMRCL     IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     FFMRXL     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN
     FFTBSL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FSTSTL1    IF   E           K DISK
     FCVTERR    O    E             DISK
     F*

     D ER1             S              5    DIM(6) CTDATA PERRCD(1)              File Errors
     D ED1             S             40    DIM(6) ALT(ER1)
     D ER2             S              5    DIM(20) CTDATA PERRCD(1)             Amount Errors
     D ED2             S             40    DIM(20) ALT(ER2)
     D CAF             S              5    DIM(18) CTDATA PERRCD(1)             FDCAP Field Errors
     D CAD             S             40    DIM(18) ALT(CAF)
     D DAF             S              5    DIM(6) CTDATA PERRCD(1)              FDDAP Field Errors
     D DAD             S             40    DIM(6) ALT(DAF)
     D DDF             S              5    DIM(11) CTDATA PERRCD(1)             FDDDP Field Errors
     D DDD             S             40    DIM(11) ALT(DDF)
     D DEF             S              5    DIM(7) CTDATA PERRCD(1)              FDDEP Field Errors
     D DED             S             40    DIM(7) ALT(DEF)
     D DHF             S              5    DIM(21) CTDATA PERRCD(1)             FDDHP Field Errors
     D DHD             S             40    DIM(21) ALT(DHF)
     D DPF             S              5    DIM(7) CTDATA PERRCD(1)              FDDPP Field Errors
     D DPD             S             40    DIM(7) ALT(DPF)
     D DUF             S              5    DIM(8) CTDATA PERRCD(1)              FDDUP Field Errors
     D DUD             S             40    DIM(8) ALT(DUF)
     D EOF             S              5    DIM(2) CTDATA PERRCD(1)              FAEOP Field Errors
     D EOD             S             40    DIM(2) ALT(EOF)
     D QCA             S             56    DIM(8) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS                  INZ
     D  XXCMD                  1     56
     D  XXST                  38     39
     D*
     D                 DS                  INZ
     D  YYCMD                  1     56
     D  YYST                  40     41
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3
     D  XXRATE                 4      8
     D  XXUNTS                 9     11    INZ('000')

     C*****************************************************************
     C* Parameter lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
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
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C* Key lists.
     C*****************************************************************
     C     BSKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C*
     C     GENKY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     GTKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     GZKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    KKFRZP
     C*
     C     GZKY2         KLIST
     C                   KFLD                    CACO#
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     PRKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKCPT4
     C*
     C     PSKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKPOS
     C*
     C     RCKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKCPT4
     C*
     C     RCKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C*
     C     RXKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKNDC
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     SVKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKCMSV
     C                   KFLD                    KKEFDT
     C*
     C     SVKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKCMSV
     C*
     C     TBKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKKEY
     C*****************************************************************
     C*  Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    DACHRG        ZZCHRG
     C     *LIKE         DEFINE    DACHRG        XXCHRG
     C     *LIKE         DEFINE    DAPAID        XXPAID
     C     *LIKE         DEFINE    DAOTHR        XXOTHR
     C     *LIKE         DEFINE    DAOTH2        XXOTH2
     C     *LIKE         DEFINE    DANDIS        XXNDIS
     C     *LIKE         DEFINE    DAMRA         XXMRA
     C     *LIKE         DEFINE    DACHRG        X1CHRG
     C     *LIKE         DEFINE    DAPAID        X1PAID
     C     *LIKE         DEFINE    DAOTHR        X1OTHR
     C     *LIKE         DEFINE    DAOTH2        X1OTH2
     C     *LIKE         DEFINE    DANDIS        X1NDIS
     C     *LIKE         DEFINE    DAMRA         X1MRA
     C     *LIKE         DEFINE    DACHRG        X2CHRG
     C     *LIKE         DEFINE    DAPAID        X2PAID
     C     *LIKE         DEFINE    DAOTHR        X2OTHR
     C     *LIKE         DEFINE    DAOTH2        X2OTH2
     C     *LIKE         DEFINE    DANDIS        X2NDIS
     C     *LIKE         DEFINE    DAMRA         X2MRA
     C     *LIKE         DEFINE    ERFILE        XXFILE
     C     *LIKE         DEFINE    ERFIEL        XXFIEL
     C     *LIKE         DEFINE    ERVAL         XXVAL
     C     *LIKE         DEFINE    ERMSG#        XXMSG#
     C     *LIKE         DEFINE    ERDSEQ        XXDSEQ
     C     *LIKE         DEFINE    ERLIN#        XXLIN#
     C     *LIKE         DEFINE    ERRCN         XXRCN
     C     *LIKE         DEFINE    ERDESC        XXDESC
     C     *LIKE         DEFINE    CAFDAT        XXFDAT
     C     *LIKE         DEFINE    CATDAT        XXTDAT
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    CSKEY         KKKEY
     C     *LIKE         DEFINE    CDSS#         KKSS#
     C     *LIKE         DEFINE    GZZIPA        KKZIPA
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    RXNDC         KKNDC
     C     *LIKE         DEFINE    SVCMSV        KKCMSV
     C     *LIKE         DEFINE    PSPOS         KKPOS
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     *ZEROS        X1CHRG
     C                   Z-ADD     *ZEROS        X1PAID
     C                   Z-ADD     *ZEROS        X1OTHR
     C                   Z-ADD     *ZEROS        X1OTH2
     C                   Z-ADD     *ZEROS        X1NDIS
     C                   Z-ADD     *ZEROS        X1MRA
     C*
     C                   Z-ADD     *ZEROS        X2CHRG
     C                   Z-ADD     *ZEROS        X2PAID
     C                   Z-ADD     *ZEROS        X2OTHR
     C                   Z-ADD     *ZEROS        X2OTH2
     C                   Z-ADD     *ZEROS        X2NDIS
     C                   Z-ADD     *ZEROS        X2MRA
     C*
     C                   Z-ADD     *HIVAL        XXFDAT
     C                   Z-ADD     *ZEROS        XXTDAT
     C                   MOVEL(P)  '*YES'        OPENF             4
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *ON
     C     EORCN         IFGT      *ZEROS
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(1)        XXMSG#
     C                   MOVE      ED1(1)        XXDESC
     C                   Z-ADD     *ZEROS        CADSEQ
     C                   Z-ADD     *ZEROS        CALIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ELSE
     C     *IN99         DOWEQ     '0'
     C*
     C     OPENF         IFEQ      '*YES'
     C                   EXSR      OVRPRP
     C                   EXSR      OVRPSP
     C                   EXSR      OVRSVP
     C                   EXSR      OVRTAB
     C                   EXSR      OPNPRP
     C                   EXSR      OPNPSP
     C                   EXSR      OPNSVP
     C                   EXSR      OPNTAB
     C                   MOVEL(P)  '*NO'         OPENF
     C                   ENDIF
     C*
     C                   EXSR      CHKCAP
     C                   EXSR      CHKDAP
     C                   EXSR      CHKDEP
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      CHKDDP
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      CHKDUP
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      CHKDHP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      CHKDPP
     C                   ENDSL
     C*
     C* GET NEXT PC FILE RECORD
     C*
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C     EOBLST        IFEQ      'C'
     C*
     C     X1CHRG        IFNE      EOCHRG
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CACHRG'      XXFIEL
     C                   MOVEL     X1CHRG        XXONE            11
     C                   MOVEL     EOCHRG        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT       XXTWO:1       XXVAL
     C                   MOVE      ER2(9)        XXMSG#
     C                   MOVEL(P)  ED2(9)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X1PAID        IFNE      EOPAID
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAPAID'      XXFIEL
     C                   MOVEL     X1PAID        XXONE            11
     C                   MOVEL     EOPAID        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(10)       XXMSG#
     C                   MOVEL(P)  ED2(10)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X1OTHR        IFNE      EOOTHR
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAOTHR'      XXFIEL
     C                   MOVEL     X1OTHR        XXONE            11
     C                   MOVEL     EOOTHR        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(11)       XXMSG#
     C                   MOVEL(P)  ED2(11)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X1OTH2        IFNE      EOOTH2
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAOTH2'      XXFIEL
     C                   MOVEL     X1OTH2        XXONE            11
     C                   MOVEL     EOOTH2        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(12)       XXMSG#
     C                   MOVEL(P)  ED2(12)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X1NDIS        IFNE      EONDIS
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CANDIS'      XXFIEL
     C                   MOVEL     X1NDIS        XXONE            11
     C                   MOVEL     EONDIS        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(13)       XXMSG#
     C                   MOVEL(P)  ED2(13)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X1MRA         IFNE      EOMRA
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAMRA '      XXFIEL
     C                   MOVEL     X1MRA         XXONE            11
     C                   MOVEL     EOMRA         XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(14)       XXMSG#
     C                   MOVEL(P)  ED2(14)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* EOFRDT MUST MATCH OLDEST CAFDAT
     C*
     C     XXFDAT        IFNE      EOFRDT
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOFRDT'      XXFIEL
     C                   MOVE      EOFRDT        XXDAT1            7
     C                   MOVE      XXFDAT        XXDAT2            7
     C     XXDAT1        CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXDAT2:1      XXVAL
     C                   MOVE      EOF(1)        XXMSG#
     C                   MOVEL(P)  EOD(1)        XXDESC
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* EOTODT MUST MATCH MOST RECENT CATDAT
     C*
     C     XXTDAT        IFNE      EOTODT
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOTODT'      XXFIEL
     C                   MOVE      EOTODT        XXDAT1            7
     C                   MOVE      XXTDAT        XXDAT2            7
     C     XXDAT1        CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXDAT2:1      XXVAL
     C                   MOVE      EOF(2)        XXMSG#
     C                   MOVEL(P)  EOD(2)        XXDESC
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     EAEPFL        ANDEQ     'P'
     C*
     C     X2CHRG        IFNE      EACHRG
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DACHRG'      XXFIEL
     C                   MOVEL     X2CHRG        XXONE            11
     C                   MOVEL     EACHRG        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(15)       XXMSG#
     C                   MOVEL(P)  ED2(15)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X2PAID        IFNE      EAPAID
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAPAID'      XXFIEL
     C                   MOVEL     X2PAID        XXONE            11
     C                   MOVEL     EAPAID        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(16)       XXMSG#
     C                   MOVEL(P)  ED2(16)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X2OTHR        IFNE      EAOTHR
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAOTHR'      XXFIEL
     C                   MOVEL     X2OTHR        XXONE            11
     C                   MOVEL     EAOTHR        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(17)       XXMSG#
     C                   MOVEL(P)  ED2(17)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X2OTH2        IFNE      EAOTH2
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAOTH2'      XXFIEL
     C                   MOVEL     X2OTH2        XXONE            11
     C                   MOVEL     EAOTH2        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(18)       XXMSG#
     C                   MOVEL(P)  ED2(18)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X2NDIS        IFNE      EANDIS
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DANDIS'      XXFIEL
     C                   MOVEL     X2NDIS        XXONE            11
     C                   MOVEL     EANDIS        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(19)       XXMSG#
     C                   MOVEL(P)  ED2(19)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     X2MRA         IFNE      EAMRA
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAMRA '      XXFIEL
     C                   MOVEL     X2MRA         XXONE            11
     C                   MOVEL     EAMRA         XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(20)       XXMSG#
     C                   MOVEL(P)  ED2(20)       XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     OPENF         IFEQ      '*NO'
     C                   EXSR      CLSPRP
     C                   EXSR      CLSPSP
     C                   EXSR      CLSSVP
     C                   EXSR      CLSTAB
     C                   EXSR      DOVPRP
     C                   EXSR      DOVPSP
     C                   EXSR      DOVSVP
     C                   EXSR      DOVTAB
     C                   ENDIF
     C*
     C                   RETURN
     C***************************************************************
     C* SUBROUTINE TO CHECK FDCAP RECORD
     C***************************************************************
     C     CHKCAP        BEGSR
     C*
     C                   EXSR      WRNCAP
     C*
     C     CAFDAT        IFLT      XXFDAT
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   ENDIF
     C*
     C     CATDAT        IFGT      XXTDAT
     C                   Z-ADD     CATDAT        XXTDAT
     C                   ENDIF
     C*
     C                   ADD       CACHRG        X1CHRG
     C                   ADD       CAMRA         X1MRA
     C                   ADD       CAOTHR        X1OTHR
     C                   ADD       CAOTH2        X1OTH2
     C                   ADD       CANDIS        X1NDIS
     C                   ADD       CAPAID        X1PAID
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FDDAP RECORD
     C***************************************************************
     C     CHKDAP        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXCHRG
     C                   Z-ADD     *ZEROS        XXPAID
     C                   Z-ADD     *ZEROS        XXOTHR
     C                   Z-ADD     *ZEROS        XXOTH2
     C                   Z-ADD     *ZEROS        XXNDIS
     C                   Z-ADD     *ZEROS        XXMRA
     C*
     C     GENKY         CHAIN     PDA01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(2)        XXMSG#
     C                   MOVEL(P)  ED1(2)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ELSE
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRNDAP
     C*
     C                   ADD       DACHRG        XXCHRG
     C                   ADD       DAMRA         XXMRA
     C                   ADD       DAOTHR        XXOTHR
     C                   ADD       DAOTH2        XXOTH2
     C                   ADD       DANDIS        XXNDIS
     C                   ADD       DAPAID        XXPAID
     C                   ADD       DACHRG        X2CHRG
     C                   ADD       DAMRA         X2MRA
     C                   ADD       DAOTHR        X2OTHR
     C                   ADD       DAOTH2        X2OTH2
     C                   ADD       DANDIS        X2NDIS
     C                   ADD       DAPAID        X2PAID
     C*
     C     GENKY         READE     PDA01                                  99
     C                   ENDDO
     C*
     C     XXCHRG        IFNE      CACHRG
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DACHRG'      XXFIEL
     C                   MOVEL     XXCHRG        XXONE            11
     C                   MOVEL     CACHRG        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(3)        XXMSG#
     C                   MOVEL(P)  ED2(3)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     XXPAID        IFNE      CAPAID
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAPAID'      XXFIEL
     C                   MOVEL     XXPAID        XXONE            11
     C                   MOVEL     CAPAID        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(4)        XXMSG#
     C                   MOVEL(P)  ED2(4)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     XXOTHR        IFNE      CAOTHR
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAOTHR'      XXFIEL
     C                   MOVEL     XXOTHR        XXONE            11
     C                   MOVEL     CAOTHR        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(5)        XXMSG#
     C                   MOVEL(P)  ED2(5)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     XXOTH2        IFNE      CAOTH2
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAOTH2'      XXFIEL
     C                   MOVEL     XXOTH2        XXONE            11
     C                   MOVEL     CAOTH2        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(6)        XXMSG#
     C                   MOVEL(P)  ED2(6)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     XXNDIS        IFNE      CANDIS
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DANDIS'      XXFIEL
     C                   MOVEL     XXNDIS        XXONE            11
     C                   MOVEL     CANDIS        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(7)        XXMSG#
     C                   MOVEL(P)  ED2(7)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C     XXMRA         IFNE      CAMRA
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAMRA '      XXFIEL
     C                   MOVEL     XXMRA         XXONE            11
     C                   MOVEL     CAMRA         XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT(P)    XXTWO:1       XXVAL
     C                   MOVE      ER2(8)        XXMSG#
     C                   MOVEL(P)  ED2(8)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FDDDP RECORD
     C***************************************************************
     C     CHKDDP        BEGSR
     C*
     C     GENKY         CHAIN     PDD01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(3)        XXMSG#
     C                   MOVEL(P)  ED1(3)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ELSE
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRNDDP
     C*
     C     GENKY         READE     PDD01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FADEP RECORD
     C***************************************************************
     C     CHKDEP        BEGSR
     C*
     C     GENKY         CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRNDEP
     C*
     C     GENKY         READE     PDE01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FDDHP RECORD
     C***************************************************************
     C     CHKDHP        BEGSR
     C*
     C     GENKY         CHAIN     PDH01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(4)        XXMSG#
     C                   MOVEL(P)  ED1(4)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ELSE
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRNDHP
     C*
     C     GENKY         READE     PDH01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FDDPP RECORD
     C***************************************************************
     C     CHKDPP        BEGSR
     C*
     C     GENKY         CHAIN     PDP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(5)        XXMSG#
     C                   MOVEL(P)  ED1(5)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ELSE
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRNDPP
     C*
     C     GENKY         READE     PDP01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FDDUP RECORD
     C***************************************************************
     C     CHKDUP        BEGSR
     C*
     C     GENKY         CHAIN     PDU01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDUP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(6)        XXMSG#
     C                   MOVEL(P)  ED1(6)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ELSE
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRNDUP
     C*
     C     GENKY         READE     PDU01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FDCAP
     C***************************************************************
     C     WRNCAP        BEGSR
     C*
     C* Amounts should crossfoot
     C*
     C     CACHRG        SUB       CAPAID        ZZCHRG
     C                   SUB       CAOTHR        ZZCHRG
     C                   SUB       CAOTH2        ZZCHRG
     C                   SUB       CANDIS        ZZCHRG
     C                   SUB       CAMRA         ZZCHRG
     C*
     C     ZZCHRG        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER2(1)        XXMSG#
     C                   MOVEL(P)  ED2(1)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* ADJUDICATION CODE IS INVALID
     C*
     C                   MOVEL(P)  CACSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CACSCD'      XXFIEL
     C                   MOVEL(P)  CACSCD        XXVAL
     C                   MOVE      CAF(1)        XXMSG#
     C                   MOVEL(P)  CAD(1)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID FROM DATE
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAFDAT'      XXFIEL
     C                   MOVEL(P)  CAFDAT        XXVAL
     C                   MOVE      CAF(2)        XXMSG#
     C                   MOVEL(P)  CAD(2)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID TO DATE
     C*
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CATDAT'      XXFIEL
     C                   MOVEL(P)  CATDAT        XXVAL
     C                   MOVE      CAF(3)        XXMSG#
     C                   MOVEL(P)  CAD(3)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID RECIEVE DATE
     C*
     C                   Z-ADD     CARDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CARDAT'      XXFIEL
     C                   MOVEL(P)  CARDAT        XXVAL
     C                   MOVE      CAF(4)        XXMSG#
     C                   MOVEL(P)  CAD(4)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* FEIN MUST BE VALID
     C*
     C                   Z-ADD     CAFEIN        KKFEIN
     C                   Z-ADD     CAMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAFEIN'      XXFIEL
     C                   MOVEL(P)  CAFEIN        XXVAL
     C                   MOVE      CAF(5)        XXMSG#
     C                   MOVEL(P)  CAD(5)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* PROVIDER TYPE MUST MATCH THE PROVIDERS TYPE
     C*
     C     CAPRTY        IFEQ      '1'
     C     EOBTYP        ANDNE     'N'
     C     CAPRTY        OREQ      '4'
     C     EOBTYP        ANDNE     'H'
     C     CAPRTY        OREQ      ' '
     C     EOBTYP        ANDNE     'D'
     C     EOBTYP        ANDNE     'X'
     C     CAPRTY        ORNE      '1'
     C     CAPRTY        ANDNE     '4'
     C     CAPRTY        ANDNE     ' '
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAPRTY'      XXFIEL
     C                   MOVEL(P)  CAPRTY        XXVAL
     C                   MOVE      CAF(6)        XXMSG#
     C                   MOVEL(P)  CAD(6)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C*
     C* DETAIL SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     CADSEQ        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CADSEQ'      XXFIEL
     C                   MOVEL(P)  CADSEQ        XXVAL
     C                   MOVE      CAF(7)        XXMSG#
     C                   MOVEL(P)  CAD(7)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* SOCIAL SECURITY NUMBER MUST BE VALID
     C*
     C                   MOVEL(P)  CASS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CASS#'       XXFIEL
     C                   MOVEL(P)  CASS#         XXVAL
     C                   MOVE      CAF(8)        XXMSG#
     C                   MOVEL(P)  CAD(8)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID ACCIDENT DATE
     C*
     C                   Z-ADD     CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAACDT'      XXFIEL
     C                   MOVEL(P)  CAACDT        XXVAL
     C                   MOVE      CAF(9)        XXMSG#
     C                   MOVEL(P)  CAD(9)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID R&C FLAG
     C*
     C     CARCFL        IFNE      'Y'
     C     CARCFL        ANDNE     'X'
     C     CARCFL        ANDNE     'S'
     C     CARCFL        ANDNE     'C'
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CARCFL'      XXFIEL
     C                   MOVEL(P)  CARCFL        XXVAL
     C                   MOVE      CAF(10)       XXMSG#
     C                   MOVEL(P)  CAD(10)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* GEO ZIP MUST BE VALID IF NOT BLANK
     C*
     C                   MOVE      *BLANKS       KKZIPA
     C     CAZIPA        IFNE      *BLANKS
     C                   MOVEL(P)  CAZIPA        KKFRZP
     C     GZKY1         SETLL     PGZ01
     C     GZKY2         READE     PGZ01                                  99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAZIPA'      XXFIEL
     C                   MOVEL(P)  CAZIPA        XXVAL
     C                   MOVE      CAF(11)       XXMSG#
     C                   MOVEL(P)  CAD(11)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ELSE
     C*
     C     KKFRZP        IFGT      GZTOZP
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAZIPA'      XXFIEL
     C                   MOVEL(P)  CAZIPA        XXVAL
     C                   MOVE      CAF(11)       XXMSG#
     C                   MOVEL(P)  CAD(11)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ELSE
     C                   MOVE      GZZIPA        KKZIPA
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C* ADJUDICATION STATE IS INVALID
     C*
     C                   MOVEL(P)  CAAST         KKKEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAAST'       XXFIEL
     C                   MOVEL(P)  CAAST         XXVAL
     C                   MOVE      CAF(12)       XXMSG#
     C                   MOVEL(P)  CAD(12)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* CHARGED AMOUNT CAN'T BE ZERO
     C*
     C     CACHRG        IFEQ      *ZEROS
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CACHRG'      XXFIEL
     C                   MOVEL(P)  CACHRG        XXVAL
     C                   MOVE      CAF(13)       XXMSG#
     C                   MOVEL(P)  CAD(13)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* LINE # CAN'T BE ZERO
     C*
     C     CALIN#        IFEQ      *ZEROS
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CALIN#'      XXFIEL
     C                   MOVEL(P)  CALIN#        XXVAL
     C                   MOVE      CAF(14)       XXMSG#
     C                   MOVEL(P)  CAD(14)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* OTHER REDUCTION 1 CODE IS INVALID
     C*
     C     CAOT1C        IFNE      *BLANKS
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  CAOT1C        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAOT1C'      XXFIEL
     C                   MOVEL(P)  CAOT1C        XXVAL
     C                   MOVE      CAF(15)       XXMSG#
     C                   MOVEL(P)  CAD(15)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* OTHER REDUCTION 2 CODE IS INVALID
     C*
     C     CAOT2C        IFNE      *BLANKS
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  CAOT2C        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CAOT2C'      XXFIEL
     C                   MOVEL(P)  CAOT2C        XXVAL
     C                   MOVE      CAF(16)       XXMSG#
     C                   MOVEL(P)  CAD(16)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* BILLED PROCEDURE CODE IS INVALID
     C*
     C     CABPRC        IFNE      *BLANKS
     C                   MOVEL(P)  CABPRC        KKCPT4
     C     PRKY2         CHAIN     PPR01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CABPRC        KKCPT4
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CABPRC'      XXFIEL
     C                   MOVEL(P)  CABPRC        XXVAL
     C                   MOVE      CAF(17)       XXMSG#
     C                   MOVEL(P)  CAD(17)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF
     C*
     C* CODE FOR THE LINE ITEM MUST BE VALID
     C*
     C     CACODE        IFNE      *BLANKS
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C     EOBTYP        OREQ      'N'
     C                   MOVEL(P)  CACODE        KKCPT4
     C                   Z-ADD     CAFDAT        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CACODE        KKCPT4
     C                   Z-ADD     CAFDAT        KKEFDT
     C     RCKY1         SETGT     PRC02
     C     RCKY2         READPE    PRC02                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CACODE'      XXFIEL
     C                   MOVEL(P)  CACODE        XXVAL
     C                   MOVE      CAF(18)       XXMSG#
     C                   MOVEL(P)  CAD(18)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     EOBTYP        WHENEQ    'P'
     C                   MOVEL(P)  CACODE        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CACODE'      XXFIEL
     C                   MOVEL(P)  CACODE        XXVAL
     C                   MOVE      CAF(18)       XXMSG#
     C                   MOVEL(P)  CAD(18)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'U'
     C                   MOVEL(P)  CACODE        KKCMSV
     C                   Z-ADD     CAFDAT        KKEFDT
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDCAP'       XXFILE
     C                   MOVEL(P)  'CACODE'      XXFIEL
     C                   MOVEL(P)  CACODE        XXVAL
     C                   MOVE      CAF(18)       XXMSG#
     C                   MOVEL(P)  CAD(18)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FDDAP
     C***************************************************************
     C     WRNDAP        BEGSR
     C*
     C* Amounts should crossfoot
     C*
     C     DACHRG        SUB       DAPAID        ZZCHRG
     C                   SUB       DAOTHR        ZZCHRG
     C                   SUB       DAOTH2        ZZCHRG
     C                   SUB       DANDIS        ZZCHRG
     C                   SUB       DAMRA         ZZCHRG
     C*
     C     ZZCHRG        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER2(2)        XXMSG#
     C                   MOVEL(P)  ED2(2)        XXDESC
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* SOCIAL SECURITY NUMBER MUST BE VALID
     C*
     C                   MOVEL(P)  DASS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DASS#'       XXFIEL
     C                   MOVEL(P)  DASS#         XXVAL
     C                   MOVE      DAF(1)        XXMSG#
     C                   MOVEL(P)  DAD(1)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID ACCIDENT DATE
     C*
     C                   Z-ADD     DAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAACDT'      XXFIEL
     C                   MOVEL(P)  DAACDT        XXVAL
     C                   MOVE      DAF(2)        XXMSG#
     C                   MOVEL(P)  DAD(2)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DETAIL SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     DADSEQ        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DADSEQ'      XXFIEL
     C                   MOVEL(P)  DADSEQ        XXVAL
     C                   MOVE      DAF(3)        XXMSG#
     C                   MOVEL(P)  DAD(3)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID TRANSACTION DATE
     C*
     C                   Z-ADD     DATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DATRDT'      XXFIEL
     C                   MOVEL(P)  DATRDT        XXVAL
     C                   MOVE      DAF(4)        XXMSG#
     C                   MOVEL(P)  DAD(4)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* OTHER REDUCTION 1 CODE IS INVALID
     C*
     C     DAOTY1        IFNE      *BLANKS
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  DAOTY1        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAOTY1'      XXFIEL
     C                   MOVEL(P)  DAOTY1        XXVAL
     C                   MOVE      DAF(5)        XXMSG#
     C                   MOVEL(P)  DAD(5)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* OTHER REDUCTION 2 CODE IS INVALID
     C*
     C     DAOTY2        IFNE      *BLANKS
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  DAOTY2        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDAP'       XXFILE
     C                   MOVEL(P)  'DAOTY2'      XXFIEL
     C                   MOVEL(P)  DAOTY2        XXVAL
     C                   MOVE      DAF(6)        XXMSG#
     C                   MOVEL(P)  DAD(6)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FDDDP
     C***************************************************************
     C     WRNDDP        BEGSR
     C*
     C* SOCIAL SECURITY NUMBER MUST BE VALID
     C*
     C                   MOVEL(P)  DDSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDSS#'       XXFIEL
     C                   MOVEL(P)  DDSS#         XXVAL
     C                   MOVE      DDF(1)        XXMSG#
     C                   MOVEL(P)  DDD(1)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID ACCIDENT DATE
     C*
     C                   Z-ADD     DDACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDACDT'      XXFIEL
     C                   MOVEL(P)  DDACDT        XXVAL
     C                   MOVE      DDF(2)        XXMSG#
     C                   MOVEL(P)  DDD(2)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DETAIL SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     DDDSEQ        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDDSEQ'      XXFIEL
     C                   MOVEL(P)  DDDSEQ        XXVAL
     C                   MOVE      DDF(3)        XXMSG#
     C                   MOVEL(P)  DDD(3)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* PROCEDURE CODE MUST BE VALID
     C*
     C                   MOVEL(P)  DDPRCD        KKCPT4
     C                   Z-ADD     CAFDAT        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  DDPRCD        KKCPT4
     C                   Z-ADD     CAFDAT        KKEFDT
     C     RCKY1         SETGT     PRC02
     C     RCKY2         READPE    PRC02                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDPRCD'      XXFIEL
     C                   MOVEL(P)  DDPRCD        XXVAL
     C                   MOVE      DDF(4)        XXMSG#
     C                   MOVEL(P)  DDD(4)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* PROCEDURE DESCRIPTION IS BLANK
     C*
     C     DDPRDS        IFEQ      *BLANKS
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDPRDS'      XXFIEL
     C                   MOVEL(P)  DDPRDS        XXVAL
     C                   MOVE      DDF(5)        XXMSG#
     C                   MOVEL(P)  DDD(5)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* CODE ORIGINATION FLAG IS INVALID
     C*
     C     DDRCFL        IFNE      'Y'
     C     DDRCFL        ANDNE     'C'
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDRCFL'      XXFIEL
     C                   MOVEL(P)  DDRCFL        XXVAL
     C                   MOVE      DDF(6)        XXMSG#
     C                   MOVEL(P)  DDD(6)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BILLED PROCEDURE CODE IS INVALID
     C*
     C     DDBPRC        IFNE      *BLANKS
     C                   MOVEL(P)  DDBPRC        KKCPT4
     C     PRKY2         CHAIN     PPR01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  DDBPRC        KKCPT4
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDBPRC'      XXFIEL
     C                   MOVEL(P)  DDBPRC        XXVAL
     C                   MOVE      DDF(7)        XXMSG#
     C                   MOVEL(P)  DDD(7)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF
     C*
     C* BILLED PROCEDURE CODE DESCRIPTION IS BLANK
     C*
     C*          DDBPRD    IFEQ *BLANKS
     C*                    MOVEL'FDDDP'   XXFILE    P
     C*                    MOVEL'DDBPRD'  XXFIEL    P
     C*                    MOVELDDBPRD    XXVAL     P
     C*                    MOVE DDF,8     XXMSG#
     C*                    MOVELDDD,8     XXDESC    P
     C*                    Z-ADDCARCN     XXRCN
     C*                    Z-ADDCADSEQ    XXDSEQ
     C*                    Z-ADDCALIN#    XXLIN#
     C*                    EXSR WRTERR
     C*                    ENDIF
     C*
     C* PLACE OF SERVICE IS INVALID
     C*
     C                   MOVE      DDPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDPOS'       XXFIEL
     C                   MOVEL(P)  DDPOS         XXVAL
     C                   MOVE      DDF(9)        XXMSG#
     C                   MOVEL(P)  DDD(9)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* UNITS CAN'T BE ZERO
     C*
     C     DDUNTS        IFEQ      *ZEROS
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDUNTS'      XXFIEL
     C                   MOVEL(P)  DDUNTS        XXVAL
     C                   MOVE      DDF(10)       XXMSG#
     C                   MOVEL(P)  DDD(10)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BODY PART IS INVALID
     C*
     C     DDPART        IFNE      *BLANKS
     C                   MOVEL(P)  'TOTH'        KKTBID
     C                   MOVEL(P)  DDPART        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDDP'       XXFILE
     C                   MOVEL(P)  'DDPART'      XXFIEL
     C                   MOVEL(P)  DDPART        XXVAL
     C                   MOVE      DDF(11)       XXMSG#
     C                   MOVEL(P)  DDD(11)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FDDEP
     C***************************************************************
     C     WRNDEP        BEGSR
     C*
     C                   SETOFF                                       90
     C*
     C* SOCIAL SECURITY NUMBER MUST BE VALID
     C*
     C                   MOVEL(P)  DESS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDEP'       XXFILE
     C                   MOVEL(P)  'DESS#'       XXFIEL
     C                   MOVEL(P)  DESS#         XXVAL
     C                   MOVE      DEF(1)        XXMSG#
     C                   MOVEL(P)  DED(1)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID ACCIDENT DATE
     C*
     C                   Z-ADD     DEACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDEP'       XXFILE
     C                   MOVEL(P)  'DEACDT'      XXFIEL
     C                   MOVEL(P)  DEACDT        XXVAL
     C                   MOVE      DEF(2)        XXMSG#
     C                   MOVEL(P)  DED(2)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DETAIL SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     DEDSEQ        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  'FDDEP'       XXFILE
     C                   MOVEL(P)  'DEDSEQ'      XXFIEL
     C                   MOVEL(P)  DEDSEQ        XXVAL
     C                   MOVE      DEF(3)        XXMSG#
     C                   MOVEL(P)  DED(3)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C*          DESEQ#    IFEQ *ZEROS
     C*
     C*                    MOVEL'FDDEP'   XXFILE    P
     C*                    MOVEL'DESEQ#'  XXFIEL    P
     C*                    MOVELDESEQ#    XXVAL     P
     C*                    MOVE DEF,4     XXMSG#
     C*                    MOVELDED,4     XXDESC    P
     C*                    Z-ADDCARCN     XXRCN
     C*                    Z-ADDCADSEQ    XXDSEQ
     C*                    Z-ADDCALIN#    XXLIN#
     C*                    EXSR WRTERR
     C*
     C*                    ENDIF
     C*
     C* EOB NOTE AND EOB CODE ARE BLANK
     C*
     C     DEEOB         IFEQ      *BLANKS
     C     DENOT1        ANDEQ     *BLANKS
     C                   MOVEL(P)  'FDDEP'       XXFILE
     C                   MOVEL(P)  'DEEOB'       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      DEF(5)        XXMSG#
     C                   MOVEL(P)  DED(5)        XXDESC
     C                   EXSR      WRTERR
     C                   SETON                                        90
     C                   ENDIF
     C*
     C* EOB CODE SOURCE IS INVALID
     C*
     C     DEESRC        IFNE      'A'
     C     DEESRC        ANDNE     'U'
     C     DEESRC        ANDNE     'N'
     C*
     C                   MOVEL(P)  'FDDEP'       XXFILE
     C                   MOVEL(P)  'DEESRC'      XXFIEL
     C                   MOVEL(P)  DEESRC        XXVAL
     C                   MOVE      DEF(6)        XXMSG#
     C                   MOVEL(P)  DED(6)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* PRINT FLAG IS INVALID
     C*
     C     DEPRNT        IFNE      'Y'
     C     DEPRNT        ANDNE     'N'
     C     DEPRNT        ANDNE     ' '
     C*
     C                   MOVEL(P)  'FDDEP'       XXFILE
     C                   MOVEL(P)  'DEPRNT'      XXFIEL
     C                   MOVEL(P)  DEPRNT        XXVAL
     C                   MOVE      DEF(7)        XXMSG#
     C                   MOVEL(P)  DED(7)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FDDHP
     C***************************************************************
     C     WRNDHP        BEGSR
     C*
     C* SOCIAL SECURITY NUMBER MUST BE VALID
     C*
     C                   MOVEL(P)  DHSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHSS#'       XXFIEL
     C                   MOVEL(P)  DHSS#         XXVAL
     C                   MOVE      DHF(1)        XXMSG#
     C                   MOVEL(P)  DHD(1)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID ACCIDENT DATE
     C*
     C                   Z-ADD     DHACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHACDT'      XXFIEL
     C                   MOVEL(P)  DHACDT        XXVAL
     C                   MOVE      DHF(2)        XXMSG#
     C                   MOVEL(P)  DHD(2)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DETAIL SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     DHDSEQ        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHDSEQ'      XXFIEL
     C                   MOVEL(P)  DHDSEQ        XXVAL
     C                   MOVE      DHF(3)        XXMSG#
     C                   MOVEL(P)  DHD(3)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* PLACE OF SERVICE IS INVALID
     C*
     C                   MOVE      DHPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHPOS'       XXFIEL
     C                   MOVEL(P)  DHPOS         XXVAL
     C                   MOVE      DHF(4)        XXMSG#
     C                   MOVEL(P)  DHD(4)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* TYPE OF SERVICE IS INVALID
     C*
     C     DHTOS         IFNE      *BLANKS
     C     EOAST         CAT(P)    DHTOS:0       KKKEY
     C     TBKY          CHAIN     PTS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHTOS'       XXFIEL
     C                   MOVEL(P)  DHTOS         XXVAL
     C                   MOVE      DHF(5)        XXMSG#
     C                   MOVEL(P)  DHD(5)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* PROCEDURE CODE MUST BE VALID
     C*
     C                   MOVEL(P)  DHPRCD        KKCPT4
     C                   Z-ADD     CAFDAT        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  DHPRCD        KKCPT4
     C                   Z-ADD     CAFDAT        KKEFDT
     C     RCKY1         SETGT     PRC02
     C     RCKY2         READPE    PRC02                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHPRCD'      XXFIEL
     C                   MOVEL(P)  DHPRCD        XXVAL
     C                   MOVE      DHF(6)        XXMSG#
     C                   MOVEL(P)  DHD(6)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* PROCEDURE CODE DESCRIPTION IS BLANK
     C*
     C     DHPRDS        IFEQ      *BLANKS
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHPRDS'      XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      DHF(7)        XXMSG#
     C                   MOVEL(P)  DHD(7)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PROCEDURE CODE MODIFIER 1 IS INVALID
     C*
     C     DHMOD1        IFNE      *BLANKS
     C                   MOVEL(P)  DHMOD1        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHMOD1'      XXFIEL
     C                   MOVEL(P)  DHMOD1        XXVAL
     C                   MOVE      DHF(8)        XXMSG#
     C                   MOVEL(P)  DHD(8)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* PROCEDURE CODE MODIFIER 2 IS INVALID
     C*
     C     DHMOD2        IFNE      *BLANKS
     C                   MOVEL(P)  DHMOD2        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHMOD2'      XXFIEL
     C                   MOVEL(P)  DHMOD2        XXVAL
     C                   MOVE      DHF(9)        XXMSG#
     C                   MOVEL(P)  DHD(9)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* PROCEDURE CODE MODIFIER 3 IS INVALID
     C*
     C     DHMOD3        IFNE      *BLANKS
     C                   MOVEL(P)  DHMOD3        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHMOD3'      XXFIEL
     C                   MOVEL(P)  DHMOD3        XXVAL
     C                   MOVE      DHF(10)       XXMSG#
     C                   MOVEL(P)  DHD(10)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* PROCEDURE CODE MODIFIER 4 IS INVALID
     C*
     C     DHMOD4        IFNE      *BLANKS
     C                   MOVEL(P)  DHMOD4        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHMOD4'      XXFIEL
     C                   MOVEL(P)  DHMOD4        XXVAL
     C                   MOVE      DHF(11)       XXMSG#
     C                   MOVEL(P)  DHD(11)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* AT LEAST ONE DIAGNOSIS CODE X-REF MUST NOT BE BLANK
     C*
     C     DHDXR1        IFEQ      *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHDXR1-4'    XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      DHF(12)       XXMSG#
     C                   MOVEL(P)  DHD(12)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DIAGNOSIS X-REF 1 MUST BE VALID
     C*
     C     DHDXR1        IFNE      'X'
     C     DHDXR1        ANDNE     ' '
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHDXR1'      XXFIEL
     C                   MOVEL(P)  DHDXR1        XXVAL
     C                   MOVE      DHF(13)       XXMSG#
     C                   MOVEL(P)  DHD(13)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DIAGNOSIS X-REF 2 MUST BE VALID
     C*
     C     DHDXR2        IFNE      'X'
     C     DHDXR2        ANDNE     ' '
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHDXR2'      XXFIEL
     C                   MOVEL(P)  DHDXR2        XXVAL
     C                   MOVE      DHF(14)       XXMSG#
     C                   MOVEL(P)  DHD(14)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DIAGNOSIS X-REF 3 MUST BE VALID
     C*
     C     DHDXR3        IFNE      'X'
     C     DHDXR3        ANDNE     ' '
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHDXR3'      XXFIEL
     C                   MOVEL(P)  DHDXR3        XXVAL
     C                   MOVE      DHF(15)       XXMSG#
     C                   MOVEL(P)  DHD(15)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DIAGNOSIS X-REF 4 MUST BE VALID
     C*
     C     DHDXR4        IFNE      'X'
     C     DHDXR4        ANDNE     ' '
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHDXR4'      XXFIEL
     C                   MOVEL(P)  DHDXR4        XXVAL
     C                   MOVE      DHF(16)       XXMSG#
     C                   MOVEL(P)  DHD(16)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* UNITS CAN'T BE ZERO
     C*
     C*          DHUNTS    IFEQ *ZEROS
     C*                    MOVEL'FDDHP'   XXFILE    P
     C*                    MOVEL'DHUNTS'  XXFIEL    P
     C*                    MOVELDHUNTS    XXVAL     P
     C*                    MOVE DHF,17    XXMSG#
     C*                    MOVELDHD,17    XXDESC    P
     C*                    Z-ADDCARCN     XXRCN
     C*                    Z-ADDCADSEQ    XXDSEQ
     C*                    Z-ADDCALIN#    XXLIN#
     C*                    EXSR WRTERR
     C*                    ENDIF
     C*
     C* BODY PART IS INVALID
     C*
     C     DHAREA        IFNE      *BLANKS
     C                   MOVEL(P)  DHAREA        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHAREA'      XXFIEL
     C                   MOVEL(P)  DHAREA        XXVAL
     C                   MOVE      DHF(18)       XXMSG#
     C                   MOVEL(P)  DHD(18)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* SOURCE OF CODE MUST BE VALID
     C*
     C     DHRCFL        IFNE      'C'
     C     DHRCFL        ANDNE     'Y'
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHRCFL'      XXFIEL
     C                   MOVEL(P)  DHRCFL        XXVAL
     C                   MOVE      DHF(19)       XXMSG#
     C                   MOVEL(P)  DHD(19)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BILLED PROCEDURE CODE IS INVALID
     C*
     C     DHBPRC        IFNE      *BLANKS
     C                   MOVEL(P)  DHBPRC        KKCPT4
     C     PRKY2         CHAIN     PPR01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  DHBPRC        KKCPT4
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDHP'       XXFILE
     C                   MOVEL(P)  'DHBPRC'      XXFIEL
     C                   MOVEL(P)  DHBPRC        XXVAL
     C                   MOVE      DHF(20)       XXMSG#
     C                   MOVEL(P)  DHD(20)       XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* BILLED PROCEDURE CODE DESCRIPTION IS BLANK
     C*
     C*          DHBPRD    IFEQ *BLANKS
     C*                    MOVEL'FDDHP'   XXFILE    P
     C*                    MOVEL'DHBPRD'  XXFIEL    P
     C*                    MOVELDHBPRD    XXVAL     P
     C*                    MOVE DHF,21    XXMSG#
     C*                    MOVELDHD,21    XXDESC    P
     C*                    Z-ADDCARCN     XXRCN
     C*                    Z-ADDCADSEQ    XXDSEQ
     C*                    Z-ADDCALIN#    XXLIN#
     C*                    EXSR WRTERR
     C*                    ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FDDPP
     C***************************************************************
     C     WRNDPP        BEGSR
     C*
     C* SOCIAL SECURITY NUMBER MUST BE VALID
     C*
     C                   MOVEL(P)  DPSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  'DPSS#'       XXFIEL
     C                   MOVEL(P)  DPSS#         XXVAL
     C                   MOVE      DPF(1)        XXMSG#
     C                   MOVEL(P)  DPD(1)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID ACCIDENT DATE
     C*
     C                   Z-ADD     DPACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  'DPACDT'      XXFIEL
     C                   MOVEL(P)  DPACDT        XXVAL
     C                   MOVE      DPF(2)        XXMSG#
     C                   MOVEL(P)  DPD(2)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DETAIL SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     DPDSEQ        IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  'DPDSEQ'      XXFIEL
     C                   MOVEL(P)  DPDSEQ        XXVAL
     C                   MOVE      DPF(3)        XXMSG#
     C                   MOVEL(P)  DPD(3)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* NDC CODE IS INVALID
     C*
     C                   MOVEL(P)  DPNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  'DPNDC'       XXFIEL
     C                   MOVEL(P)  DPNDC         XXVAL
     C                   MOVE      DPF(4)        XXMSG#
     C                   MOVEL(P)  DPD(4)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* CODE DESCRIPTION IS BLANK
     C*
     C     DPDESC        IFEQ      *BLANKS
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  'DPDESC'      XXFIEL
     C                   MOVEL(P)  DPDESC        XXVAL
     C                   MOVE      DPF(5)        XXMSG#
     C                   MOVEL(P)  DPD(5)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID RX FILLED DATE
     C*
     C                   Z-ADD     DPRXFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  'DPRXFD'      XXFIEL
     C                   MOVEL(P)  DPRXFD        XXVAL
     C                   MOVE      DPF(6)        XXMSG#
     C                   MOVEL(P)  DPD(6)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BILLED NDC CODE IS BLANK
     C*
     C     DPBNDC        IFNE      *BLANKS
     C                   MOVEL(P)  DPBNDC        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDPP'       XXFILE
     C                   MOVEL(P)  'DPBNDC'      XXFIEL
     C                   MOVEL(P)  DPBNDC        XXVAL
     C                   MOVE      DPF(7)        XXMSG#
     C                   MOVEL(P)  DPD(7)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FDDUP
     C***************************************************************
     C     WRNDUP        BEGSR
     C*
     C* SOCIAL SECURITY NUMBER MUST BE VALID
     C*
     C                   MOVEL(P)  DUSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'FDDUP'       XXFILE
     C                   MOVEL(P)  'DUSS#'       XXFIEL
     C                   MOVEL(P)  DUSS#         XXVAL
     C                   MOVE      DUF(1)        XXMSG#
     C                   MOVEL(P)  DUD(1)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID ACCIDENT DATE
     C*
     C                   Z-ADD     DUACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FDDUP'       XXFILE
     C                   MOVEL(P)  'DUACDT'      XXFIEL
     C                   MOVEL(P)  DUACDT        XXVAL
     C                   MOVE      DUF(2)        XXMSG#
     C                   MOVEL(P)  DUD(2)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DETAIL SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     DUDSEQ        IFEQ      *ZEROS
     C                   MOVEL(P)  'FDDUP'       XXFILE
     C                   MOVEL(P)  'DUDSEQ'      XXFIEL
     C                   MOVEL(P)  DUDSEQ        XXVAL
     C                   MOVE      DUF(3)        XXMSG#
     C                   MOVEL(P)  DUD(3)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* SERVICE CODE IS INVALID
     C*
     C                   MOVE      DURVCD        XXRVCD
     C                   MOVEL(P)  DURATE        XXRATE
     C                   MOVEL(P)  XXCMSV        KKCMSV
     C                   Z-ADD     CAFDAT        KKEFDT
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *ON
     C                   ELSE
     C     SVSTAT        IFEQ      'I'
     C     DURVCD        IFNE      *ZEROS
     C     DURATE        ORNE      *BLANKS
     C                   MOVEL(P)  'FDDUP'       XXFILE
     C                   MOVEL(P)  'DURVCD'      XXFIEL
     C                   MOVEL(P)  XXCMSV        XXVAL
     C                   MOVE      DUF(4)        XXMSG#
     C                   MOVEL(P)  DUD(4)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* CODE DESCRIPTION IS BLANK
     C*
     C     DUDESC        IFEQ      *BLANKS
     C                   MOVEL(P)  'FDDUP'       XXFILE
     C                   MOVEL(P)  'DUDESC'      XXFIEL
     C                   MOVEL(P)  DUDESC        XXVAL
     C                   MOVE      DUF(5)        XXMSG#
     C                   MOVEL(P)  DUD(5)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PLACE OF SERVICE IS INVALID
     C*
     C                   MOVE      DUPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FDDUP'       XXFILE
     C                   MOVEL(P)  'DUPOS'       XXFIEL
     C                   MOVEL(P)  DUPOS         XXVAL
     C                   MOVE      DUF(6)        XXMSG#
     C                   MOVEL(P)  DUD(6)        XXDESC
     C                   Z-ADD     CARCN         XXRCN
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CALIN#        XXLIN#
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BILLED SERVICE CODE IS INVALID
     C*
     C*                    MOVE DUBPRC    XXRVCD
     C*                    MOVELDUBRAT    XXRATE    P
     C*                    MOVELXXCMSV    KKCMSV    P
     C*                    Z-ADDCAFDAT    KKEFDT
     C*          SVKY1     SETGTPSV01
     C*          SVKY2     REDPEPSV01                    99
     C*          *IN99     IFEQ *ON
     C*                    MOVEL'FDDUP'   XXFILE    P
     C*                    MOVEL'DUBPRC'  XXFIEL    P
     C*                    MOVELXXCMSV    XXVAL     P
     C*                    MOVE DUF,7     XXMSG#
     C*                    MOVELDUD,7     XXDESC    P
     C*                    Z-ADDCARCN     XXRCN
     C*                    Z-ADDCADSEQ    XXDSEQ
     C*                    Z-ADDCALIN#    XXLIN#
     C*                    EXSR WRTERR
     C*                    ENDIF                           *IN99 = *OFF
     C*
     C* BILLED CODE DESCRIPTION IS BLANK
     C*
     C*          DUBPRD    IFEQ *BLANKS
     C*                    MOVEL'FDDUP'   XXFILE    P
     C*                    MOVEL'DUBPRD'  XXFIEL    P
     C*                    MOVELDUBPRD    XXVAL     P
     C*                    MOVE DUF,8     XXMSG#
     C*                    MOVELDUD,8     XXDESC    P
     C*                    Z-ADDCARCN     XXRCN
     C*                    Z-ADDCADSEQ    XXDSEQ
     C*                    Z-ADDCALIN#    XXLIN#
     C*                    EXSR WRTERR
     C*                    ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WRITES TO ERROR FILE
     C***************************************************************
     C     WRTERR        BEGSR
     C*
     C                   CLEAR                   PER01
     C*
     C                   MOVEL(P)  XXFILE        ERFILE
     C                   MOVEL(P)  XXFIEL        ERFIEL
     C                   MOVEL(P)  XXVAL         ERVAL
     C                   Z-ADD     XXMSG#        ERMSG#
     C                   MOVEL(P)  XXDESC        ERDESC
     C                   Z-ADD     EOCO#         ERCO#
     C                   Z-ADD     EOSUB#        ERSUB#
     C                   Z-ADD     EOFND         ERFND
     C                   Z-ADD     EOFYR         ERFYR
     C                   Z-ADD     EOCASE        ERCASE
     C                   Z-ADD     EOEOB#        EREOB#
     C                   Z-ADD     XXRCN         ERRCN
     C                   Z-ADD     XXDSEQ        ERDSEQ
     C                   Z-ADD     XXLIN#        ERLIN#
     C                   Z-ADD     EOACDT        ERACDT
     C                   MOVEL(P)  EOSS#         ERSS#
     C*
     C                   WRITE     PER01
     C*
     C                   ENDSR
     C*****************************************************************
     C* CLSPRP - CLOSE FMPRL
     C*****************************************************************
     C     CLSPRP        BEGSR
     C*
     C                   CLOSE     FMPRL
     C*
     C                   ENDSR
     C*****************************************************************
     C* CLSPSP - CLOSE FTPSL
     C*****************************************************************
     C     CLSPSP        BEGSR
     C*
     C                   CLOSE     FTPSL
     C*
     C                   ENDSR
     C*****************************************************************
     C* CLSSVP - CLOSE FMSVL1
     C*****************************************************************
     C     CLSSVP        BEGSR
     C*
     C                   CLOSE     FMSVL1
     C*
     C                   ENDSR
     C*****************************************************************
     C* CLSTAB - CLOSE FTABL
     C*****************************************************************
     C     CLSTAB        BEGSR
     C*
     C                   CLOSE     FTABL
     C*
     C                   ENDSR
     C*****************************************************************
     C* DOVPRP - DELETES OVERRIDE OF FMPRL
     C*****************************************************************
     C     DOVPRP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDSR
     C*****************************************************************
     C* DOVPSP - DELETES OVERRIDE OF FTPSL
     C*****************************************************************
     C     DOVPSP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*****************************************************************
     C* DOVSVP - DELETES OVERRIDE OF FMSVL1
     C*****************************************************************
     C     DOVSVP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*****************************************************************
     C* DOVTAB - DELETES OVERRIDE OF FTABL
     C*****************************************************************
     C     DOVTAB        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*****************************************************************
     C* OPNPRP - OPEN FMPRL
     C*****************************************************************
     C     OPNPRP        BEGSR
     C*
     C                   OPEN      FMPRL
     C*
     C                   ENDSR
     C*****************************************************************
     C* OPNPSP - OPEN FTPSL
     C*****************************************************************
     C     OPNPSP        BEGSR
     C*
     C                   OPEN      FTPSL
     C*
     C                   ENDSR
     C*****************************************************************
     C* OPNSVP - OPEN FMSVL1
     C*****************************************************************
     C     OPNSVP        BEGSR
     C*
     C                   OPEN      FMSVL1
     C*
     C                   ENDSR
     C*****************************************************************
     C* OPNTAB - OPEN FTABL
     C*****************************************************************
     C     OPNTAB        BEGSR
     C*
     C                   OPEN      FTABL
     C*
     C                   ENDSR
     C*****************************************************************
     C* OVRPRP - OVERRIDES FMPRL TO STATE FILE
     C*****************************************************************
     C     OVRPRP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(6)        XXCMD
     C                   MOVEL     EOAST         XXST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*****************************************************************
     C* OVRPSP - OVERRIDES FTPSL TO STATE FILE
     C*****************************************************************
     C     OVRPSP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        XXCMD
     C                   MOVEL     EOAST         XXST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*****************************************************************
     C* OVRSVP - OVERRIDES FMSVL1 TO STATE FILE
     C*****************************************************************
     C     OVRSVP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(4)        YYCMD
     C                   MOVEL     EOAST         YYST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    YYCMD
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*****************************************************************
     C* OVRTAB - OVERRIDES FTABL TO STATE FILE
     C*****************************************************************
     C     OVRTAB        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(8)        XXCMD
     C                   MOVEL     EOAST         XXST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*****************************************************************
** ER1/ED1 - File Errors/Descriptions       |
11001There are no FDCAP records for this EOB#
11002There are no FDDAP records for this line
11003There are no FDDDP records for this line
11004There are no FDDHP records for this line
11005There are no FDDPP records for this line
11006There are no FDDUP records for this line
** ER2/ED2 - Amount Errors/Descriptions     |
12001FDCAP amounts do not cross-foot
12002FDDAP amounts do not cross-foot
12003Sum of FDDAP Charges do not equal FDCAP
12004Sum of FDDAP Payments do not equal FDCAP
12005Sum of FDDAP Other do not equal FDCAP
12006Sum of FDDAP Other 2 do not equal FDCAP
12007Sum of FDDAP Network do not equal FDCAP
12008Sum of FDDAP MRA do not equal FDCAP
12009Sum of FDCAP Charges do not equal FAEOP
12010Sum of FDCAP Payments do not equal FAEOP
12011Sum of FDCAP Other do not equal FAEOP
12012Sum of FDCAP Other 2 do not equal FAEOP
12013Sum of FDCAP Network do not equal FAEOP
12014Sum of FDCAP MRA do not equal FAEOP
12015Sum of FDDAP Charges do not equal FAEAP
12016Sum of FDDAP Payments do not equal FAEAP
12017Sum of FDDAP Other do not equal FAEAP
12018Sum of FDDAP Other 2 do not equal FAEAP
12019Sum of FDDAP Network do not equal FAEAP
12020Sum of FDDAP MRA do not equal FAEAP
** CAF/CAD - FDCAP Field Errors/Description |
13001Invalid Adjudication code.
13002Invalid FROM service date.
13003Invalid TO service date.
13004Invalid Date Recieved.
13005Invalid Provider FEIN/Mod.
13006Invalid Provider Type.
13007Detail Sequence# is zero.
13008Social Security # is invalid.
13009Accident date is invalid.
13010Invalid R&C flag.
13011Invalid Geo zip.
13012Invalid Adjudication State.
13013Charged amount is zero.
13014Line # is zero.
13015Other reductions code 1 is invalid.
13016Other reductions code 2 is invalid.
13017Billed procedure code is invalid.
13018Code for line item is invalid.
** DAF/DAD - FDDAP Field Errors/Description |
14001Social Security # is invalid.
14002Accident date is invalid.
14003Detail Sequence# is zero.
14004Transaction Date is invalid.
14005Other reductions code 1 is invalid.
14006Other reductions code 2 is invalid.
** DDF/DDD - FDDDP Field Errors/Description |
15001Social Security # is invalid.
15002Accident date is invalid.
15003Detail Sequence# is zero.
15004Procedure code is invalid.
15005Procedure code description is blank.
15006Code origination file flag is invalid.
15007Billed Procedure code is invalid.
15008Billed Procedure code desc. is blank.
15009Place of service is invalid.
15010Units is zero.
15011Body area is invalid.
** DEF/DED - FDDEP Field Errors/Description |
16001Social Security # is invalid.
16002Accident date is invalid.
16003Detail Sequence# is zero.
16004Sequence# is zero.
16005EOB code and EOB note are blank.
16006EOB code source is invalid.
16007Print flag is invalid.
** DHF/DHD - FDDHP Field Errors/Description |
17001Social Security # is invalid.
17002Accident date is invalid.
17003Detail Sequence# is zero.
17004Place of service is invalid.
17005Type of service is invalid.
17006Procedure code is invalid.
17007Procedure code description is blank.
17008Procedure code modifier 1 is invalid.
17009Procedure code modifier 2 is invalid.
17010Procedure code modifier 3 is invalid.
17011Procedure code modifier 4 is invalid.
17012All Diagnosis code xrefs are all blank.
17013Diagnosis code xref 1 is invalid.
17014Diagnosis code xref 2 is invalid.
17015Diagnosis code xref 3 is invalid.
17016Diagnosis code xref 4 is invalid.
17017Units is zero.
17018Body area is invalid.
17019Invalid Source of code
17020Billed procedure code is invalid
17021Billed procedure code desc is blank
** DPF/DPD - FDDPP Field Errors/Description |
18001Social Security # is invalid.
18002Accident date is invalid.
18003Detail Sequence# is zero.
18004NDC code is blank
18005Code description is blank
18006Invalid Rx Filled Date
18007Billed NDC code is blank
** DUF/DUD - FDDUP Field Errors/Description |
19001Social Security # is invalid.
19002Accident date is invalid.
19003Detail Sequence# is zero.
19004Service code is invalid.
19005Service code description is blank.
19006Place of service is invalid.
19007Billed Service code is invalid.
19008Billed Service code desc. is blank.
** EOF/EOD - FAEOP Field Errors/Description |
07032EOFRDT must match oldest CAFDAT
07033EOTODT must match most current CATDAT
**  QCMDEXC TABLE
DLTOVR FILE(FTPSL)
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
