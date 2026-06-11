     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE CHKHEADERS - CHECKS PBILLS HEADER FILES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM CHECKS PBILLS HEADER FILES        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    COMMAND LINE                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 08/02/01  PROGRAMMER: C:62  PROJECT: MC246          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  READ  - FAEOP                                           *
     H*****************************************************************
     FFAEOP     IF   E             DISK
     FFAEAL1    IF   E           K DISK
     FFAEDL     IF   E           K DISK
     FFAEEL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     FFAESL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FCVTERR    O    E             DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FSTSTL1    IF   E           K DISK
     FUMPCL9    IF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
     FNMNWL     IF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN
     FFTGTL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFMRCL1    IF   E           K DISK
     FFTBSL     IF   E           K DISK
     F*
     F*

     D ER1             S              5    DIM(6) CTDATA PERRCD(1)              File Errors
     D ED1             S             40    DIM(6) ALT(ER1)
     D ER2             S              5    DIM(8) CTDATA PERRCD(1)              Amount Errors
     D ED2             S             40    DIM(8) ALT(ER2)
     D EAF             S              5    DIM(23) CTDATA PERRCD(1)             FAEAP Field Errors
     D EAD             S             40    DIM(23) ALT(EAF)
     D EDF             S              5    DIM(3) CTDATA PERRCD(1)              FAEDP Field Errors
     D EDD             S             40    DIM(3) ALT(EDF)
     D EEF             S              5    DIM(5) CTDATA PERRCD(1)              FAEEP Field Errors
     D EED             S             40    DIM(5) ALT(EEF)
     D EHF             S              5    DIM(9) CTDATA PERRCD(1)              FAEHP Field Errors
     D EHD             S             40    DIM(9) ALT(EHF)
     D EOF             S              5    DIM(31) CTDATA PERRCD(1)             FAEOP Field Errors
     D EOD             S             40    DIM(31) ALT(EOF)
     D EPF             S              5    DIM(3) CTDATA PERRCD(1)              FAEPP Field Errors
     D EPD             S             40    DIM(3) ALT(EPF)
     D ESF             S              5    DIM(7) CTDATA PERRCD(1)              FAESP Field Errors
     D ESD             S             40    DIM(7) ALT(ESF)
     D EUF             S              5    DIM(34) CTDATA PERRCD(1)             FAEUP Field Errors
     D EUD             S             40    DIM(34) ALT(EUF)
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS                  INZ
     D  XXCMD                  1     56
     D  XXST                  38     39
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*****************************************************************
     D* Constants
     D*****************************************************************
     D DETAIL          C                   CONST('CHKDETAILS')

     C*****************************************************************
     C* Parameter lists
     C*****************************************************************
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
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C* Key lists.
     C*****************************************************************
     C     BSKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CMKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    KKRCN
     C*
     C     EOKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     GENKY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     GTKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     GTKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKTBID
     C*
     C     NWKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EONTWK
     C*
     C     PCKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOCERT
     C                   KFLD                    EOEXT
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
     C     RCKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKCPT4
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKKEY
     C*
     C*  LIKE DEFINES
     C*
     C     *LIKE         DEFINE    EOCHRG        ZZCHRG
     C     *LIKE         DEFINE    EOCHRG        XXCHRG
     C     *LIKE         DEFINE    EOPAID        XXPAID
     C     *LIKE         DEFINE    EOOTHR        XXOTHR
     C     *LIKE         DEFINE    EOOTH2        XXOTH2
     C     *LIKE         DEFINE    EONDIS        XXNDIS
     C     *LIKE         DEFINE    EOMRA         XXMRA
     C     *LIKE         DEFINE    EOSS#         XXSS#
     C     *LIKE         DEFINE    EOACDT        XXACDT
     C     *LIKE         DEFINE    ERFILE        XXFILE
     C     *LIKE         DEFINE    ERFIEL        XXFIEL
     C     *LIKE         DEFINE    ERVAL         XXVAL
     C     *LIKE         DEFINE    ERMSG#        XXMSG#
     C     *LIKE         DEFINE    ERDESC        XXDESC
     C     *LIKE         DEFINE    EORCN         XXRCN
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    CSKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    CDSS#         KKSS#
     C     *LIKE         DEFINE    EOAST         HDAST
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     '*YES'        OPENF             4
     C                   MOVEL     'ZZ'          HDAST
     C*
     C* READ THE INCOMING PC FILE
     C*
     C                   READ      PEO01                                  99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EOBLST        IFNE      'X'
     C     EOBLST        ANDNE     'D'
     C*
     C                   EXSR      CHKEOP
     C*
     C* INITIALIZE AMOUNT FIELDS
     C*
     C                   Z-ADD     *ZEROS        XXCHRG
     C                   Z-ADD     *ZEROS        XXPAID
     C                   Z-ADD     *ZEROS        XXOTHR
     C                   Z-ADD     *ZEROS        XXOTH2
     C                   Z-ADD     *ZEROS        XXNDIS
     C                   Z-ADD     *ZEROS        XXMRA
     C*
     C     0             DO        EORCN         X                 3 0
     C                   EXSR      CHKEAP
     C                   ENDDO
     C     EOBLST        IFEQ      'C'
     C                   EXSR      AMTCHK
     C                   ENDIF
     C*
     C                   EXSR      CHKEEP
     C                   EXSR      CHKESP
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      CHKEDP
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      CHKEUP
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      CHKEHP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      CHKEPP
     C                   ENDSL
     C*
     C                   CALL      DETAIL
     C                   PARM      EOCO#         ##CO#             3 0
     C                   PARM      EOSUB#        ##SUB#            5 0
     C                   PARM      EOFND         ##FND             3 0
     C                   PARM      EOFYR         ##FYR             3 0
     C                   PARM      EOCASE        ##CASE            7 0
     C                   PARM      EOEOB#        ##EOB#            9 0
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READ      PEO01                                  99
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C     OPENF         IFEQ      '*NO'
     C                   EXSR      CLSPRP
     C                   EXSR      DOVPRP
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* SUBROUTINE TO CHECK SUM OF FAEAP AMOUNTS EQUALS FAEOP AMOUNTS
     C*****************************************************************
     C     AMTCHK        BEGSR
     C*
     C     XXCHRG        IFNE      EOCHRG
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EACHRG'      XXFIEL
     C                   MOVEL     XXCHRG        XXONE            11
     C                   MOVEL     EOCHRG        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT       XXTWO:1       XXVAL
     C                   MOVE      ER2(3)        XXMSG#
     C                   MOVEL(P)  ED2(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C     XXPAID        IFNE      EOPAID
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAPAID'      XXFIEL
     C                   MOVEL     XXPAID        XXONE            11
     C                   MOVEL     EOPAID        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT       XXTWO:1       XXVAL
     C                   MOVE      ER2(4)        XXMSG#
     C                   MOVEL(P)  ED2(4)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C     XXOTHR        IFNE      EOOTHR
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAOTHR'      XXFIEL
     C                   MOVEL     XXOTHR        XXONE            11
     C                   MOVEL     EOOTHR        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT       XXTWO:1       XXVAL
     C                   MOVE      ER2(5)        XXMSG#
     C                   MOVEL(P)  ED2(5)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C     XXOTH2        IFNE      EOOTH2
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAOTH2'      XXFIEL
     C                   MOVEL     XXOTH2        XXONE            11
     C                   MOVEL     EOOTH2        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT       XXTWO:1       XXVAL
     C                   MOVE      ER2(6)        XXMSG#
     C                   MOVEL(P)  ED2(6)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C     XXNDIS        IFNE      EONDIS
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EANDIS'      XXFIEL
     C                   MOVEL     XXNDIS        XXONE            11
     C                   MOVEL     EONDIS        XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT       XXTWO:1       XXVAL
     C                   MOVE      ER2(7)        XXMSG#
     C                   MOVEL(P)  ED2(7)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C     XXMRA         IFNE      EOMRA
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAMRA '      XXFIEL
     C                   MOVEL     XXMRA         XXONE            11
     C                   MOVEL     EOMRA         XXTWO            11
     C     XXONE         CAT(P)    'VS':1        XXVAL
     C                   CAT       XXTWO:1       XXVAL
     C                   MOVE      ER2(8)        XXMSG#
     C                   MOVEL(P)  ED2(8)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAEAP RECORD
     C***************************************************************
     C     CHKEAP        BEGSR
     C*
     C                   Z-ADD     X             KKRCN
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *ON
     C                   ADD       0             XXCHRG
     C                   ADD       0             XXMRA
     C                   ADD       0             XXOTHR
     C                   ADD       0             XXOTH2
     C                   ADD       0             XXNDIS
     C                   ADD       0             XXPAID
     C     KKRCN         IFLT      EORCN
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(1)        XXMSG#
     C                   MOVEL(P)  ED1(1)        XXDESC
     C                   Z-ADD     KKRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ELSE
     C     EOBLST        IFEQ      'C'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(1)        XXMSG#
     C                   MOVEL(P)  ED1(1)        XXDESC
     C                   Z-ADD     KKRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EXSR      WRNEAP
     C                   ADD       EACHRG        XXCHRG
     C                   ADD       EAMRA         XXMRA
     C                   ADD       EAOTHR        XXOTHR
     C                   ADD       EAOTH2        XXOTH2
     C                   ADD       EANDIS        XXNDIS
     C                   ADD       EAPAID        XXPAID
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAEDP RECORD
     C***************************************************************
     C     CHKEDP        BEGSR
     C*
     C     GENKY         CHAIN     PED01                              99
     C     *IN99         IFEQ      *ON
     C     EORCN         IFGT      *ZEROS
     C                   MOVEL(P)  'FAEDP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(2)        XXMSG#
     C                   MOVEL(P)  ED1(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ELSE
     C     EOBLST        IFEQ      'C'
     C                   MOVEL(P)  'FAEDP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(2)        XXMSG#
     C                   MOVEL(P)  ED1(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EXSR      WRNEDP
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAEEP RECORD
     C***************************************************************
     C     CHKEEP        BEGSR
     C*
     C     GENKY         CHAIN     PEE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      WRNEEP
     C     GENKY         READE     PEE01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAEHP RECORD
     C***************************************************************
     C     CHKEHP        BEGSR
     C*
     C     GENKY         CHAIN     PEH01                              99
     C     *IN99         IFEQ      *ON
     C     EORCN         IFGT      *ZEROS
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(3)        XXMSG#
     C                   MOVEL(P)  ED1(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ELSE
     C     EOBLST        IFEQ      'C'
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(3)        XXMSG#
     C                   MOVEL(P)  ED1(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EXSR      WRNEHP
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAEOP RECORD
     C***************************************************************
     C     CHKEOP        BEGSR
     C*
     C                   EXSR      WRNEOP
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAEPP RECORD
     C***************************************************************
     C     CHKEPP        BEGSR
     C*
     C     GENKY         CHAIN     PEP01                              99
     C     *IN99         IFEQ      *ON
     C     EORCN         IFGT      *ZEROS
     C                   MOVEL(P)  'FAEPP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(4)        XXMSG#
     C                   MOVEL(P)  ED1(4)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ELSE
     C     EOBLST        IFEQ      'C'
     C                   MOVEL(P)  'FAEPP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(4)        XXMSG#
     C                   MOVEL(P)  ED1(4)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EXSR      WRNEPP
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAESP RECORD
     C***************************************************************
     C     CHKESP        BEGSR
     C*
     C     GENKY         CHAIN     PES01                              99
     C     *IN99         IFEQ      *ON
     C     EOBLST        IFEQ      'C'
     C     EORCN         ORGT      *ZEROS
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(5)        XXMSG#
     C                   MOVEL(P)  ED1(5)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ELSE
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      WRNESP
     C     GENKY         READE     PES01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO CHECK FAEUP RECORD
     C***************************************************************
     C     CHKEUP        BEGSR
     C*
     C     GENKY         CHAIN     PEU01                              99
     C     *IN99         IFEQ      *ON
     C     EORCN         IFGT      *ZEROS
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(6)        XXMSG#
     C                   MOVEL(P)  ED1(6)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ELSE
     C     EOBLST        IFEQ      'C'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER1(6)        XXMSG#
     C                   MOVEL(P)  ED1(6)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EXSR      WRNEUP
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAEAP
     C***************************************************************
     C     WRNEAP        BEGSR
     C*
     C* Amounts should crossfoot
     C*
     C     EACHRG        SUB       EAPAID        ZZCHRG
     C                   SUB       EAOTHR        ZZCHRG
     C                   SUB       EAOTH2        ZZCHRG
     C                   SUB       EANDIS        ZZCHRG
     C                   SUB       EAMRA         ZZCHRG
     C*
     C     ZZCHRG        IFNE      *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C*
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER2(2)        XXMSG#
     C                   MOVEL(P)  ED2(2)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C*
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* SOCIAL SECURITY # MUST BE VALID
     C*
     C                   MOVEL(P)  EASS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EASS#'       XXFIEL
     C                   MOVEL(P)  EASS#         XXVAL
     C                   MOVE      EAF(1)        XXMSG#
     C                   MOVEL(P)  EAD(1)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     EAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAACDT'      XXFIEL
     C                   MOVEL(P)  EAACDT        XXVAL
     C                   MOVE      EAF(2)        XXMSG#
     C                   MOVEL(P)  EAD(2)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID TRANSACTION DATE
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EATRDT'      XXFIEL
     C                   MOVEL(P)  EATRDT        XXVAL
     C                   MOVE      EAF(3)        XXMSG#
     C                   MOVEL(P)  EAD(3)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* MAINTENANCE USER IS BLANK
     C*
     C     EAUSER        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAUSER'      XXFIEL
     C                   MOVEL(P)  EAUSER        XXVAL
     C                   MOVE      EAF(4)        XXMSG#
     C                   MOVEL(P)  EAD(4)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INITIAL USER IS BLANK
     C*
     C     EAIUSR        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAIUSR'      XXFIEL
     C                   MOVEL(P)  EAIUSR        XXVAL
     C                   MOVE      EAF(5)        XXMSG#
     C                   MOVEL(P)  EAD(5)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID WC PROVIDER CHECK DATE
     C*
     C     EAWCPD        IFNE      *ZEROS
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAWCPD'      XXFIEL
     C                   MOVEL(P)  EAWCPD        XXVAL
     C                   MOVE      EAF(6)        XXMSG#
     C                   MOVEL(P)  EAD(6)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID WC BRS CHECK DATE
     C*
     C     EAWCBD        IFNE      *ZEROS
     C                   Z-ADD     EAWCBD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAWCBD'      XXFIEL
     C                   MOVEL(P)  EAWCBD        XXVAL
     C                   MOVE      EAF(7)        XXMSG#
     C                   MOVEL(P)  EAD(7)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID WC NWS CHECK DATE
     C*
     C     EAWCND        IFNE      *ZEROS
     C                   Z-ADD     EAWCND        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAWCND'      XXFIEL
     C                   MOVEL(P)  EAWCND        XXVAL
     C                   MOVE      EAF(8)        XXMSG#
     C                   MOVEL(P)  EAD(8)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID BRS INVOICE H/R FLAG
     C*
     C     EAEPFL        IFEQ      'P'
     C     EABHRF        IFNE      'H'
     C     EABHRF        ANDNE     'R'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EABHRF'      XXFIEL
     C                   MOVEL(P)  EABHRF        XXVAL
     C                   MOVE      EAF(9)        XXMSG#
     C                   MOVEL(P)  EAD(9)        XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID BRS INVOICE PROCESS FLAG.
     C*
     C     EABBLF        IFNE      'P'
     C     EABBLF        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EABBLF'      XXFIEL
     C                   MOVEL(P)  EABBLF        XXVAL
     C                   MOVE      EAF(10)       XXMSG#
     C                   MOVEL(P)  EAD(10)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID NWS INVOICE H/R FLAG
     C*
     C     EANHRF        IFNE      'H'
     C     EANHRF        ANDNE     'R'
     C     EANHRF        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EANHRF'      XXFIEL
     C                   MOVEL(P)  EANHRF        XXVAL
     C                   MOVE      EAF(11)       XXMSG#
     C                   MOVEL(P)  EAD(11)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID NWS INVOICE PROCESS FLAG.
     C*
     C     EANBLF        IFNE      'P'
     C     EANBLF        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EANBLF'      XXFIEL
     C                   MOVEL(P)  EANBLF        XXVAL
     C                   MOVE      EAF(12)       XXMSG#
     C                   MOVEL(P)  EAD(12)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID EDI SENT DATE
     C*
     C     EAESDT        IFNE      *ZEROS
     C                   Z-ADD     EAESDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAESDT'      XXFIEL
     C                   MOVEL(P)  EAESDT        XXVAL
     C                   MOVE      EAF(13)       XXMSG#
     C                   MOVEL(P)  EAD(13)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID EDI FLAG
     C*
     C     EAEDI         IFNE      'Y'
     C     EAEDI         ANDNE     'N'
     C     EAEDI         ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAEDI'       XXFIEL
     C                   MOVEL(P)  EAEDI         XXVAL
     C                   MOVE      EAF(14)       XXMSG#
     C                   MOVEL(P)  EAD(14)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID EOMB PRINT FLAG
     C*
     C     EAEPFL        IFNE      'E'
     C     EAEPFL        ANDNE     'D'
     C     EAEPFL        ANDNE     'X'
     C     EAEPFL        ANDNE     'P'
     C     EAEPFL        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAEPFL'      XXFIEL
     C                   MOVEL(P)  EAEPFL        XXVAL
     C                   MOVE      EAF(15)       XXMSG#
     C                   MOVEL(P)  EAD(15)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID BILLING PROCESS FLAG.
     C*
     C     EABPFL        IFNE      'Y'
     C     EABPFL        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EABPFL'      XXFIEL
     C                   MOVEL(P)  EABPFL        XXVAL
     C                   MOVE      EAF(16)       XXMSG#
     C                   MOVEL(P)  EAD(16)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID SYSTEM TYPE
     C*
     C     EASYS         IFNE      'P'
     C     EASYS         ANDNE     'H'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EASYS'       XXFIEL
     C                   MOVEL(P)  EASYS         XXVAL
     C                   MOVE      EAF(17)       XXMSG#
     C                   MOVEL(P)  EAD(17)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* FEIN MUST BE VALID
     C*
     C                   Z-ADD     EAFEIN        KKFEIN
     C                   Z-ADD     EAMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAFEIN'      XXFIEL
     C                   MOVEL(P)  EAFEIN        XXVAL
     C                   MOVE      EAF(18)       XXMSG#
     C                   MOVEL(P)  EAD(18)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID PAYMENT LINK
     C*
     C     EAPLNK        IFNE      '1'
     C     EAPLNK        ANDNE     '2'
     C     EAPLNK        ANDNE     '3'
     C     EAPLNK        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAPLNK'      XXFIEL
     C                   MOVEL(P)  EAPLNK        XXVAL
     C                   MOVE      EAF(19)       XXMSG#
     C                   MOVEL(P)  EAD(19)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID BRS INVOICE DATE
     C*
     C     EABIDT        IFNE      *ZEROS
     C                   Z-ADD     EABIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EABIDT'      XXFIEL
     C                   MOVEL(P)  EABIDT        XXVAL
     C                   MOVE      EAF(20)       XXMSG#
     C                   MOVEL(P)  EAD(20)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID NETWORK INVOICE DATE
     C*
     C     EANIDT        IFNE      *ZEROS
     C                   Z-ADD     EANIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EANIDT'      XXFIEL
     C                   MOVEL(P)  EANIDT        XXVAL
     C                   MOVE      EAF(21)       XXMSG#
     C                   MOVEL(P)  EAD(21)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID AUTO CHECK PROCESS FLAG.
     C*
     C     EAAPPF        IFNE      'Y'
     C     EAAPPF        ANDNE     'P'
     C     EAAPPF        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EAAPPF'      XXFIEL
     C                   MOVEL(P)  EAAPPF        XXVAL
     C                   MOVE      EAF(22)       XXMSG#
     C                   MOVEL(P)  EAD(22)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID T/M BILLING FLAG.
     C*
     C     EATMFL        IFNE      'Y'
     C     EATMFL        ANDNE     ' '
     C                   MOVEL(P)  'FAEAP'       XXFILE
     C                   MOVEL(P)  'EATMFL'      XXFIEL
     C                   MOVEL(P)  EATMFL        XXVAL
     C                   MOVE      EAF(23)       XXMSG#
     C                   MOVEL(P)  EAD(23)       XXDESC
     C                   Z-ADD     EARCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAEDP
     C***************************************************************
     C     WRNEDP        BEGSR
     C*
     C* SOCIAL SECURITY # IS BLANK
     C*
     C                   MOVEL(P)  EDSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEDP'       XXFILE
     C                   MOVEL(P)  'EDSS#'       XXFIEL
     C                   MOVEL(P)  EDSS#         XXVAL
     C                   MOVE      EDF(1)        XXMSG#
     C                   MOVEL(P)  EDD(1)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     EDACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEDP'       XXFILE
     C                   MOVEL(P)  'EDACDT'      XXFIEL
     C                   MOVEL(P)  EDACDT        XXVAL
     C                   MOVE      EDF(2)        XXMSG#
     C                   MOVEL(P)  EDD(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID EDI FLAG
     C*
     C     EDEDI         IFNE      'Y'
     C     EDEDI         ANDNE     'N'
     C     EDEDI         ANDNE     ' '
     C                   MOVEL(P)  'FAEDP'       XXFILE
     C                   MOVEL(P)  'EDEDI'       XXFIEL
     C                   MOVEL(P)  EDEDI         XXVAL
     C                   MOVE      EDF(3)        XXMSG#
     C                   MOVEL(P)  EDD(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAEEP
     C***************************************************************
     C     WRNEEP        BEGSR
     C*
     C* SOCIAL SECURITY # IS BLANK
     C*
     C                   MOVEL(P)  EESS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEEP'       XXFILE
     C                   MOVEL(P)  'EESS#'       XXFIEL
     C                   MOVEL(P)  EESS#         XXVAL
     C                   MOVE      EEF(1)        XXMSG#
     C                   MOVEL(P)  EED(1)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     EEACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEEP'       XXFILE
     C                   MOVEL(P)  'EEACDT'      XXFIEL
     C                   MOVEL(P)  EEACDT        XXVAL
     C                   MOVE      EEF(2)        XXMSG#
     C                   MOVEL(P)  EED(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* SEQUENCE NUMBER CAN'T BE ZERO
     C*
     C     EESEQ#        IFEQ      *ZEROS
     C                   MOVEL(P)  'FAEEP'       XXFILE
     C                   MOVEL(P)  'EESEQ#'      XXFIEL
     C                   MOVEL(P)  EESEQ#        XXVAL
     C                   MOVE      EEF(3)        XXMSG#
     C                   MOVEL(P)  EED(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* EOB CODE OR NOTE MUST BE ENTERED
     C*
     C     EEEOB         IFEQ      *BLANKS
     C     EENOT1        ANDEQ     *BLANKS
     C                   MOVEL(P)  'FAEEP'       XXFILE
     C                   MOVEL(P)  'EEEOB'       XXFIEL
     C                   MOVEL(P)  EEEOB         XXVAL
     C                   MOVE      EEF(4)        XXMSG#
     C                   MOVEL(P)  EED(4)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID PRINT EOB/NOTE FLAG
     C*
     C     EEPRNT        IFNE      'Y'
     C     EEPRNT        ANDNE     'N'
     C     EEPRNT        ANDNE     ' '
     C                   MOVEL(P)  'FAEEP'       XXFILE
     C                   MOVEL(P)  'EEPRNT'      XXFIEL
     C                   MOVEL(P)  EEPRNT        XXVAL
     C                   MOVE      EEF(5)        XXMSG#
     C                   MOVEL(P)  EED(5)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAEHP
     C***************************************************************
     C     WRNEHP        BEGSR
     C*
     C* SOCIAL SECURITY # IS BLANK
     C*
     C                   MOVEL(P)  EHSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHSS#'       XXFIEL
     C                   MOVEL(P)  EHSS#         XXVAL
     C                   MOVE      EHF(1)        XXMSG#
     C                   MOVEL(P)  EHD(1)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     EHACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHACDT'      XXFIEL
     C                   MOVEL(P)  EHACDT        XXVAL
     C                   MOVE      EHF(2)        XXMSG#
     C                   MOVEL(P)  EHD(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID DATE RECON SENT TO CARRIER
     C*
     C     EHDFC         IFNE      *ZEROS
     C                   Z-ADD     EHDFC         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHDFC'       XXFIEL
     C                   MOVEL(P)  EHDFC         XXVAL
     C                   MOVE      EHF(3)        XXMSG#
     C                   MOVEL(P)  EHD(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* PRINCIPLE DIAGNOSIS CODE MUST BE VALID
     C*
     C                   MOVEL(P)  EHDIA1        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHDIA1'      XXFIEL
     C                   MOVEL(P)  EHDIA1        XXVAL
     C                   MOVE      EHF(4)        XXMSG#
     C                   MOVEL(P)  EHD(4)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DIAGNOSIS CODE 2 MUST BE VALID IF NOT BLANK
     C*
     C     EHDIA2        IFNE      *BLANKS
     C                   MOVEL(P)  EHDIA2        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHDIA2'      XXFIEL
     C                   MOVEL(P)  EHDIA2        XXVAL
     C                   MOVE      EHF(5)        XXMSG#
     C                   MOVEL(P)  EHD(5)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* DIAGNOSIS CODE 3 MUST BE VALID IF NOT BLANK
     C*
     C     EHDIA3        IFNE      *BLANKS
     C                   MOVEL(P)  EHDIA3        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHDIA3'      XXFIEL
     C                   MOVEL(P)  EHDIA3        XXVAL
     C                   MOVE      EHF(6)        XXMSG#
     C                   MOVEL(P)  EHD(6)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* DIAGNOSIS CODE 4 MUST BE VALID IF NOT BLANK
     C*
     C     EHDIA4        IFNE      *BLANKS
     C                   MOVEL(P)  EHDIA4        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHDIA4'      XXFIEL
     C                   MOVEL(P)  EHDIA4        XXVAL
     C                   MOVE      EHF(7)        XXMSG#
     C                   MOVEL(P)  EHD(7)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID EDI FLAG
     C*
     C     EHEDI         IFNE      'Y'
     C     EHEDI         ANDNE     'N'
     C     EHEDI         ANDNE     ' '
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHEDI'       XXFIEL
     C                   MOVEL(P)  EHEDI         XXVAL
     C                   MOVE      EHF(8)        XXMSG#
     C                   MOVEL(P)  EHD(8)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID EMPLOYMENT RELATED FLAG
     C*
     C     EHEMPL        IFNE      'Y'
     C     EHEMPL        ANDNE     'N'
     C     EHEMPL        ANDNE     ' '
     C                   MOVEL(P)  'FAEHP'       XXFILE
     C                   MOVEL(P)  'EHEMPL'      XXFIEL
     C                   MOVEL(P)  EHEDI         XXVAL
     C                   MOVE      EHF(9)        XXMSG#
     C                   MOVEL(P)  EHD(9)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAEOP
     C***************************************************************
     C     WRNEOP        BEGSR
     C*
     C* Amounts should crossfoot
     C*
     C     EOCHRG        SUB       EOPAID        ZZCHRG
     C                   SUB       EOOTHR        ZZCHRG
     C                   SUB       EOOTH2        ZZCHRG
     C                   SUB       EONDIS        ZZCHRG
     C                   SUB       EOMRA         ZZCHRG
     C*
     C     ZZCHRG        IFNE      *ZEROS
     C     EOBLST        ANDEQ     'C'
     C*
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  *BLANKS       XXFIEL
     C                   MOVEL(P)  *BLANKS       XXVAL
     C                   MOVE      ER2(1)        XXMSG#
     C                   MOVEL(P)  ED2(1)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C*
     C                   EXSR      WRTERR
     C*
     C                   ENDIF
     C*
     C* INVALID CLAIM
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOCASE'      XXFIEL
     C                   MOVE      EOSUB#        XXSUB#            5
     C                   MOVE      EOFND         XXFND             3
     C                   MOVE      EOFYR         XXFYR             3
     C                   MOVE      EOCASE        XXCASE            7
     C     XXSUB#        CAT(P)    '/':0         XXVAL
     C                   CAT       XXFND:0       XXVAL
     C                   CAT       '/':0         XXVAL
     C                   CAT       XXFYR:0       XXVAL
     C                   CAT       '/':0         XXVAL
     C                   CAT       XXCASE:0      XXVAL
     C                   MOVE      EOF(1)        XXMSG#
     C                   MOVEL(P)  EOD(1)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID TRANSACTION DATE
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOTRDT'      XXFIEL
     C                   MOVEL(P)  EOTRDT        XXVAL
     C                   MOVE      EOF(2)        XXMSG#
     C                   MOVEL(P)  EOD(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* FEIN MUST BE VALID
     C*
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOFEIN'      XXFIEL
     C                   MOVEL(P)  EOFEIN        XXVAL
     C                   MOVE      EOF(3)        XXMSG#
     C                   MOVEL(P)  EOD(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   SETOFF                                       99
     C     EOFRDT        IFEQ      *ZEROS
     C     EOTODT        ANDEQ     *ZEROS
     C     CAKY          CHAIN     PCA01                              99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* INVALID FROM DATE
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOFRDT'      XXFIEL
     C                   MOVEL(P)  EOFRDT        XXVAL
     C                   MOVE      EOF(4)        XXMSG#
     C                   MOVEL(P)  EOD(4)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID TO DATE
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOTODT'      XXFIEL
     C                   MOVEL(P)  EOTODT        XXVAL
     C                   MOVE      EOF(5)        XXMSG#
     C                   MOVEL(P)  EOD(5)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* PROVIDER NAME IS BLANK
     C*
     C     EOPNAM        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOPNAM'      XXFIEL
     C                   MOVEL(P)  EOPNAM        XXVAL
     C                   MOVE      EOF(6)        XXMSG#
     C                   MOVEL(P)  EOD(6)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PROVIDER ADDRESS 1 IS BLANK
     C*
     C     EOADR1        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOADR1'      XXFIEL
     C                   MOVEL(P)  EOADR1        XXVAL
     C                   MOVE      EOF(7)        XXMSG#
     C                   MOVEL(P)  EOD(7)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PROVIDER CITY IS BLANK
     C*
     C     EOCTY         IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOCTY'       XXFIEL
     C                   MOVEL(P)  EOCTY         XXVAL
     C                   MOVE      EOF(8)        XXMSG#
     C                   MOVEL(P)  EOD(8)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PROVIDER STATE IS BLANK
     C*
     C     EOST          IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOST'        XXFIEL
     C                   MOVEL(P)  EOST          XXVAL
     C                   MOVE      EOF(9)        XXMSG#
     C                   MOVEL(P)  EOD(9)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PROVIDER ZIP IS ZERO
     C*
     C     EOZIP         IFEQ      *ZEROS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOZIP'       XXFIEL
     C                   MOVEL(P)  EOZIP         XXVAL
     C                   MOVE      EOF(10)       XXMSG#
     C                   MOVEL(P)  EOD(10)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* EOMB NUMBER IS ZERO
     C*
     C     EOEOB#        IFEQ      *ZEROS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOEOB#'      XXFIEL
     C                   MOVEL(P)  EOEOB#        XXVAL
     C                   MOVE      EOF(11)       XXMSG#
     C                   MOVEL(P)  EOD(11)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PRINT FLAG IS INVALID
     C*
     C     EOSTAT        IFNE      'P'
     C     EOSTAT        ANDNE     'R'
     C     EOSTAT        ANDNE     ' '
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOSTAT'      XXFIEL
     C                   MOVEL(P)  EOSTAT        XXVAL
     C                   MOVE      EOF(12)       XXMSG#
     C                   MOVEL(P)  EOD(12)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* MAINTENANCE USER IS BLANK
     C*
     C     EOUSER        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOUSER'      XXFIEL
     C                   MOVEL(P)  EOUSER        XXVAL
     C                   MOVE      EOF(13)       XXMSG#
     C                   MOVEL(P)  EOD(13)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INITIAL USER IS BLANK
     C*
     C     EOIUSR        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOIUSR'      XXFIEL
     C                   MOVEL(P)  EOIUSR        XXVAL
     C                   MOVE      EOF(14)       XXMSG#
     C                   MOVEL(P)  EOD(14)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* SUBMISSION DATE MUST BE VALID IF NOT EQUAL TO ZERO
     C*
     C     EOWCCK        IFNE      *ZEROS
     C     EOAST         ANDEQ     'FL'
     C                   Z-ADD     EOWCCK        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOWCCK'      XXFIEL
     C                   MOVEL(P)  EOWCCK        XXVAL
     C                   MOVE      EOF(15)       XXMSG#
     C                   MOVEL(P)  EOD(15)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* DIAGNOSIS CODE 1 IS BLANK
     C*
     C     EOBTYP        IFEQ      'N'
     C     EOBTYP        OREQ      'H'
     C     EODIA1        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EODIA1'      XXFIEL
     C                   MOVEL(P)  EODIA1        XXVAL
     C                   MOVE      EOF(16)       XXMSG#
     C                   MOVEL(P)  EOD(16)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* TAPE FLAG MUST BE VALID
     C*
     C     EOTAPE        IFNE      'Y'
     C     EOTAPE        ANDNE     'N'
     C     EOTAPE        ANDNE     *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOTAPE'      XXFIEL
     C                   MOVEL(P)  EOTAPE        XXVAL
     C                   MOVE      EOF(17)       XXMSG#
     C                   MOVEL(P)  EOD(17)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ADJUDICATION STATE IS INVALID
     C*
     C                   MOVEL(P)  EOAST         KKKEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOAST'       XXFIEL
     C                   MOVEL(P)  EOAST         XXVAL
     C                   MOVE      EOF(18)       XXMSG#
     C                   MOVEL(P)  EOD(18)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* PRE-CERT IS INVALID
     C*
     C     EOCERT        IFNE      *BLANKS
     C     PCKY          CHAIN     PPC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOCERT'      XXFIEL
     C                   MOVEL(P)  EOCERT        XXVAL
     C                   MOVE      EOF(19)       XXMSG#
     C                   MOVEL(P)  EOD(19)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* SOCIAL SECURITY # IS BLANK
     C*
     C                   MOVEL(P)  EOSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOSS#'       XXFIEL
     C                   MOVEL(P)  EOSS#         XXVAL
     C                   MOVE      EOF(20)       XXMSG#
     C                   MOVEL(P)  EOD(20)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOACDT'      XXFIEL
     C                   MOVEL(P)  EOACDT        XXVAL
     C                   MOVE      EOF(21)       XXMSG#
     C                   MOVEL(P)  EOD(21)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BILL TYPE IS INVALID
     C*
     C     EOBTYP        IFNE      'N'
     C     EOBTYP        ANDNE     'H'
     C     EOBTYP        ANDNE     'X'
     C     EOBTYP        ANDNE     'D'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOBTYP'      XXFIEL
     C                   MOVEL(P)  EOBTYP        XXVAL
     C                   MOVE      EOF(22)       XXMSG#
     C                   MOVEL(P)  EOD(22)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BILL STATUS IS INVALID
     C*
     C     EOBLST        IFNE      'C'
     C     EOBLST        ANDNE     'P'
     C     EOBLST        ANDNE     'X'
     C     EOBLST        ANDNE     'D'
     C     EOBLST        ANDNE     'U'
     C     EOBLST        ANDNE     'R'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOBLST'      XXFIEL
     C                   MOVEL(P)  EOBLST        XXVAL
     C                   MOVE      EOF(23)       XXMSG#
     C                   MOVEL(P)  EOD(23)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ADJUDICATION CODE IS INVALID
     C*
     C                   MOVEL(P)  EOCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOCSCD'      XXFIEL
     C                   MOVEL(P)  EOCSCD        XXVAL
     C                   MOVE      EOF(24)       XXMSG#
     C                   MOVEL(P)  EOD(24)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID DATE SENT TO CARRIER
     C*
     C     EOCSDT        IFNE      *ZEROS
     C                   Z-ADD     EOCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOCSDT'      XXFIEL
     C                   MOVEL(P)  EOCSDT        XXVAL
     C                   MOVE      EOF(25)       XXMSG#
     C                   MOVEL(P)  EOD(25)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID DATE RECEIVED BY CARRIER
     C*
     C     EOCRDT        IFNE      *ZEROS
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOCRDT'      XXFIEL
     C                   MOVEL(P)  EOCRDT        XXVAL
     C                   MOVE      EOF(26)       XXMSG#
     C                   MOVEL(P)  EOD(26)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* CHARGE AMOUNT IS ZERO
     C*
     C     EOCHRG        IFEQ      *ZEROS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOCHRG'      XXFIEL
     C                   MOVEL(P)  EOCHRG        XXVAL
     C                   MOVE      EOF(27)       XXMSG#
     C                   MOVEL(P)  EOD(27)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* NETWORK IS INVALID
     C*
     C     EONDIS        IFNE      *ZEROS
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EONTWK'      XXFIEL
     C                   MOVEL(P)  EONTWK        XXVAL
     C                   MOVE      EOF(28)       XXMSG#
     C                   MOVEL(P)  EOD(28)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* LAST NAME IS BLANK
     C*
     C     EOLSN         IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOLSN'       XXFIEL
     C                   MOVEL(P)  EOLSN         XXVAL
     C                   MOVE      EOF(29)       XXMSG#
     C                   MOVEL(P)  EOD(29)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* FIRST NAME IS BLANK
     C*
     C     EOFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOFSN'       XXFIEL
     C                   MOVEL(P)  EOFSN         XXVAL
     C                   MOVE      EOF(30)       XXMSG#
     C                   MOVEL(P)  EOD(30)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID STATE SUBMISSION DATE
     C*
     C     EOSBDT        IFNE      *BLANKS
     C                   MOVE      EOSBDT        X1DATE            8 0
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEOP'       XXFILE
     C                   MOVEL(P)  'EOSBDT'      XXFIEL
     C                   MOVEL(P)  EOSBDT        XXVAL
     C                   MOVE      EOF(31)       XXMSG#
     C                   MOVEL(P)  EOD(31)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAEPP
     C***************************************************************
     C     WRNEPP        BEGSR
     C*
     C* SOCIAL SECURITY # IS BLANK
     C*
     C                   MOVEL(P)  EPSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEPP'       XXFILE
     C                   MOVEL(P)  'EPSS#'       XXFIEL
     C                   MOVEL(P)  EPSS#         XXVAL
     C                   MOVE      EPF(1)        XXMSG#
     C                   MOVEL(P)  EPD(1)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     EPACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEPP'       XXFILE
     C                   MOVEL(P)  'EPACDT'      XXFIEL
     C                   MOVEL(P)  EPACDT        XXVAL
     C                   MOVE      EPF(2)        XXMSG#
     C                   MOVEL(P)  EPD(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID EDI FLAG
     C*
     C     EPEDI         IFNE      'Y'
     C     EPEDI         ANDNE     'N'
     C     EPEDI         ANDNE     ' '
     C                   MOVEL(P)  'FAEPP'       XXFILE
     C                   MOVEL(P)  'EPEDI'       XXFIEL
     C                   MOVEL(P)  EPEDI         XXVAL
     C                   MOVE      EPF(3)        XXMSG#
     C                   MOVEL(P)  EPD(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAESP
     C***************************************************************
     C     WRNESP        BEGSR
     C*
     C* SOCIAL SECURITY # IS BLANK
     C*
     C                   MOVEL(P)  ESSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  'ESSS#'       XXFIEL
     C                   MOVEL(P)  ESSS#         XXVAL
     C                   MOVE      ESF(1)        XXMSG#
     C                   MOVEL(P)  ESD(1)        XXDESC
     C                   Z-ADD     ESRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     ESACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  'ESACDT'      XXFIEL
     C                   MOVEL(P)  ESACDT        XXVAL
     C                   MOVE      ESF(2)        XXMSG#
     C                   MOVEL(P)  ESD(2)        XXDESC
     C                   Z-ADD     ESRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID STATUS TYPE
     C*
     C     ESBLST        IFNE      'P'
     C     ESBLST        ANDNE     'C'
     C     ESBLST        ANDNE     'U'
     C     ESBLST        ANDNE     'D'
     C     ESBLST        ANDNE     'R'
     C     ESBLST        ANDNE     'X'
     C     ESBLST        ANDNE     'E'
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  'ESBLST'      XXFIEL
     C                   MOVEL(P)  ESBLST        XXVAL
     C                   MOVE      ESF(3)        XXMSG#
     C                   MOVEL(P)  ESD(3)        XXDESC
     C                   Z-ADD     ESRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID STATUS CODE
     C*
     C     ESBLST        IFEQ      'R'
     C     ESBLST        OREQ      'P'
     C     ESBLST        OREQ      'D'
     C     ESBLST        OREQ      'E'
     C     ESBLST        OREQ      'O'
     C     ESSTCD        IFNE      'CPY'
     C                   MOVEL(P)  ESBLST        KKTYPE
     C                   MOVEL(P)  ESSTCD        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  'ESSTCD'      XXFIEL
     C                   MOVEL(P)  ESSTCD        XXVAL
     C                   MOVE      ESF(4)        XXMSG#
     C                   MOVEL(P)  ESD(4)        XXDESC
     C                   Z-ADD     ESRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* REASON IS BLANK
     C*
     C     ESREAS        IFEQ      *BLANKS
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  'ESREAS'      XXFIEL
     C                   MOVEL(P)  ESREAS        XXVAL
     C                   MOVE      ESF(5)        XXMSG#
     C                   MOVEL(P)  ESD(5)        XXDESC
     C                   Z-ADD     ESRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID DATE RECON RECIEVED BY CARRIER
     C*
     C     ESRRDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     ESRRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  'ESRRDT'      XXFIEL
     C                   MOVEL(P)  ESRRDT        XXVAL
     C                   MOVE      ESF(6)        XXMSG#
     C                   MOVEL(P)  ESD(6)        XXDESC
     C                   Z-ADD     ESRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID DATE RECON SENT TO CARRIER
     C*
     C     ESRSDT        IFNE      *ZEROS
     C                   Z-ADD     ESRSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAESP'       XXFILE
     C                   MOVEL(P)  'ESRSDT'      XXFIEL
     C                   MOVEL(P)  ESRSDT        XXVAL
     C                   MOVE      ESF(7)        XXMSG#
     C                   MOVEL(P)  ESD(7)        XXDESC
     C                   Z-ADD     ESRCN         XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* WARNING SUBROUTINE - FOR FAEUP
     C***************************************************************
     C     WRNEUP        BEGSR
     C*
     C* SOCIAL SECURITY # IS BLANK
     C*
     C                   MOVEL(P)  EUSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUSS#'       XXFIEL
     C                   MOVEL(P)  EUSS#         XXVAL
     C                   MOVE      EUF(1)        XXMSG#
     C                   MOVEL(P)  EUD(1)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* ACCIDENT DATE MUST BE VALID
     C*
     C                   Z-ADD     EUACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUACDT'      XXFIEL
     C                   MOVEL(P)  EUACDT        XXVAL
     C                   MOVE      EUF(2)        XXMSG#
     C                   MOVEL(P)  EUD(2)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* BILL TYPE MUST BE VALID
     C*
     C     EUBLTY        IFNE      *BLANKS
     C     EOAST         CAT(P)    'BT':0        BTTBID            4
     C                   MOVEL(P)  BTTBID        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        BTTBID
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  BTTBID        KKTBID
     C                   MOVEL(P)  EUBLTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUBLTY'      XXFIEL
     C                   MOVEL(P)  EUBLTY        XXVAL
     C                   MOVE      EUF(3)        XXMSG#
     C                   MOVEL(P)  EUD(3)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID STATEMENT COVERS FROM DATE
     C*
     C     EUSCPF        IFNE      *ZEROS
     C                   Z-ADD     EUSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUSCPF'      XXFIEL
     C                   MOVEL(P)  EUSCPF        XXVAL
     C                   MOVE      EUF(4)        XXMSG#
     C                   MOVEL(P)  EUD(4)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID STATEMENT COVERS TO DATE
     C*
     C     EUSCPT        IFNE      *ZEROS
     C                   Z-ADD     EUSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUSCPT'      XXFIEL
     C                   MOVEL(P)  EUSCPT        XXVAL
     C                   MOVE      EUF(5)        XXMSG#
     C                   MOVEL(P)  EUD(5)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID ADMISSION DATE
     C*
     C     EUADDT        IFNE      *ZEROS
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUADDT'      XXFIEL
     C                   MOVEL(P)  EUADDT        XXVAL
     C                   MOVE      EUF(6)        XXMSG#
     C                   MOVEL(P)  EUD(6)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID ADMISSION HOUR
     C*
     C     EUADHR        IFNE      *ZEROS
     C     EUADHR        ANDGT     24
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUADHR'      XXFIEL
     C                   MOVEL(P)  EUADHR        XXVAL
     C                   MOVE      EUF(7)        XXMSG#
     C                   MOVEL(P)  EUD(7)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID DISCHARGE HOUR
     C*
     C     EUDCHR        IFNE      *ZEROS
     C     EUDCHR        ANDGT     24
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUDCHR'      XXFIEL
     C                   MOVEL(P)  EUDCHR        XXVAL
     C                   MOVE      EUF(8)        XXMSG#
     C                   MOVEL(P)  EUD(8)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID OCCURRENCE DATE 1
     C*
     C     EUODT1        IFNE      *ZEROS
     C                   Z-ADD     EUODT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUODT1'      XXFIEL
     C                   MOVEL(P)  EUODT1        XXVAL
     C                   MOVE      EUF(9)        XXMSG#
     C                   MOVEL(P)  EUD(9)        XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID OCCURRENCE DATE 2
     C*
     C     EUODT2        IFNE      *ZEROS
     C                   Z-ADD     EUODT2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUODT2'      XXFIEL
     C                   MOVEL(P)  EUODT2        XXVAL
     C                   MOVE      EUF(10)       XXMSG#
     C                   MOVEL(P)  EUD(10)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID OCCURRENCE DATE 3
     C*
     C     EUODT3        IFNE      *ZEROS
     C                   Z-ADD     EUODT3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUODT3'      XXFIEL
     C                   MOVEL(P)  EUODT3        XXVAL
     C                   MOVE      EUF(11)       XXMSG#
     C                   MOVEL(P)  EUD(11)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID OCCURRENCE DATE 4
     C*
     C     EUODT4        IFNE      *ZEROS
     C                   Z-ADD     EUODT4        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUODT4'      XXFIEL
     C                   MOVEL(P)  EUODT4        XXVAL
     C                   MOVE      EUF(12)       XXMSG#
     C                   MOVEL(P)  EUD(12)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID OCCURRENCE SPAN FROM DATE
     C*
     C     EUOSFD        IFNE      *ZEROS
     C                   Z-ADD     EUOSFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUOSFD'      XXFIEL
     C                   MOVEL(P)  EUOSFD        XXVAL
     C                   MOVE      EUF(13)       XXMSG#
     C                   MOVEL(P)  EUD(13)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID OCCURRENCE SPAN TO DATE
     C*
     C     EUOSTD        IFNE      *ZEROS
     C                   Z-ADD     EUOSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUOSTD'      XXFIEL
     C                   MOVEL(P)  EUOSTD        XXVAL
     C                   MOVE      EUF(14)       XXMSG#
     C                   MOVEL(P)  EUD(14)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID INTERNAL CONTROL NUMBER
     C*
     C     EUICN         IFNE      *BLANKS
     C                   MOVE(P)   'SVTY'        KKTBID
     C                   MOVEL(P)  EUICN         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUICN '      XXFIEL
     C                   MOVEL(P)  EUICN         XXVAL
     C                   MOVE      EUF(15)       XXMSG#
     C                   MOVEL(P)  EUD(15)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCICN NE *BL
     C*
     C* INVALID DOCUMENT CONTROL NUMBER
     C*
     C     EUDCN         IFNE      *BLANKS
     C                   MOVE(P)   'SVTZ'        KKTBID
     C                   MOVEL(P)  EUDCN         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUDCN'       XXFIEL
     C                   MOVEL(P)  EUDCN         XXVAL
     C                   MOVE      EUF(16)       XXMSG#
     C                   MOVEL(P)  EUD(16)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDCN NE *BL
     C*
     C* PRINCIPAL DIAGNOSIS CODE MUST BE VALID
     C*
     C                   MOVEL(P)  EUDIA1        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUDIA1'      XXFIEL
     C                   MOVEL(P)  EUDIA1        XXVAL
     C                   MOVE      EUF(17)       XXMSG#
     C                   MOVEL(P)  EUD(17)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* DIAGNOSIS CODE 2 MUST BE VALID IF NOT BLANK
     C*
     C     EUDIA2        IFNE      *BLANKS
     C                   MOVEL(P)  EUDIA2        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUDIA2'      XXFIEL
     C                   MOVEL(P)  EUDIA2        XXVAL
     C                   MOVE      EUF(18)       XXMSG#
     C                   MOVEL(P)  EUD(18)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* DIAGNOSIS CODE 3 MUST BE VALID IF NOT BLANK
     C*
     C     EUDIA3        IFNE      *BLANKS
     C                   MOVEL(P)  EUDIA3        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUDIA3'      XXFIEL
     C                   MOVEL(P)  EUDIA3        XXVAL
     C                   MOVE      EUF(19)       XXMSG#
     C                   MOVEL(P)  EUD(19)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* DIAGNOSIS CODE 4 MUST BE VALID IF NOT BLANK
     C*
     C     EUDIA4        IFNE      *BLANKS
     C                   MOVEL(P)  EUDIA4        KKICD9
     C                   MOVEL(P)  'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUDIA4'      XXFIEL
     C                   MOVEL(P)  EUDIA4        XXVAL
     C                   MOVE      EUF(20)       XXMSG#
     C                   MOVEL(P)  EUD(20)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOAST         IFEQ      'FL'
     C     EOAST         OREQ      'LA'
     C     EOAST         OREQ      'TX'
     C*
     C* PROCEDURE CODE METHOD IS INVALID
     C*
     C     EUPCM         IFNE      '0'
     C     EUPCM         ANDNE     '4'
     C     EUPCM         ANDNE     '5'
     C     EUPCM         ANDNE     '9'
     C     EOBLST        IFEQ      'C'
     C     EORCN         ORGT      *ZEROS
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPCM'       XXFIEL
     C                   MOVEL(P)  EUPCM         XXVAL
     C                   MOVE      EUF(21)       XXMSG#
     C                   MOVEL(P)  EUD(21)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EUPCM         IFNE      '0'
     C*
     C*   OVERRIDE TO FMPRL WITH STATE
     C*
     C     EOAST         IFNE      HDAST
     C     OPENF         IFEQ      '*YES'
     C                   EXSR      OVRPRP
     C                   EXSR      OPNPRP
     C                   MOVEL(P)  '*NO'         OPENF
     C                   ELSE
     C                   EXSR      CLSPRP
     C                   EXSR      DOVPRP
     C                   EXSR      OVRPRP
     C                   EXSR      OPNPRP
     C                   ENDIF
     C                   MOVEL(P)  EOAST         EOAST
     C                   ENDIF
     C*
     C* INVALID PRINCIPLE PROCEDURE CODE
     C*
     C                   SELECT
     C     EUPCM         WHENEQ    '4'
     C                   MOVEL     EUPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC1'      XXFIEL
     C                   MOVEL(P)  EUPRC1        XXVAL
     C                   MOVE      EUF(22)       XXMSG#
     C                   MOVEL(P)  EUD(22)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '5'
     C                   MOVEL     EUPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  EUPRC1        KKCPT4
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC1'      XXFIEL
     C                   MOVEL(P)  EUPRC1        XXVAL
     C                   MOVE      EUF(22)       XXMSG#
     C                   MOVEL(P)  EUD(22)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '9'
     C                   MOVEL(P)  EUPRC1        KKICD9
     C                   MOVE      EUPRC1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC1'      XXFIEL
     C                   MOVEL(P)  EUPRC1        XXVAL
     C                   MOVE      EUF(22)       XXMSG#
     C                   MOVEL(P)  EUD(22)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDSL
     C*
     C* INVALID PRINCIPAL PROCEDURE DATE
     C*
     C     EUPRD1        IFNE      *ZEROS
     C                   Z-ADD     EUPRD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRD1'      XXFIEL
     C                   MOVEL(P)  EUPRD1        XXVAL
     C                   MOVE      EUF(23)       XXMSG#
     C                   MOVEL(P)  EUD(23)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID PROCEDURE CODE 2
     C*
     C                   SELECT
     C     EUPCM         WHENEQ    '4'
     C                   MOVEL     EUPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC2'      XXFIEL
     C                   MOVEL(P)  EUPRC2        XXVAL
     C                   MOVE      EUF(24)       XXMSG#
     C                   MOVEL(P)  EUD(24)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '5'
     C                   MOVEL     EUPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  EUPRC2        KKCPT4
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC2'      XXFIEL
     C                   MOVEL(P)  EUPRC2        XXVAL
     C                   MOVE      EUF(24)       XXMSG#
     C                   MOVEL(P)  EUD(24)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '9'
     C                   MOVEL(P)  EUPRC2        KKICD9
     C                   MOVE      EUPRC2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC2'      XXFIEL
     C                   MOVEL(P)  EUPRC2        XXVAL
     C                   MOVE      EUF(24)       XXMSG#
     C                   MOVEL(P)  EUD(24)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDSL
     C*
     C* INVALID PROCEDURE CODE 3
     C*
     C                   SELECT
     C     EUPCM         WHENEQ    '4'
     C                   MOVEL     EUPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC3'      XXFIEL
     C                   MOVEL(P)  EUPRC3        XXVAL
     C                   MOVE      EUF(25)       XXMSG#
     C                   MOVEL(P)  EUD(25)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '5'
     C                   MOVEL     EUPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  EUPRC3        KKCPT4
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC3'      XXFIEL
     C                   MOVEL(P)  EUPRC3        XXVAL
     C                   MOVE      EUF(25)       XXMSG#
     C                   MOVEL(P)  EUD(25)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '9'
     C                   MOVEL(P)  EUPRC3        KKICD9
     C                   MOVE      EUPRC3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC3'      XXFIEL
     C                   MOVEL(P)  EUPRC3        XXVAL
     C                   MOVE      EUF(25)       XXMSG#
     C                   MOVEL(P)  EUD(25)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDSL
     C*
     C* INVALID PROCEDURE CODE 4
     C*
     C                   SELECT
     C     EUPCM         WHENEQ    '4'
     C                   MOVEL     EUPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC4'      XXFIEL
     C                   MOVEL(P)  EUPRC4        XXVAL
     C                   MOVE      EUF(26)       XXMSG#
     C                   MOVEL(P)  EUD(26)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '5'
     C                   MOVEL     EUPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  EUPRC4        KKCPT4
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC4'      XXFIEL
     C                   MOVEL(P)  EUPRC4        XXVAL
     C                   MOVE      EUF(26)       XXMSG#
     C                   MOVEL(P)  EUD(26)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  *IN99 = *ON
     C     EUPCM         WHENEQ    '9'
     C                   MOVEL(P)  EUPRC4        KKICD9
     C                   MOVE      EUPRC4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRC4'      XXFIEL
     C                   MOVEL(P)  EUPRC4        XXVAL
     C                   MOVE      EUF(26)       XXMSG#
     C                   MOVEL(P)  EUD(26)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDSL
     C*
     C* INVALID PROCEDURE DATE 2
     C*
     C     EUPRD2        IFNE      *ZEROS
     C                   Z-ADD     EUPRD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRD2'      XXFIEL
     C                   MOVEL(P)  EUPRD2        XXVAL
     C                   MOVE      EUF(27)       XXMSG#
     C                   MOVEL(P)  EUD(27)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID PROCEDURE DATE 3
     C*
     C     EUPRD3        IFNE      *ZEROS
     C                   Z-ADD     EUPRD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRD3'      XXFIEL
     C                   MOVEL(P)  EUPRD3        XXVAL
     C                   MOVE      EUF(28)       XXMSG#
     C                   MOVEL(P)  EUD(28)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID PROCEDURE DATE 4
     C*
     C     EUPRD4        IFNE      *ZEROS
     C                   Z-ADD     EUPRD4        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRD4'      XXFIEL
     C                   MOVEL(P)  EUPRD4        XXVAL
     C                   MOVE      EUF(29)       XXMSG#
     C                   MOVEL(P)  EUD(29)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID PROCEDURE DATE 5
     C*
     C     EUPRD5        IFNE      *ZEROS
     C                   Z-ADD     EUPRD5        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRD5'      XXFIEL
     C                   MOVEL(P)  EUPRD5        XXVAL
     C                   MOVE      EUF(30)       XXMSG#
     C                   MOVEL(P)  EUD(30)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID PROCEDURE DATE 6
     C*
     C     EUPRD6        IFNE      *ZEROS
     C                   Z-ADD     EUPRD6        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUPRD6'      XXFIEL
     C                   MOVEL(P)  EUPRD6        XXVAL
     C                   MOVE      EUF(31)       XXMSG#
     C                   MOVEL(P)  EUD(31)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID EDI FLAG
     C*
     C     EUEDI         IFNE      'Y'
     C     EUEDI         ANDNE     'N'
     C     EUEDI         ANDNE     ' '
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUEDI'       XXFIEL
     C                   MOVEL(P)  EUEDI         XXVAL
     C                   MOVE      EUF(32)       XXMSG#
     C                   MOVEL(P)  EUD(32)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C*
     C* INVALID SUBMISSION DATE
     C*
     C     EUSBDT        IFNE      *ZEROS
     C                   Z-ADD     EUSBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUSBDT'      XXFIEL
     C                   MOVEL(P)  EUSBDT        XXVAL
     C                   MOVE      EUF(33)       XXMSG#
     C                   MOVEL(P)  EUD(33)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID DISCHARGE DATE
     C*
     C     EUDCDT        IFNE      *ZEROS
     C                   Z-ADD     EUDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  'FAEUP'       XXFILE
     C                   MOVEL(P)  'EUDCDT'      XXFIEL
     C                   MOVEL(P)  EUDCDT        XXVAL
     C                   MOVE      EUF(34)       XXMSG#
     C                   MOVEL(P)  EUD(34)       XXDESC
     C                   Z-ADD     *ZEROS        XXRCN
     C                   EXSR      WRTERR
     C                   ENDIF
     C                   ENDIF
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
     C* DOVPRP - DELETES OVERRIDE OF FMPRL
     C*****************************************************************
     C     DOVPRP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
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
     C* OVRPRP - OVERRIDES FMPRL TO STATE FILE
     C*****************************************************************
     C     OVRPRP        BEGSR
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
** ER1/ED1 - File Errors/Descriptions       |
01001There are no FAEAP records for this EOMB
01002There are no FAEDP records for this EOMB
01003There are no FAEHP records for this EOMB
01004There are no FAEPP records for this EOMB
01005There are no FAESP records for this EOMB
01006There are no FAEUP records for this EOMB
** ER2/ED2 - Amount Errors/Descriptions     |
02001FAEOP amount fields do not cross-foot
02002FAEAP amount fields do not cross-foot
02003Sum of FAEAP Charges do not equal FAEOP
02004Sum of FAEAP Payments do not equal FAEOP
02005Sum of FAEAP Other do not equal FAEOP
02006Sum of FAEAP Other 2 do not equal FAEOP
02007Sum of FAEAP Network do not equal FAEOP
02008Sum of FAEAP MRA do not equal FAEOP
** EAF/EAD - FAEAP Field Errors/Description |
03001Social Security # is invalid.
03002Accident date is invalid.
03003Invalid transaction date.
03004Maintenance User is blank.
03005Initial User is blank.
03006WC Provider check date is invalid.
03007WC BRS check date is invalid.
03008WC NWS check date is invalid.
03009BRS Invoice H/R flag is invalid.
03010BRS Invoice Process flag is invalid.
03011NWS Invoice H/R flag is invalid.
03012NWS Invoice Process flag is invalid.
03013Invalid EDI sent date.
03014Invalid EDI flag.
03015Invalid EOMB print flag.
03016Invalid Billing process flag.
03017Invalid System.
03018Invalid Provider FEIN/Mod.
03019Invalid Payment Link.
03020Invalid BRS Invoice date.
03021Invalid NWS Invoice date.
03022Invalid Auto check process flag.
03023Invalid T/M billing flag.
** EDF/EDD - FAEDP Field Errors/Description |
04001Social Security # is invalid.
04002Accident date is invalid.
04003Data submission flag is invalid.
** EEF/EED - FAEEP Field Errors/Description |
05001Social Security # is invalid.
05002Accident date is invalid.
05003Sequence number can't be zero.
05004EOB code and EOB note are blank.
05005Invalid Print flag.
** EHF/EHD - FAEHP Field Errors/Description |
06001Social Security # is invalid.
06002Accident date is invalid.
06003Invalid Date first consulted
06004Principle Diagnosis code 1 is invalid
06005Diagnosis code 2 is invalid
06006Diagnosis code 3 is invalid
06007Diagnosis code 4 is invalid
06008Data submission flag is invalid.
06009Employment related flag is invalid.
** EOF/EOD - FAEOP Field Errors/Description |
07001Invalid case.
07002Invalid transaction date.
07003Invalid Provider FEIN/Mod.
07004Invalid FROM date.
07005Invalid TO date.
07006Provider name is blank.
07007Provider address 1 is blank.
07008Provider city is blank.
07009Provider state is blank.
07010Provider zip code is zero.
07011EOMB number is zero.
07012Invalid Print Flag.
07013Maintenance User is blank.
07014Initial User is blank.
07015Florida Submission Date is invalid.
07016Invalid Diagnosis code.
07017Invalid Tape flag.
07018Adjudication state is invalid.
07019Invalid Pre-Cert.
07020Social Security # is invalid.
07021Accident date is invalid.
07022Invalid Bill type.
07023Invalid Bill status.
07024Invalid Adjudication code.
07025Invalid Date sent to carrier.
07026Invalid Date recieved by carrier.
07027Charged amount is zero.
07028Invalid Network Code.
07029Claimant last name is blank
07030Claimant first name is blank
07031Invalid State submission date
** EPF/EPD - FAEPP Field Errors/Description |
08001Social Security # is invalid.
08002Accident date is invalid.
08003Data submission flag is invalid.
** ESF/ESD - FAESP Field Errors/Description |
09001Social Security # is invalid.
09002Accident date is invalid.
09003Status type is invalid.
09004Invalid Status code.
09005Reason is blank.
09006Invalid Date recon rcvd by carrier.
09007Invalid recon sent to carrier.
** EUF/EUD - FAEUP Field Errors/Description |
10001Social Security # is invalid.
10002Accident date is invalid.
10003Invalid Bill type.
10004Statement covers from date invalid.
10005Statement covers to date invalid.
10006Admission date is invalid.
10007Admission hour is invalid.
10008Discharge hour is invalid.
10009Occurrence date 1 is invalid.
10010Occurrence date 2 is invalid.
10011Occurrence date 3 is invalid.
10012Occurrence date 4 is invalid.
10013Occurrence span from date is invalid.
10014Occurrence span to date is invalid.
10015Internal control number is invalid.
10016Document control number is invalid.
10017Principal diagnosis code is invalid.
10018Other diagnosis code 2 is invalid.
10019Other diagnosis code 3 is invalid.
10020Other diagnosis code 4 is invalid.
10021Procedure code method is invalid.
10022Principal procedure code is invalid.
10023Principal procedure date is invalid.
10024Other procedure code 2 is invalid.
10025Other procedure code 3 is invalid.
10026Other procedure code 4 is invalid.
10027Other procedure date 2 is invalid.
10028Other procedure date 3 is invalid.
10029Other procedure date 4 is invalid.
10030Other procedure date 5 is invalid.
10031Other procedure date 6 is invalid.
10032EDI flag invalid.
10033Submission date is invalid.
10034Discharge date is invalid.
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
