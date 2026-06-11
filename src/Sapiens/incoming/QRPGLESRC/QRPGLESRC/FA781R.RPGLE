     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  FA781R - API to Populate EOB Header and Detail Files
     H*****************************************************************
     H*                                                               *
     H*  Called from: F0350R01 - Work With provider Bills Display/Upl *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/16/2013 by C:277/079   Project: 114MJE         *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  02/27/16 C: 307 Support: 40084                               *
     H*                  DX code cut off                              *
     H*                                                               *
     H*  11/04/15 C: 079 Project: 114MTD                              *
     H*                  Add ICD-10 Logic                             *
     H*                                                               *
     H*  02/28/14 C: 296 Support: 84889                               *
     H*                  1. Populate DHMOD from U2MOD only.           *
     H*                                                               *
     H*  10/04/13 C: 079 Support: MC147 83717                         *
     H*                  1. Populate muliline sequence number         *
     H*                  2. Populate billed procedure code            *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     F* Image Cross reference files
     F*
     FOPTIMPTL1 IF   E           K DISK    USROPN
     F*
     F* Transfer files
     F*
     FFAU1L15   UF   E           K DISK
     F*
     FFAU2L1    UF   E           K DISK
     FFAU2L2    UF   E           K DISK
     F                                     RENAME(PU201:PU202)
     F*
     FFAU9P     UF   E             DISK
     F*
     F* Header files
     F*
     FFMCMP     IF   E             DISK
     FFAEOL4    UF A E           K DISK
     F                                     INFDS(DBFDS)
     FFAEAL1    UF A E           K DISK
     FFAEAL37   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFAEEL     IF   E           K DISK
     FFAEEL3    IF A E           K DISK
     F                                     RENAME(PEE01:PEE02)
     FNMNWL     IF   E           K DISK
     FFTBSL     IF   E           K DISK
     FFAESL2    IF A E           K DISK
     FFAEDL     IF A E           K DISK
     FFAEHL     IF A E           K DISK
     FFAEPL     UF A E           K DISK
     FFAEUL     IF A E           K DISK
     F*
     F* Detail files
     F*
     FFDCAP     O    E             DISK
     FFDDAL2    IF A E           K DISK
     FFDDDL     IF A E           K DISK
     FFDDHL     IF A E           K DISK
     FFDDPL     IF A E           K DISK
     FFDDUL     IF A E           K DISK
     F*
     F* Miscellaneous files
     F*
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDCAL19   UF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFMC1L     UF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FSTSTL     IF   E           K DISK
     FFMBEL     IF   E           K DISK
     FFMRCL     IF   E           K DISK
     F*  Fee Schedule Base Control File
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     D CLM             S              1    DIM(20)
     D* Program constants.
     D COMAPI          C                   CONST('FA007R')
     D DIGITS          C                   CONST('0123456789')
     D XXPROV          C                   CONST('0000000000000')
     D XXMCAR          C                   CONST('000000000000')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CLAIM#                 1     20
     D  GROUP                  1      3
     D  YEAR                   4      6
     D  CASE                   7     13
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3    INZ('000')
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11    INZ('000')
     D*
     D                 DS
     D  XXDXR                  1      4  0
     D  DX1                    1      1  0
     D  DX2                    2      2  0
     D  DX3                    3      3  0
     D  DX4                    4      4  0
     D*
     D                 DS
     D  VALUE                  1     11
     D  SIGN                   1      1
     D  AMOUNT                 2     11  2
     D                 DS
     D  U1PROV                 1     13
     D  U1FEI1                 1      9  0
     D  U1MOD1                10     12
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3    INZ('000')
     D  K1RATE                 4      8
     D  K1UNIT                 9     11    INZ('000')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@dbrrn           9 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@DCID           12            FAU9P RRN
     C                   PARM                    @@rrn2            9 0
     C                   PARM                    @@RTCD            1            FAU1P RRN
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
     C                   PARM      '*YYMD'       SRFFMT
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C*
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C     CAPKY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     NWKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNWCD
     C*
     C     EOBKY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    U1EOB#
     C*
     C     EOBKY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    U1RCN
     C*
     C     GZKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKZIPA
     C*
     C     PDKY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     P1KY          KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C     U1KY1         KLIST
     C                   KFLD                    KKDCID
     C*
     C     U2KY1         KLIST
     C                   KFLD                    U1TRID
     C*
     C     U2KY2         KLIST
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     OPKY          KLIST
     C                   KFLD                    KKSDOC
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOSS#         KKSS#
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    EOAST         XXAST
     C     *LIKE         DEFINE    GZZIPA        KKZIPA
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    EASINV        KKSINV
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C     *LIKE         DEFINE    CALIN#        XXLIN#
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    CATAX         XXTAX
     C     *LIKE         DEFINE    EOIDOC        KKSDOC
     C     *LIKE         DEFINE    OPDCID        KKDCID
     C     *LIKE         DEFINE    BEREVI        XXREVI
     C*
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*

     C*================================================================
     C* GETAST - GET THE ADJUDICATION STATE
     C*================================================================
     C     GETAST        BEGSR
     C*
     C                   Select
     C*
     C                   When      u1ast <> *Blanks
     C                   eval      xxast = u1ast
     C*
     C                   Other
     C*
     C                   MOVEL(P)  'UC'          XXAST
     C                   MOVEL(P)  CMAST         KKST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     STINST        ANDEQ     'Y'
     C                   MOVE(P)   CMAST         XXAST
     C*
     C                   ELSE
     C*
     C     P1KY          CHAIN     PP101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     P1ST          ANDNE     *BLANKS
     C*
     C                   MOVEL     P1ST          KEY              10
     C     KEY           CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1ST          XXAST
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   endsl
     C*
     C                   ENDSR
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   EXSR      PGMSTR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C                   CALL      COMAPI        COMPL                  99
     C*
     C     U1KY1         CHAIN     PU101                              98
     C     *IN98         IFEQ      *OFF
     C*
     C                   EXSR      CHKAM1
     C                   EXSR      UPDT1A
     C                   EXSR      WRITER
     C                   EXSR      UPDT1B
     C*
     C                   ENDIF
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - OPEN FILES IF WCMS, ACCESS IMAGE DOCUMENT
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   call      'CHKIMGR'
 1   C                   parm      *blank        ##rtcd            1
     C*
     C* ACCESS THE CLAIM FILE
     C*
     C     @@DBRRN       CHAIN     PCM01                              99
     C*
     C                   IF        *IN99 = *OFF
     C*
     C                   EVAL      KKCO# = CMCO#
     C                   EVAL      KKSUB#= CMSUB#
     C                   EVAL      KKFND = CMFND
     C                   EVAL      KKFYR = CMFYR
     C                   EVAL      KKCASE= CMCASE
     C*
     C                   ENDIF
     C*
     C* ACCESS THE XREF FILE TO GET THE DOCUMENT ID
     C*
     C                   IF        ##rtcd = 'I'
     C*
     C                   OPEN      OPTIMPTL1
     C*
     C                   EVAL      KKSDOC = @@DCID
     C*
     C     OPKY          CHAIN     POP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   EVAL      KKDCID = OPDCID
     C                   ENDIF
     C*
     C                   CLOSE     OPTIMPTL1
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT1A - Update Bill Header Record (FAU1P) - 1ST UPDATE
     C*================================================================
     C     UPDT1A        BEGSR
     C*
     C     U1EOB#        IFEQ      *ZEROS
     C*
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1EOB#
     C                   UPDATE    PC101
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMCO#         U1CO#
     C                   Z-ADD     CMSUB#        U1SUB#
     C                   Z-ADD     CMSUB#        U1SUB#
     C                   Z-ADD     CMFND         U1FND
     C                   Z-ADD     CMFYR         U1FYR
     C                   Z-ADD     CMCASE        U1CASE
     C                   MOVE      CMSS#         U1SS#
     C                   Z-ADD     CMACDT        U1ACDT
     C                   Z-ADD     C1EOB#        U1EOB#
     C*
     C                   UPDATE    PU101
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT1B - Update Bill Header Record (FAU1P) - 2nd UPDATE
     C*================================================================
     C*
     C     UPDT1B        BEGSR
     C*
     C     U1KY1         CHAIN     PU101                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C                   Z-ADD     EOCO#         U1CO#
     C                   Z-ADD     EOSUB#        U1SUB#
     C                   Z-ADD     EOFND         U1FND
     C                   Z-ADD     EOFYR         U1FYR
     C                   Z-ADD     EOCASE        U1CASE
     C                   Z-ADD     EOFEIN        U1PFEI
     C                   Z-ADD     EOMOD         U1MOD
     C                   MOVEL(P)  EOSS#         U1PSS#
     C                   Z-ADD     EOACDT        U1ACDT
     C                   Z-ADD     *ZEROS        U1RCN
     C*
     C                   MOVEL(P)  'C'           U1PRFL
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U1PRDT
     C*
     C                   UPDATE    PU101
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT2A - Update Bill Detail Record (FAU2P) - 1ST UPDATE
     C*================================================================
     C     UPDT2A        BEGSR
     C*
     C                   Z-ADD     CAEOB#        U2EOB#
     C                   Z-ADD     CADSEQ        U2DSEQ
     C*
     C                   UPDATE    PU201
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT2B - Update Bill Detail Record (FAU2P) - 2ND UPDATE
     C*================================================================
     C     UPDT2B        BEGSR
     C*
     C     U2KY2         CHAIN     PU202                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  'C'           U2PRFL
     C                   UPDATE    PU202
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* Get the adjudication state
     C*
     C                   EXSR      GETAST
     C*
     C* Write the header records
     C*
     C                   EXSR      WRTEOP                                       FAEOP
     C                   EXSR      WRTEAP                                       FAEAP
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      WRTEDP                                       FAEDP
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      WRTEUP                                       FAEUP
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      WRTEHP                                       FAEHP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      WRTEPP                                       FAEPP
     C                   ENDSL
     C*
     C* Write the detail records
     C*
     C                   EXSR      WRTDTL
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEOP - Write FAEOP record
     C*================================================================
     C     WRTEOP        BEGSR
     C*
     C* WRITE FIRST EOMB
     C*
     C     EOBKY1        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEO01
     C                   Z-ADD     U1CO#         EOCO#
     C                   Z-ADD     U1FND         EOFND
     C                   Z-ADD     U1FYR         EOFYR
     C                   Z-ADD     U1CASE        EOCASE
     C*
     C                   Z-ADD     U1TRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C                   MOVE      U1FEIN        EOFEIN
     C                   Z-ADD     U1MOD         EOMOD
     C                   MOVE      U1AREA        EOAREA
     C                   MOVE      U1PHN         EOPHN
     C                   MOVEL(P)  U1RLIC        EORLIC
     C*
     C                   Z-ADD     U1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOFRDT
     C*
     C                   Z-ADD     U1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOTODT
     C                   MOVEL(P)  U1NTWK        KKNWCD
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   NW10A3
     C                   CLEAR                   NWFL01
     C                   ENDIF
     C*
     C* FEIN/MOD does not exist. If the full FEIN/Mod was passed in
     C* then use that, otherwise use the FEIN only and set mod = 000
     C*
     C     U1PROV        IFNE      *BLANKS
     C     U1PROV        ANDNE     XXPROV
     C                   MOVE      U1FEI1        KKFEIN
     C                   MOVE      U1MOD1        KKMOD
     C*
     C                   ELSE
     C                   MOVE      U1FEIN        KKFEIN
     C                   Z-ADD     *ZEROS        KKMOD
     C*
     C                   ENDIF
     C*
     C* Get provider info from the file
     C*
     C     PDKY1         CHAIN     PPD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     PDSTAT        ANDNE     'I'
     C*
     C     PDDBA1        IFNE      'Y'
     C*
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVE      PDNAME        EOPNAM
     C*
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EOPNAM
     C*
     C                   ENDIF
     C*
     C                   MOVE      PDADR1        EOADR1
     C                   MOVE      PDADR2        EOADR2
     C*
     C                   ELSE
     C                   MOVE      PDADR1        EOPNAM
     C                   MOVE      PDADR2        EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C*
     C                   ENDIF
     C*
     C                   MOVE      PDCTY         EOCTY
     C                   MOVE      PDST          EOST
     C                   MOVE      PDZIP         EOZIP
     C*
     C* Get provider info from the fields passed in
     C*
     C                   ELSE
     C*
     C     U1PFSN        IFEQ      *BLANKS
     C                   MOVEL(P)  U1PNAM        EOPNAM
     C                   ELSE
     C     U1PFSN        CAT(P)    U1PNAM:1      EOPNAM
     C                   ENDIF
     C                   MOVEL(P)  U1ADR1        EOADR1
     C                   MOVEL(P)  U1ADR2        EOADR2
     C                   MOVEL(P)  U1CTY         EOCTY
     C                   MOVEL(P)  U1ST          EOST
     C*
     C                   MOVEL     U1ZIP         TEST4             4
     C     TEST4         IFEQ      '0000'
     C     TEST4         OREQ      '    '
     C                   MOVE      U1ZIP         ZIP               5
     C                   MOVEL     ZIP           EOZIP
     C                   ELSE
     C                   MOVE      U1ZIP         EOZIP
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL     PDSPCD        EOEOB1
     C*
     C                   MOVEL(P)  'P'           EOBLST
     C                   MOVEL     '1'           EOCSCD
     C*
     C                   Z-ADD     U1EOB#        EOEOB#
     C                   Z-ADD     KKRCN         EORCN
     C*
     C     U1BTYP        IFEQ      'H'
     C                   MOVEL(P)  'H'           EOSYS
     C                   ELSE
     C                   MOVEL(P)  'P'           EOSYS
     C                   ENDIF
     C*
     C                   MOVEL(P)  @@DCID        EOIDOC
     C                   MOVEL(P)  U1CONT        EOCONT
     C                   Z-ADD     U1FFEI        EOFFEI
     C                   MOVEL(P)  U1FNAM        EOFNAM
     C                   Z-ADD     U1FZIP        EOFZIP
     C                   MOVEL(P)  U1BLIC        EOBLIC
     C                   MOVEL(P)  U1BTYC        EOBTYC
     C                   MOVEL(P)  U1RNAM        EORNAM
     C                   Z-ADD     U1RFEI        EOSFEI
     C                   MOVEL(P)  U1RFNM        EORFNM
     C                   MOVEL(P)  U1RLNM        EORLNM
     C                   MOVEL(P)  U1BLID        EOBLID
     C*
     C                   MOVEL(P)  Q1USER        EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL(P)  Q1USER        EOIUSR
     C                   MOVEL     SRDTO         EOIDT
     C*
     C                   MOVEL(P)  U1ICD1        EODIA1
     C                   MOVEL(P)  U1ICD2        EODIA2
     C                   MOVEL(P)  U1ICD3        EODIA3
     C                   MOVEL(P)  U1ICD4        EODIA4
     C*
     C     U1ZIPA        IFNE      *ZEROS
     C                   MOVEL     U1ZIPA        EOFLIC
     C                   ELSE
     C                   MOVEL     *BLANKS       EOFLIC
     C                   ENDIF
     C*
     C                   Z-ADD     1             EO#BLL
     C                   MOVEL     XXAST         EOAST
     C*
     C                   Z-ADD     U1SUB#        EOSUB#
     C*
     C     U1ZIPA        IFNE      *ZEROS
     C                   MOVEL(P)  U1ZIPA        KKZIPA
     C                   ELSE
     C                   MOVEL     *BLANKS       KKZIPA
     C                   ENDIF
     C*
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF
     C*
     C                   MOVEL(P)  U1SS#         EOSS#
     C                   Z-ADD     U1ACDT        EOACDT
     C*
     C                   MOVEL(P)  Q1PGM         EOIPGM
     C                   Z-ADD     DSTIME        EOITIM
     C*
     C                   SELECT
     C     U1BTYP        WHENEQ    'M'
     C                   MOVEL(P)  'N'           EOBTYP
     C     U1BTYP        WHENEQ    'H'
     C                   MOVEL(P)  'H'           EOBTYP
     C     U1BTYP        WHENEQ    'P'
     C                   MOVEL(P)  'X'           EOBTYP
     C     U1BTYP        WHENEQ    'D'
     C                   MOVEL(P)  'D'           EOBTYP
     C                   ENDSL
     C*
     C                   MOVEL(P)  'N'           EOTAPE
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BEEDI         EOTAPE
     C                   MOVEL(P)  BEREVI        EOREVI
     C                   ENDIF
     C*
     C                   Z-ADD     U1CSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOCSDT
     C*
     C                   Z-ADD     U1CRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOCRDT
     C*
     C                   MOVEL     U1TRID        EOSINV
     C                   MOVEL(P)  U1CLM#        EOCID#
     C*
     C                   MOVE      U1CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EOCHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        EOCHRG
     C                   ENDIF
     C*
     C                   MOVEL(P)  U1NTWK        EONTWK
     C*
     C     U1SPCD        IFEQ      *BLANKS
     C                   MOVEL     PDSPCD        EOSPCD
     C                   ELSE
     C                   MOVEL     U1SPCD        EOSPCD
     C                   ENDIF
     C*
     C                   MOVEL(P)  EOSS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CDLSN         EOLSN
     C                   MOVEL(P)  CDFSN         EOFSN
     C                   MOVEL(P)  CDMID         EOMID
     C                   ELSE
     C                   MOVEL     *BLANKS       EOLSN
     C                   MOVEL     *BLANKS       EOFSN
     C                   MOVEL     *BLANKS       EOMID
     C                   ENDIF
     C                   MOVEL(P)  U1NDSC        EONDSC
     C*
     C                   WRITE     PEO01
     C*
     C                   eval      @@Rrn2 = dbrrn
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEAP - Write FAEAP record
     C*================================================================
     C     WRTEAP        BEGSR
     C*
     C     EOBKY2        CHAIN     PEA01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEA01
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C                   Z-ADD     EOTRDT        EATRDT
     C     EOBLST        IFEQ      'P'
     C                   MOVEL(P)  'E'           EAEPFL
     C                   ENDIF
     C*
     C                   MOVE      U1CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EACHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        EACHRG
     C                   ENDIF
     C                   MOVEL(P)  EOTAPE        EAEDI
     C*
     C                   MOVEL(P)  'A'           EASTAT
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        EAUSER
     C                   MOVEL(P)  Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL(P)  Q1USER        EAIUSR
     C                   MOVEL(P)  Q1PGM         EAIPGM
     C                   Z-ADD     DSTIME        EAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAIDT
     C*
     C                   MOVEL(P)  'H'           EABHRF
     C                   MOVEL(P)  'H'           EANHRF
     C                   MOVEL(P)  EOSYS         EASYS
     C*
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C                   MOVEL(P)  U1TRID        EASINV
     C*
     C                   MOVE      EOPNAM        EAPNAM
     C                   MOVE      EOADR1        EAADR1
     C                   MOVE      EOADR2        EAADR2
     C                   MOVE      EOCTY         EACTY
     C                   MOVE      EOST          EAST
     C                   MOVE      EOZIP         EAZIP
     C*
     C                   WRITE     PEA01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEDP - Write FAEDP record
     C*================================================================
     C     WRTEDP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOBKY1        CHAIN     PED01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PED01
     C                   Z-ADD     EOCO#         EDCO#
     C                   Z-ADD     EOSUB#        EDSUB#
     C                   Z-ADD     EOFND         EDFND
     C                   Z-ADD     EOFYR         EDFYR
     C                   Z-ADD     EOCASE        EDCASE
     C                   Z-ADD     EOEOB#        EDEOB#
     C                   MOVEL     EOSS#         EDSS#
     C                   Z-ADD     EOACDT        EDACDT
     C                   MOVEL     U1LIC#        EDLIC#
     C*
     C                   MOVEL     EOTAPE        EDEDI
     C*
     C                   MOVEL     'A'           EDSTAT
     C                   MOVEL     U1PCON        EDACCT
     C                   MOVEL(P)  U1PPLN        EDPPLN
     C                   MOVEL(P)  U1ACCT        EDACCT
     C                   MOVEL(P)  U1RLIC        EDFLIC
     C                   MOVEL(P)  U1RFLC        EDRFLC
     C                   MOVEL(P)  U1ATLC        EDATLC
     C*
     C                   MOVEL(P)  EOUSER        EDUSER
     C                   MOVEL(P)  EOPGM         EDPGM
     C                   Z-ADD     EOTIME        EDTIME
     C                   Z-ADD     EODT          EDDT
     C                   MOVEL(P)  EOUSER        EDIUSR
     C                   MOVEL(P)  EOPGM         EDIPGM
     C                   Z-ADD     EOITIM        EDITIM
     C                   Z-ADD     EOIDT         EDIDT
     C*
     C***** Beg Del *** 11/04/15 *******************************************
     C****************** MOVEL(P)  EODIA1        EDDIA1
     C****************** MOVEL(P)  EODIA2        EDDIA2
     C****************** MOVEL(P)  EODIA3        EDDIA3
     C****************** MOVEL(P)  EODIA4        EDDIA4
     C***** End Del *** 11/04/15 *******************************************
     C***** beg add *** 11/04/15 *******************************************
     C*
     C***** Beg Chg *** 02/27/16 *******************************************
     C****************** Eval      EDICD1001 = EODIA1
     C****************** Eval      EDICD1002 = EODIA2
     C****************** Eval      EDICD1003 = EODIA3
     C****************** Eval      EDICD1004 = EODIA4
     C                   Eval      EDICD1001 = U1ICD1
     C                   Eval      EDICD1002 = U1ICD2
     C                   Eval      EDICD1003 = U1ICD3
     C                   Eval      EDICD1004 = U1ICD4
     C                   Eval      EDICDIND = 'Y'
     C***** End Chg *** 02/27/16 *******************************************
     C*
     C***** end add *** 11/04/15 *******************************************
     C                   MOVEL(P)  U1POTX        EDPOTX
     C                   MOVEL(P)  U1RNAM        EDRLSN
     C                   Z-ADD     U1RFEI        EDRFEI
     C*
     C                   MOVE      U1SFEI        EDRFEI
     C                   MOVE      U1SFNM        EDRFSN
     C                   MOVE      U1SLNM        EDRLSN
     C                   MOVE      U1SAD1        EDRAD1
     C                   MOVE      U1SAD2        EDRAD2
     C                   MOVE      U1SCTY        EDRCTY
     C                   MOVE      U1SST         EDRST
     C                   MOVE      U1SZIP        EDRZIP
      /Free
          EDtnpi = U1tnpi;
      /End-Free
     C*
     C                   WRITE     PED01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEHP - Write FAEHP record
     C*================================================================
     C     WRTEHP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOBKY1        CHAIN     PEH01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEH01
     C                   Z-ADD     EOCO#         EHCO#
     C                   Z-ADD     EOSUB#        EHSUB#
     C                   Z-ADD     EOFND         EHFND
     C                   Z-ADD     EOFYR         EHFYR
     C                   Z-ADD     EOCASE        EHCASE
     C                   Z-ADD     EOEOB#        EHEOB#
     C                   MOVEL(P)  EOSS#         EHSS#
     C                   Z-ADD     EOACDT        EHACDT
     C                   MOVEL     U1PCON        EHACCT
     C*
     C***** Beg Del *** 11/04/15 *******************************************
     C*****************  MOVEL(P)  EODIA1        EHDIA1
     C*****************  MOVEL(P)  EODIA2        EHDIA2
     C*****************  MOVEL(P)  EODIA3        EHDIA3
     C*****************  MOVEL(P)  EODIA4        EHDIA4
     C***** End Del *** 11/04/15 *******************************************
     C***** beg add *** 11/04/15 *******************************************
     C*
     C***** Beg Chg *** 02/27/16 *******************************************
     C*****************  Eval      EHICD1001 = EODIA1
     C*****************  Eval      EHICD1002 = EODIA2
     C*****************  Eval      EHICD1003 = EODIA3
     C*****************  Eval      EHICD1004 = EODIA4
     C                   Eval      EHICD1001 = U1ICD1
     C                   Eval      EHICD1002 = U1ICD2
     C                   Eval      EHICD1003 = U1ICD3
     C                   Eval      EHICD1004 = U1ICD4
     C                   Eval      EHICDIND = 'Y'
     C***** End Chg *** 02/27/16 *******************************************
     C*
     C***** end add *** 11/04/15 *******************************************
     C                   MOVEL(P)  U1LIC#        EHLIC#
     C*
     C                   MOVEL     EOTAPE        EHEDI
     C*
     C                   MOVEL(P)  'Y'           EHEMPL
     C                   MOVEL(P)  'A'           EHSTAT
     C                   MOVEL(P)  U1PPLN        EHPPLN
     C                   MOVEL(P)  U1ACCT        EHACCT
     C                   MOVEL(P)  U1RLIC        EHFLIC
     C     U1BLIC        IFNE      *BLANKS
     C                   MOVEL(P)  U1BLIC        EHFLIC
     C                   ENDIF
     C                   MOVEL(P)  U1RFLC        EHRFLC
     C                   MOVEL(P)  U1ATLC        EHATLC
     C                   MOVEL(P)  U1RFNP        EHRNPI
     C*
     C                   MOVEL(P)  EOUSER        EHUSER
     C                   MOVEL(P)  EOPGM         EHPGM
     C                   Z-ADD     EOTIME        EHTIME
     C                   Z-ADD     EODT          EHDT
     C                   MOVEL(P)  EOUSER        EHIUSR
     C                   MOVEL(P)  EOPGM         EHIPGM
     C                   Z-ADD     EOITIM        EHITIM
     C                   Z-ADD     EOIDT         EHIDT
     C                   MOVEL(P)  U1RNAM        EHRLSN
     C                   Z-ADD     U1RFEI        EHRFEI
     C                   MOVE      U1SFEI        EHRFEI
     C                   MOVE      U1SFNM        EHRFSN
     C                   MOVE      U1SLNM        EHRLSN
     C                   MOVE      U1SAD1        EHRAD1
     C                   MOVE      U1SAD2        EHRAD2
     C                   MOVE      U1SCTY        EHRCTY
     C                   MOVE      U1SST         EHRST
     C                   MOVE      U1SZIP        EHRZIP
     C*
     C                   WRITE     PEH01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEPP - Write FAEPP record
     C*================================================================
     C     WRTEPP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOBKY1        CHAIN     PEP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEP01
     C                   Z-ADD     EOCO#         EPCO#
     C                   Z-ADD     EOSUB#        EPSUB#
     C                   Z-ADD     EOFND         EPFND
     C                   Z-ADD     EOFYR         EPFYR
     C                   Z-ADD     EOCASE        EPCASE
     C                   Z-ADD     EOEOB#        EPEOB#
     C                   MOVEL(P)  EOSS#         EPSS#
     C                   Z-ADD     EOACDT        EPACDT
     C                   MOVEL     EOTAPE        EPEDI
     C                   MOVEL     U1PCON        EPACCT
     C                   MOVEL     U1PHAR        EPPHAR
     C                   MOVEL(P)  U1RLIC        EPPLIC
     C                   MOVEL(P)  U1ACCT        EPACCT
     C                   MOVEL(P)  U1RLIC        EPFLIC
     C                   MOVEL(P)  U1PDEA        EPPDEA
     C                   MOVEL(P)  U1BTYC        EPBTCD
     C                   MOVEL(P)  U1RFNP        EPRFNP
     C*
     C                   MOVEL(P)  EOUSER        EPUSER
     C                   MOVEL(P)  EOPGM         EPPGM
     C                   Z-ADD     EOTIME        EPTIME
     C                   Z-ADD     EODT          EPDT
     C                   MOVEL(P)  EOUSER        EPIUSR
     C                   MOVEL(P)  EOPGM         EPIPGM
     C                   Z-ADD     EOITIM        EPITIM
     C                   Z-ADD     EOIDT         EPIDT
      /Free
          EPrfnp = U1pnpi;
          EPrnln = U1rnam;
          EPrnfn = U1sfnm;
      /End-Free
     C*
     C                   WRITE     PEP01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEUP - Write FAEUP record
     C*================================================================
     C     WRTEUP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOBKY1        CHAIN     PEU01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEU01
     C                   Z-ADD     EOCO#         EUCO#
     C                   Z-ADD     EOSUB#        EUSUB#
     C                   Z-ADD     EOFND         EUFND
     C                   Z-ADD     EOFYR         EUFYR
     C                   Z-ADD     EOCASE        EUCASE
     C                   MOVEL     EOSS#         EUSS#
     C                   Z-ADD     EOACDT        EUACDT
     C                   Z-ADD     EOEOB#        EUEOB#
     C                   MOVE      '0'           EUPCM
     C     U1MCAR        IFEQ      XXMCAR
     C                   MOVEL     *BLANKS       EUMCAR
     C                   ELSE
     C                   MOVEL(P)  U1MCAR        EUMCAR
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     U1CFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUSCPF
     C*
     C                   Z-ADD     U1CTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUSCPT
     C*
     C                   MOVEL(P)  U1PCON        EUACCT
     C*
     C                   MOVEL(P)  U1BLTY        EUBLTY
     C*
     C                   Z-ADD     U1ADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUADDT
     C*
     C***** Beg Del *** 11/04/15 *******************************************
     C**************     MOVEL(P)  EODIA1        EUDIA1
     C**************     MOVEL(P)  EODIA2        EUDIA2
     C**************     MOVEL(P)  EODIA3        EUDIA3
     C**************     MOVEL(P)  EODIA4        EUDIA4
     C***** End Del *** 11/04/15 *******************************************
     C***** beg add *** 11/04/15 *******************************************
     C*
     C***** Beg Chg *** 02/27/16 *******************************************
     C**************     Eval      EUICD1001 = EODIA1
     C**************     Eval      EUICD1002 = EODIA2
     C**************     Eval      EUICD1003 = EODIA3
     C**************     Eval      EUICD1004 = EODIA4
     C                   Eval      EUICD1001 = U1ICD1
     C                   Eval      EUICD1002 = U1ICD2
     C                   Eval      EUICD1003 = U1ICD3
     C                   Eval      EUICD1004 = U1ICD4
     C***** End Chg *** 02/27/16 *******************************************
     C                   Eval      EUICD1005 = U1ICD5
     C                   Eval      EUICD1006 = U1ICD6
     C                   Eval      EUICD1007 = U1ICD7
     C                   Eval      EUICD1008 = U1ICD8
     C*
     C                   Eval      EUPRC1001 = U1PRC1
     C                   Eval      EUPRC1002 = U1PRC2
     C                   Eval      EUPRC1003 = U1PRC3
     C                   Eval      EUPRC1004 = U1PRC4
     C                   Eval      EUPRC1005 = U1PRC5
     C*
     C                   Eval      EUADIA1001 = U1ADDG
     C*
     C                   Eval      EUCAU1001 = U1CAU1
     C                   Eval      EUCAU1002 = U1CAU2
     C                   Eval      EUCAU1003 = U1CAU3
     C*
     C                   Eval      EUICDIND = 'Y'
     C*
     C***** end add *** 11/04/15 *******************************************
     C                   MOVEL     U1LIC#        EULIC#
     C*
     C                   MOVEL     EOTAPE        EUEDI
     C                   MOVEL(P)  U1OLIC        EUOLIC
     C***** Beg Del *** 11/04/15 *******************************************
     C*****************  MOVEL(P)  U1ICD5        EUDIA5
     C*****************  MOVEL(P)  U1ICD6        EUDIA6
     C*****************  MOVEL(P)  U1ICD7        EUDIA7
     C*****************  MOVEL(P)  U1ICD8        EUDIA8
     C*****************  MOVEL(P)  U1PRC3        EUPRC3
     C*****************  MOVEL(P)  U1PRC4        EUPRC4
     C*****************  MOVEL(P)  U1PRC5        EUPRC5
     C*****************  MOVEL(P)  U1CAU1        EUCAU1
     C*****************  MOVEL(P)  U1CAU2        EUCAU2
     C*****************  MOVEL(P)  U1CAU3        EUCAU3
     C***** End Del *** 11/04/15 *******************************************
     C                   MOVEL(P)  U1SCHD        EUSCHD
     C                   MOVEL(P)  U1SVTY        EUICN
     C                   Z-ADD     U1ADHR        EUADHR
     C*
     C                   Z-ADD     U1DCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUDCDT
     C*
     C                   Z-ADD     U1DCHR        EUDCHR
     C                   MOVEL(P)  U1ADTY        EUADTY
     C                   MOVEL(P)  U1TSS         EUDCN
     C                   MOVEL(P)  U1PCM         EUPCM
     C***** Beg Del *** 11/04/15 *******************************************
     C****************   MOVEL(P)  U1PRC1        EUPRC1
     C****************   MOVEL(P)  U1PRC2        EUPRC2
     C***** End Del *** 11/04/15 *******************************************
     C                   MOVEL(P)  U1PPLN        EUPPLN
     C                   MOVEL(P)  U1ACCT        EUACCT
     C                   MOVE(P)   U1DRG#        EUDRG#
     C*
     C                   Z-ADD     U1PPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUPRD1
     C*
     C***** Beg Del *** 11/04/15 *******************************************
     C****************   MOVEL(P)  U1ADDG        EUADDI
     C***** End Del *** 11/04/15 *******************************************
     C                   MOVEL(P)  U1RLIC        EUFLIC
     C                   MOVEL(P)  U1ATLC        EUATLC
     C                   MOVEL(P)  U1RLIC        EURLIC
     C                   MOVEL(P)  U1ANPI        EUANPI
     C*
     C                   Z-ADD     U1PCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUPRD2
     C*
     C                   MOVEL     'A'           EUSTAT
     C                   MOVEL(P)  EOUSER        EUUSER
     C                   MOVEL(P)  EOPGM         EUPGM
     C                   Z-ADD     EOTIME        EUTIME
     C                   Z-ADD     EODT          EUDT
     C                   MOVEL(P)  EOUSER        EUIUSR
     C                   MOVEL(P)  EOPGM         EUIPGM
     C                   Z-ADD     EOITIM        EUITIM
     C                   Z-ADD     EOIDT         EUIDT
     C                   MOVEL(P)  U1RNAM        EURLSN
     C                   Z-ADD     U1RFEI        EURFEI
     C*
     C                   MOVE      U1ADSC        EUADSC
     C                   MOVE      U1DCST        EUDCST
     C                   MOVE      U1PREA        EUPREA
     C                   MOVE      U1IMST        EUIMST
     C*
     C                   MOVE      U1SFEI        EURFEI
     C                   MOVE      U1SFNM        EURFSN
     C                   MOVE      U1SLNM        EURLSN
     C                   MOVE      U1SAD1        EURAD1
     C                   MOVE      U1SAD2        EURAD2
     C                   MOVE      U1SCTY        EURCTY
     C                   MOVE      U1SST         EURST
     C                   MOVE      U1SZIP        EURZIP
     C*
     C                   WRITE     PEU01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTCAP - Write FDCAP record
     C*================================================================
     C     WRTCAP        BEGSR
     C*
     C     U2EOB#        IFEQ      *ZEROS
     C     U2DSEQ        ANDEQ     *ZEROS
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C     CAKY          SETGT     PCA02
     C     CAKY2         READPE    PCA02                                  99
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   CLEAR                   PCA01
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     EOCSCD        CACSCD
     C*
     C                   Z-ADD     U2SVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         CAFDAT
     C*
     C                   Z-ADD     CAFDAT        CATDAT
     C                   Z-ADD     EOTRDT        CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL     '1'           CAPRTY
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL     '4'           CAPRTY
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL     'D'           CAPRTY
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL     'X'           CAPRTY
     C                   ENDSL
     C*
     C     U2UNTS        IFNE      *ZEROS
     C                   Z-ADD     U2UNTS        CAUNTS
     C                   ELSE
     C                   Z-ADD     1             CAUNTS
     C                   ENDIF
     C*
     C                   Z-ADD     XXDSEQ        CADSEQ
     C     EOBLST        IFEQ      'P'
     C                   Z-ADD     0             CACHK#
     C                   ELSE
     C                   Z-ADD     EOCHK#        CACHK#
     C                   ENDIF
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  'C'           CARCFL
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL(P)  'S'           CARCFL
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  'C'           CARCFL
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL(P)  'X'           CARCFL
     C                   ENDSL
     C*
     C                   MOVEL(P)  EOUSER        CAUSER
     C                   MOVEL(P)  EOPGM         CAPGM
     C                   Z-ADD     EODT          CADT
     C                   Z-ADD     EOTIME        CATIME
     C                   MOVEL(P)  EOUSER        CAIUSR
     C                   MOVEL(P)  EOPGM         CAIPGM
     C                   Z-ADD     EOIDT         CAIDT
     C                   Z-ADD     EOITIM        CAITIM
     C*
     C                   Z-ADD     EOSUB#        CASUB#
     C                   MOVE      EOZIPA        CAZIPA
     C                   MOVEL(P)  EOAST         CAAST
     C*
     C                   MOVE      U2CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CACHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        CACHRG
     C                   ENDIF
     C*
     C                   Z-ADD     U2LIN#        CALIN#
     C*** Beg Add *** 10/04/13 *********************************************
     C                   Z-ADD     U2LIN#        CAMSEQ
     C*** End Add *** 10/04/13 *********************************************
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  U2CODE        CACODE
     C                   MOVEL(P)  U2CODE        CACPT4
     C                   MOVEL(P)  U2CODE        CABPRC
     C*
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  U2CODE        CACODE
     C                   MOVEL(P)  U2CODE        CACPT4
     C                   MOVEL(P)  U2CODE        CABPRC
     C*
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL(P)  U2NDC         CACODE
     C*
     C     EOBTYP        WHENEQ    'H'
     C                   RESET                   KKCMSV
     C     DIGITS        CHECK     U2RVCD        N                 3 0    99
     C     *IN99         IFEQ      *ON
     C     U2CODE        IFEQ      *BLANKS
     C                   MOVEL     U2RVCD        KKHCPC
     C                   ENDIF
     C                   ELSE
     C                   MOVE      U2RVCD        KKRVCD
     C                   MOVEL(P)  U2CODE        KKHCPC
     C                   ENDIF
     C                   MOVE      KKRVCD        CASVCD
     C                   MOVEL     KKCMSV        CACODE
     C                   ENDSL
     C*
     C                   WRITE     PCA01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDDP - Write FDDDP record
     C*================================================================
     C     WRTDDP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDD01
     C                   Z-ADD     CACO#         DDCO#
     C                   Z-ADD     CASUB#        DDSUB#
     C                   Z-ADD     CAFND         DDFND
     C                   Z-ADD     CAFYR         DDFYR
     C                   Z-ADD     CACASE        DDCASE
     C                   MOVEL     CASS#         DDSS#
     C                   Z-ADD     CAACDT        DDACDT
     C                   Z-ADD     CAEOB#        DDEOB#
     C                   Z-ADD     CADSEQ        DDDSEQ
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DDUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DDUNTS
     C                   ENDIF
     C*
     C                   MOVEL     U2CODE        DDPRCD
     C                   MOVEL     U2CDSC        DDPRDS
     C                   MOVEL     U2CODE        DDBPRC
     C                   MOVEL     U2CDSC        DDBPRD
     C                   MOVEL     U2POS         DDPOS
     C*
     C     DDPOS         IFEQ      *BLANKS
     C     DDPOS         OREQ      '00'
     C                   MOVEL     '11 '         DDPOS
     C                   ENDIF
     C*
     C                   MOVE      'C'           DDRCFL
     C*
     C                   MOVE      U2DXR         XXDXR
     C*
     C     DX1           IFEQ      1
     C     DX2           OREQ      1
     C     DX3           OREQ      1
     C     DX4           OREQ      1
     C                   MOVEL     'X'           DDDXR1
     C                   ENDIF
     C*
     C     DX1           IFEQ      2
     C     DX2           OREQ      2
     C     DX3           OREQ      2
     C     DX4           OREQ      2
     C                   MOVEL     'X'           DDDXR2
     C                   ENDIF
     C*
     C     DX1           IFEQ      3
     C     DX2           OREQ      3
     C     DX3           OREQ      3
     C     DX4           OREQ      3
     C                   MOVEL     'X'           DDDXR3
     C                   ENDIF
     C*
     C     DX1           IFEQ      4
     C     DX2           OREQ      4
     C     DX3           OREQ      4
     C     DX4           OREQ      4
     C                   MOVEL     'X'           DDDXR4
     C                   ENDIF
     C*
     C                   SELECT
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA1        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR1
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA2        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR2
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA3        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR3
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA4        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR4
     C                   ENDSL
     C                   MOVEL(P)  U2TOO#        DDPART
     C*** Beg Add *** 10/04/13 *********************************************
     C                   if        u2bprc <> *blanks
     C*** End Add *** 10/04/13 *********************************************
     C                   MOVEL(P)  U2BPRC        DDBPRC
     C*** Beg Add *** 10/04/13 *********************************************
     C                   endif
     C*** End Add *** 10/04/13 *********************************************
     C*** Beg Chg *** 02/28/14 *********************************************
     C********           MOVEL(P)  U2BMD1        DDMOD1
     C                   MOVEL(P)  U2MOD         DDMOD1
     C*** End Chg *** 02/28/14 *********************************************
     C*
     C                   MOVEL     'A'           DDSTAT
     C                   MOVEL(P)  CAUSER        DDUSER
     C                   MOVEL(P)  CAPGM         DDPGM
     C                   Z-ADD     CATIME        DDTIME
     C                   Z-ADD     CADT          DDDT
     C                   MOVEL(P)  CAUSER        DDIUSR
     C                   MOVEL(P)  CAPGM         DDIPGM
     C                   Z-ADD     CAITIM        DDITIM
     C                   Z-ADD     CAIDT         DDIDT
     C*
     C                   WRITE     PDD01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDHP - Write FDDHP record
     C*================================================================
     C     WRTDHP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     CAPKY1        CHAIN     PDH01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDH01
     C                   Z-ADD     CACO#         DHCO#
     C                   Z-ADD     CASUB#        DHSUB#
     C                   Z-ADD     CAFND         DHFND
     C                   Z-ADD     CAFYR         DHFYR
     C                   Z-ADD     CACASE        DHCASE
     C                   MOVEL     CASS#         DHSS#
     C                   Z-ADD     CAACDT        DHACDT
     C                   Z-ADD     CAEOB#        DHEOB#
     C                   Z-ADD     CADSEQ        DHDSEQ
     C*
     C                   MOVEL     U2CODE        DHPRCD
     C                   MOVEL     U2CDSC        DHPRDS
     C                   MOVEL     U2CODE        DHBPRC
     C                   MOVEL     U2CDSC        DHBPRD
     C                   MOVE      U2POS         DHPOS
     C*
     C     DHPOS         IFEQ      *BLANKS
     C     DHPOS         OREQ      '00'
     C                   MOVEL     ' 11'         DHPOS
     C                   ENDIF
     C*
     C                   MOVEL     U2MOD         DHMOD1
     C                   MOVE      'C'           DHRCFL
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DHUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DHUNTS
     C                   ENDIF
     C*
     C                   MOVE      U2DXR         XXDXR
     C*
     C     DX1           IFEQ      1
     C     DX2           OREQ      1
     C     DX3           OREQ      1
     C     DX4           OREQ      1
     C                   MOVEL     'X'           DHDXR1
     C                   ENDIF
     C*
     C     DX1           IFEQ      2
     C     DX2           OREQ      2
     C     DX3           OREQ      2
     C     DX4           OREQ      2
     C                   MOVEL     'X'           DHDXR2
     C                   ENDIF
     C*
     C     DX1           IFEQ      3
     C     DX2           OREQ      3
     C     DX3           OREQ      3
     C     DX4           OREQ      3
     C                   MOVEL     'X'           DHDXR3
     C                   ENDIF
     C*
     C     DX1           IFEQ      4
     C     DX2           OREQ      4
     C     DX3           OREQ      4
     C     DX4           OREQ      4
     C                   MOVEL     'X'           DHDXR4
     C                   ENDIF
     C*
     C                   SELECT
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     EHDIA1        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR1
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     EHDIA2        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR2
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     EHDIA3        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR3
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     EHDIA4        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR4
     C                   ENDSL
     C                   MOVEL(P)  U2MOD2        DHMOD2
     C                   MOVEL(P)  U2MOD3        DHMOD3
     C                   MOVEL(P)  U2MOD4        DHMOD4
     C                   MOVEL(P)  U2BLFC        DHBLFC
     C                   MOVEL(P)  U2DUCD        DHDUCD
     C*** Beg Add *** 10/04/13 *********************************************
     C                   if        u2bprc <> *blanks
     C*** End Add *** 10/04/13 *********************************************
     C                   MOVEL(P)  U2BPRC        DHBPRC
     C*** Beg Add *** 10/04/13 *********************************************
     C                   endif
     C*** End Add *** 10/04/13 *********************************************
     C*** Beg Chg *** 02/28/14 *********************************************
     C********           MOVEL(P)  U2BMD1        DHMOD1
     C                   MOVEL(P)  U2MOD         DHMOD1
     C*** End Chg *** 02/28/14 *********************************************
     C*
     C                   MOVEL     'A'           DHSTAT
     C                   MOVEL(P)  CAUSER        DHUSER
     C                   MOVEL(P)  CAPGM         DHPGM
     C                   Z-ADD     CATIME        DHTIME
     C                   Z-ADD     CADT          DHDT
     C                   MOVEL(P)  CAUSER        DHIUSR
     C                   MOVEL(P)  CAPGM         DHIPGM
     C                   Z-ADD     CAITIM        DHITIM
     C                   Z-ADD     CAIDT         DHIDT
      /Free
          DHnpir = U2rnpi;
          DHrnlc = U2rnlc;
      /End-Free
     C*
     C                   WRITE     PDH01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDPP - Write FDDPP record
     C*================================================================
     C     WRTDPP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDP01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDP01
     C                   Z-ADD     CACO#         DPCO#
     C                   Z-ADD     CASUB#        DPSUB#
     C                   Z-ADD     CAFND         DPFND
     C                   Z-ADD     CAFYR         DPFYR
     C                   Z-ADD     CACASE        DPCASE
     C                   MOVEL     CASS#         DPSS#
     C                   Z-ADD     CAACDT        DPACDT
     C                   Z-ADD     CAEOB#        DPEOB#
     C                   Z-ADD     CADSEQ        DPDSEQ
     C                   MOVEL(P)  U2NDC         DPNDC
     C                   MOVEL(P)  U2RX#         DPRX#
     C                   MOVEL(P)  DPNDC         DPBNDC
     C                   Z-ADD     U2UNTS        XXQTY             9 0
     C                   Z-ADD     XXQTY         DPRXMQ
     C                   MOVEL(P)  U2CDSC        DPDESC
     C*
     C                   Z-ADD     U2SVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         DPRXFD
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DPUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DPUNTS
     C                   ENDIF
     C                   MOVEL(P)  U2RXPT        DPRXPT
     C*
     C     EOBTYP        IFEQ      'X'
     C     DPRXPT        ANDEQ     'S'
     C                   MOVEL(P)  U2PRIN        DPPRIN
     C                   MOVEL(P)  U2CODE        DPHCPC
     C                   ENDIF
     C*
     C                   MOVEL(P)  U2NWRF        DPNWRF
     C                   MOVEL(P)  U2CERT        DPCERT
     C                   MOVEL(P)  U2LIC#        DPLIC#
     C                   MOVEL(P)  U2NDCD        DPDESC
     C                   MOVEL(P)  U2RXDY        DPRXDY
     C*
     C                   MOVE      U2DFEE        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DPDFEE
     C                   ELSE
     C                   Z-ADD     AMOUNT        DPDFEE
     C                   ENDIF
     C*
     C                   MOVEL(P)  U2RXDI        DPRXDI
     C                   MOVEL(P)  U2BCDT        DPBCDT
     C                   MOVEL(P)  U2BNDC        DPBNDC
     C*
     C                   MOVEL     'A'           DPSTAT
     C                   MOVEL(P)  CAUSER        DPUSER
     C                   MOVEL(P)  CAPGM         DPPGM
     C                   Z-ADD     CATIME        DPTIME
     C                   Z-ADD     CADT          DPDT
     C                   MOVEL(P)  CAUSER        DPIUSR
     C                   MOVEL(P)  CAPGM         DPIPGM
     C                   Z-ADD     CAITIM        DPITIM
     C                   Z-ADD     CAIDT         DPIDT
     C*
     C                   WRITE     PDP01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDTL - Write Detail Records
     C*================================================================
     C     WRTDTL        BEGSR
     C*
     C* Initialize XXTAX
     C*
     C                   MOVE      *ZEROS        XXTAX
     C*
     C     U2KY1         CHAIN     PU201                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      CHKAM2
     C                   EXSR      WRTCAP                                       FDCAP
     C                   EXSR      UPDT2A
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      WRTDDP                                       FADDP
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      WRTDUP                                       FADUP
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      WRTDHP                                       FADHP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      WRTDPP                                       FADHP
     C                   ENDSL
     C*
     C                   EXSR      UPDT2B
     C*
     C     U2KY1         READE     PU201                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDUP - Write FDDUP record
     C*================================================================
     C     WRTDUP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDU01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDU01
     C                   Z-ADD     CACO#         DUCO#
     C                   Z-ADD     CASUB#        DUSUB#
     C                   Z-ADD     CAFND         DUFND
     C                   Z-ADD     CAFYR         DUFYR
     C                   Z-ADD     CACASE        DUCASE
     C                   MOVEL     CASS#         DUSS#
     C                   Z-ADD     CAACDT        DUACDT
     C                   Z-ADD     CAEOB#        DUEOB#
     C                   Z-ADD     CADSEQ        DUDSEQ
     C*
     C                   MOVEL     KKRVCD        DURVCD
     C                   MOVEL     KKHCPC        DURATE
     C*
     C                   MOVE      U2POS         DUPOS
     C*
     C     DUPOS         IFEQ      *BLANKS
     C     DUPOS         OREQ      '00'
     C                   MOVEL     ' 21'         DUPOS
     C                   ENDIF
     C*
     C                   Z-ADD     U2UNTS        DUUNTS
     C                   MOVE      DURVCD        DUBPRC
     C                   MOVE      DURATE        DUBRAT
     C                   MOVEL(P)  U2CDSC        DUDESC
     C                   MOVEL(P)  DUDESC        DUBPRD
     C                   MOVEL(P)  U2DUCD        DUDUCD
     C*** Beg Add *** 10/04/13 *********************************************
     C                   if        u2bprc <> *blanks
     C*** End Add *** 10/04/13 *********************************************
     C                   MOVEL(P)  U2BPRC        DUBRAT
     C*** Beg Add *** 10/04/13 *********************************************
     C                   endif
     C*** End Add *** 10/04/13 *********************************************
     C*** Beg Chg *** 02/28/14 *********************************************
     C********           MOVEL(P)  U2BMD1        DUPMOD
     C                   MOVEL(P)  U2MOD         DUPMOD
     C*** End Chg *** 02/28/14 *********************************************
     C*
     C                   MOVEL(P)  U2MOD         DUPMOD
     C                   MOVEL(P)  U2MOD2        DUMOD2
     C                   MOVEL(P)  U2PSCD        DUPSCD
     C                   MOVEL(P)  U2CDSC        DUSCDS
     C*
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL(P)  CAUSER        DUUSER
     C                   MOVEL(P)  CAPGM         DUPGM
     C                   Z-ADD     CATIME        DUTIME
     C                   Z-ADD     CADT          DUDT
     C                   MOVEL(P)  CAUSER        DUIUSR
     C                   MOVEL(P)  CAPGM         DUIPGM
     C                   Z-ADD     CAITIM        DUITIM
     C                   Z-ADD     CAIDT         DUIDT
     C*
     C                   WRITE     PDU01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKAM1 - Check amounts in upload file and convert to zero if blanks
     C*================================================================
     C     CHKAM1        BEGSR
     C*
     C     U1CHRG        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U1CHRG
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKAM2 - Check amounts in upload file and convert to zero if blanks
     C*================================================================
     C     CHKAM2        BEGSR
     C*
     C     U2CHRG        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2CHRG
     C                   ENDIF
     C*
     C                   ENDSR
