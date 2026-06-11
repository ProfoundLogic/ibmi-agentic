     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FA736R - API - Update EOMB with New Claim #
     H*****************************************************************
     H* Description: Transfers a single EOB to a new claim
     H*
     H*****************************************************************
     H* Created on 10/10/2002 by c:62 for Project WC1078
     H*****************************************************************
     H* Revisions:
     H*
     H*****************************************************************
     FFMCML1    IF   E           K DISK
     FFAEOL4    UF   E           K DISK
     FFALTL7    UF   E           K DISK
     FFAEAL1    UF   E           K DISK
     FFAEDL     UF   E           K DISK
     FFAEEL     UF   E           K DISK
     FFAEHL     UF   E           K DISK
     FFAEPL     UF   E           K DISK
     FFAESL     UF   E           K DISK
     FFAEUL     UF   E           K DISK
     FFDCALR    UF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDDAL2    UF   E           K DISK
     FFDDDL     UF   E           K DISK
     FFDDEL     UF   E           K DISK
     FFDDHL     UF   E           K DISK
     FFDDPL     UF   E           K DISK
     FFDDUL     UF   E           K DISK
     FFDMGL1    UF   E           K DISK
     FFDCHL1    UF   E           K DISK
     FNDTML1    UF   E           K DISK
     FNMARLA    UF   E           K DISK
     FNMARL5    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FNMIML     UF A E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@WCF#            3 0
     C                   PARM                    @@FFYR            3 0
     C                   PARM                    @@TFYR            3 0
     C                   PARM                    @@FCAS            7 0
     C                   PARM                    @@TCAS            7 0
     C                   PARM                    @@EOB#            9 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ARKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFFYR
     C                   KFLD                    KKFCAS
     C                   KFLD                    KKINV#
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKTFYR
     C                   KFLD                    KKTCAS
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKTFYR
     C                   KFLD                    KKTCAS
     C*
     C     CAPKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFFYR
     C                   KFLD                    KKFCAS
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKTFYR
     C                   KFLD                    KKTCAS
     C*
     C     EOBKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFFYR
     C                   KFLD                    KKFCAS
     C                   KFLD                    KKEOB#
     C*
     C     IMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKINV#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFFYR
     C                   KFLD                    KKFCAS
     C*
     C     IMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKINV#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKTFYR
     C                   KFLD                    KKTCAS
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFFYR
     C     *LIKE         DEFINE    EOCASE        KKFCAS
     C     *LIKE         DEFINE    EOFYR         KKTFYR
     C     *LIKE         DEFINE    EOCASE        KKTCAS
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CADSEQ        HDDSEQ
     C     *LIKE         DEFINE    ARINV#        KKINV#
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@WCF#        ##WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##WCF#            3
     C                   PARM      *BLANKS       ##SUB#            5
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   MOVE      ##SUB#        KKSUB#
     C                   MOVE      ##FND         KKFND
     C                   Z-ADD     @@FFYR        KKFFYR
     C                   Z-ADD     @@FCAS        KKFCAS
     C                   Z-ADD     @@TFYR        KKTFYR
     C                   Z-ADD     @@TCAS        KKTCAS
     C                   Z-ADD     @@EOB#        KKEOB#
     C*
     C* Let's get "To claim" info
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      TRNEOB
     C                   ENDIF
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*================================================================
     C* TRNEOB - Transfer EOB
     C*================================================================
     C     TRNEOB        BEGSR
     C*
     C* File FAEOP
     C*
     C     EOBKY         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     @@TFYR        EOFYR
     C                   Z-ADD     @@TCAS        EOCASE
     C                   MOVEL(P)  CMSS#         EOSS#
     C                   Z-ADD     CMACDT        EOACDT
     C                   MOVEL(P)  CMLSN         EOLSN
     C                   MOVEL(P)  CMFSN         EOFSN
     C                   MOVEL(P)  CMMID         EOMID
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     Q1PGM         EOPGM
     C                   MOVEL     DSTIME        EOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C* FILE FAEAP
     C*
     C     EOBKY         CHAIN     PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL(P)  EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
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
     C                   UPDATE    PEA01
     C*
     C     EOBKY         READE     PEA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE FALTP
     C*
     C     EOBKY         CHAIN     PLT01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         LTFYR
     C                   Z-ADD     EOCASE        LTCASE
     C                   MOVEL(P)  EOSS#         LTSS#
     C                   Z-ADD     EOACDT        LTACDT
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
     C     EOBKY         READE     PLT01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE NMARP
     C*
     C     EOBKY         CHAIN     PAR01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         ARFYR
     C                   Z-ADD     EOCASE        ARCASE
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
     C                   Z-ADD     ARINV#        KKINV#
     C*
     C* NMIMP FILE
     C*
     C     IMKY1         CHAIN(N)  PIM01                              99
     C     *IN99         IFEQ      *OFF
     C     IMKY2         CHAIN(N)  PIM01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     EOFYR         IMFYR
     C                   Z-ADD     EOCASE        IMCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        IMUSER
     C                   MOVEL     Q1PGM         IMPGM
     C                   MOVEL     DSTIME        IMTIME
     C                   MOVEL     Q1USER        IMIUSR
     C                   MOVEL     Q1PGM         IMIPGM
     C                   MOVEL     DSTIME        IMITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IMDT
     C                   MOVEL     SRDTO         IMIDT
     C*
     C                   WRITE     PIM01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PAR01
     C*
     C     ARKY          CHAIN     PAR02                              99
     C     *IN99         IFEQ      *ON
     C     IMKY1         CHAIN     PIM01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PIM01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOBKY         READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE NDTMP
     C*
     C     EOBKY         CHAIN     PTM01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         TMFYR
     C                   Z-ADD     EOCASE        TMCASE
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
     C                   UPDATE    PTM01
     C*
     C     EOBKY         READE     PTM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE FAEDP
     C*
     C     EOBKY         CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         EDFYR
     C                   Z-ADD     EOCASE        EDCASE
     C                   MOVEL(P)  EOSS#         EDSS#
     C                   Z-ADD     EOACDT        EDACDT
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
     C                   UPDATE    PED01
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C* FILE FAEEP
     C*
     C     EOBKY         CHAIN     PEE01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         EEFYR
     C                   Z-ADD     EOCASE        EECASE
     C                   MOVEL(P)  EOSS#         EESS#
     C                   Z-ADD     EOACDT        EEACDT
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
     C                   UPDATE    PEE01
     C*
     C     EOBKY         READE     PEE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE FAEHP
     C*
     C     EOBKY         CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         EHFYR
     C                   Z-ADD     EOCASE        EHCASE
     C                   MOVEL(P)  EOSS#         EHSS#
     C                   Z-ADD     EOACDT        EHACDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EHUSER
     C                   MOVEL     Q1PGM         EHPGM
     C                   MOVEL     DSTIME        EHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C*
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C* FILE FAEPP
     C*
     C     EOBKY         CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         EPFYR
     C                   Z-ADD     EOCASE        EPCASE
     C                   MOVEL(P)  EOSS#         EPSS#
     C                   Z-ADD     EOACDT        EPACDT
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
     C                   UPDATE    PEP01
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C* FILE FAESP
     C*
     C     EOBKY         CHAIN     PES01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         ESFYR
     C                   Z-ADD     EOCASE        ESCASE
     C                   MOVEL(P)  EOSS#         ESSS#
     C                   Z-ADD     EOACDT        ESACDT
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
     C                   UPDATE    PES01
     C*
     C     EOBKY         READE     PES01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE FAEUP
     C*
     C     EOBKY         CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         EUFYR
     C                   Z-ADD     EOCASE        EUCASE
     C                   MOVEL(P)  EOSS#         EUSS#
     C                   Z-ADD     EOACDT        EUACDT
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
     C                   UPDATE    PEU01
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C* FILE FDCAP
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C     CAKY1         SETGT     PCA02
     C     CAKY2         READPE    PCA02                                  99
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             HDDSEQ
     C                   ELSE
     C                   Z-ADD     1             HDDSEQ
     C                   ENDIF
     C*
     C     EOBKY         CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL(P)  EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   Z-ADD     HDDSEQ        CADSEQ
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
     C* FILE FDDAP
     C*
     C     CAPKY         CHAIN     PDA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         DAFYR
     C                   Z-ADD     EOCASE        DACASE
     C                   MOVEL(P)  EOSS#         DASS#
     C                   Z-ADD     EOACDT        DAACDT
     C                   Z-ADD     CADSEQ        DADSEQ
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
     C     CAPKY         READE     PDA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE FDDDP
     C*
     C     CAPKY         CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         DDFYR
     C                   Z-ADD     EOCASE        DDCASE
     C                   MOVEL(P)  EOSS#         DDSS#
     C                   Z-ADD     EOACDT        DDACDT
     C                   Z-ADD     CADSEQ        DDDSEQ
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
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C* FILE FDDEP
     C*
     C     CAPKY         CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   Z-ADD     CADSEQ        DEDSEQ
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
     C                   UPDATE    PDE01
     C*
     C     CAPKY         READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE FDCHP
     C*
     C     CAPKY         CHAIN     PCH01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         CHFYR
     C                   Z-ADD     EOCASE        CHCASE
     C                   Z-ADD     CADSEQ        CHDSEQ
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
     C     CAPKY         READE     PCH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C* FILE FDDHP
     C*
     C     CAPKY         CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   MOVEL(P)  EOSS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   Z-ADD     CADSEQ        DHDSEQ
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
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C* FILE FDDPP
     C*
     C     CAPKY         CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         DPFYR
     C                   Z-ADD     EOCASE        DPCASE
     C                   MOVEL(P)  EOSS#         DPSS#
     C                   Z-ADD     EOACDT        DPACDT
     C                   Z-ADD     CADSEQ        DPDSEQ
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
     C                   UPDATE    PDP01
     C                   ENDIF
     C*
     C* FILE FDDUP
     C*
     C     CAPKY         CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   MOVEL(P)  EOSS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   Z-ADD     CADSEQ        DUDSEQ
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
     C                   UPDATE    PDU01
     C                   ENDIF
     C*
     C* FILE FDMGP
     C*
     C     CAPKY         CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOFYR         MGFYR
     C                   Z-ADD     EOCASE        MGCASE
     C                   Z-ADD     CADSEQ        MGDSEQ
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
     C                   UPDATE    PMG01
     C*
     C     CAPKY         READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   UPDATE    PCA01                                        FDCAP
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C     CAKY1         SETGT     PCA02
     C     CAKY2         READPE    PCA02                                  99
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             HDDSEQ
     C                   ELSE
     C                   Z-ADD     1             HDDSEQ
     C                   ENDIF
     C*
     C     EOBKY         READE     PCA01                                  99
     C*
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   UPDATE    PEO01                                99      FAEOP
     C                   ENDIF
     C*
     C                   ENDSR
