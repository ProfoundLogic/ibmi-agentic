     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE F1393R - CodeReview Complete/Pend Program
     H*
     H*****************************************************************
     H* Created on 06/06/02 by C:62 for Project 114MDC                *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 06/11/12 C:251 PROJECT 114MER                                 *
     H*                Remove params from call                        *
     H*                                                               *
     H* 01/07/03 C:79 PROJECT 114MDC                                  *
     H*               1. CLEAR CAFLAG WHEN PENDING CODEREVIEW BILLS SO*
     H*                  ADJUDICATION WILL APPLY EDITS(RULES,CCI,DUPS)*
     H*               2. POPULATE CACHK# WHEN PENDING CODEREVIEW      *
     H*                  SO LINES SHOW IN HISTORY                     *
     H*                                                               *
     H* 10/11/02 C:62 PROJECT 114MDC                                  *
     H*               MAKE SURE EVERY BILL IS PENDED                  *
     H*                                                               *
     H*****************************************************************
     FFAEOP     UF   E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     FFMF1L     IF   E           K DISK
     FFDCAL12   UF   E           K DISK
     FFAEAP     O    E             DISK
     FFAEAL1    UF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFAEHL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFDDAL     UF   E           K DISK
     FFDMGL4    IF   E           K DISK
     FFXLOP     UF   E           K DISK
     FWMCIL     IF   E           K DISK
     FFTBSL     IF   E           K DISK
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D/COPY SCOPYMT01I
     D*
     D REASON          C                   CONST('BILL/EOMB COMPLETED')
     I*
     IPEA02
     I              EACO#                       E2CO#
     I              EASUB#                      E2SUB#
     I              EAFND                       E2FND
     I              EAFYR                       E2FYR
     I              EACASE                      E2CASE
     I              EASS#                       E2SS#
     I              EAACDT                      E2ACDT
     I              EAEOB#                      E2EOB#
     I              EARCN                       E2RCN
     I              EATRDT                      E2TRDT
     I              EACHRG                      E2CHRG
     I              EAMRA                       E2MRA
     I              EAOTHR                      E2OTHR
     I              EAOTH2                      E2OTH2
     I              EANDIS                      E2NDIS
     I              EAPAID                      E2PAID
     I              EASTAT                      E2STAT
     I              EAUSER                      E2USER
     I              EAPGM                       E2PGM
     I              EATIME                      E2TIME
     I              EADT                        E2DT
     I              EAIUSR                      E2IUSR
     I              EAIPGM                      E2IPGM
     I              EAITIM                      E2ITIM
     I              EAIDT                       E2IDT
     I              EAEPFL                      E2EPFL
     I              EAESDT                      E2ESDT
     I              EATRCD                      E2TRCD
     I              EACKST                      E2CKST
     I              EACRKB                      E2CRKB
     I              EAPMKB                      E2PMKB
     I              EAMEKB                      E2MEKB
     I              EAFQKB                      E2FQKB
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CHG             1
     C                   PARM                    @@CRPE            1
     C                   PARM                    @@PCOD            3
     C                   PARM                    @@REAS           60
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              PARM                    @@CRKB            2
     C*****              PARM                    @@PMKB            2
     C*****              PARM                    @@FQKB            2
     C*** End Del *** 06/11/12 ******************************************
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     BSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C* Key list for Detail Charge file
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKSYS
     C*
     C* Key list for Case Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* Key list for Bill Detail Amount file
     C*
     C     DAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C* Key list for Group Auxiliary file
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* Key list for Bill Header Amount file
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     EAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Physician Header file
     C*
     C     EHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Pharmacy Header file
     C*
     C     EPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Hospital Header file
     C*
     C     EUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C     LOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* Key list for Bill Detail Amount file
     C*
     C     MGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    EOMRA         DFMRA
     C     *LIKE         DEFINE    EONDIS        DFNDIS
     C     *LIKE         DEFINE    EOOTHR        DFOTHR
     C     *LIKE         DEFINE    EOOTH2        DFOTH2
     C     *LIKE         DEFINE    EOCHRG        DFCHRG
     C     *LIKE         DEFINE    EOPAID        DFPAID
     C     *LIKE         DEFINE    EANAMT        ##NAMT
     C     *LIKE         DEFINE    EAFND         #1WCF#
     C     *LIKE         DEFINE    EOSYS         ##SYS
     C     *LIKE         DEFINE    EOSYS         ##CALL
     C     *LIKE         DEFINE    EOSYS         ##RTCD
     C     *LIKE         DEFINE    EOSYS         ##PRTY
     C     *LIKE         DEFINE    EOSYS         ##OUTS
     C     *LIKE         DEFINE    EOFND         KKWCF#
     C     *LIKE         DEFINE    CIPTYP        KKTYPE
     C     *LIKE         DEFINE    CISYS         KKSYS
     C     *LIKE         DEFINE    EOTRCD        XXTRCD
     C     *LIKE         DEFINE    EACKST        XXCKST
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C*****************************************************************
     C* Start Main Subroutine
     C*****************************************************************
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* CHKERR - Check for adjudication errors.
     C*================================================================
     C     CHKERR        BEGSR
     C*
     C                   MOVE      *OFF          ERRORS            1
     C*
     C* Here we check for ajudication errors and not CodeReview errors
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL     MGMSG#        STATE             2
     C     STATE         IFNE      'CR'
     C                   MOVE      *ON           ERRORS
     C                   LEAVE
     C                   ENDIF
     C*
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* CKAMTS - Calculate current recon amounts.
     C*================================================================
     C     CKAMTS        BEGSR
     C*
     C                   Z-ADD     0             TLCHRG           11 2
     C                   Z-ADD     0             TLPAID           11 2
     C                   Z-ADD     0             TLMRA            11 2
     C                   Z-ADD     0             TLNDIS           11 2
     C                   Z-ADD     0             TLOTHR           11 2
     C                   Z-ADD     0             TLOTH2           11 2
     C*
     C                   Z-ADD     0             SMCHRG           11 2
     C                   Z-ADD     0             SMPAID           11 2
     C                   Z-ADD     0             SMMRA            11 2
     C                   Z-ADD     0             SMNDIS           11 2
     C                   Z-ADD     0             SMOTHR           11 2
     C                   Z-ADD     0             SMOTH2           11 2
     C*
     C* Calculate totals for the EOB for all recons
     C*
     C     CAKY          CHAIN(N)  PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CACHRG        TLCHRG
     C                   ADD       CAPAID        TLPAID
     C                   ADD       CAMRA         TLMRA
     C                   ADD       CANDIS        TLNDIS
     C                   ADD       CAOTHR        TLOTHR
     C                   ADD       CAOTH2        TLOTH2
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C* Calculate amounts for the original bill and previous recons
     C*
     C     EAKY1         CHAIN     PEA02                              97
     C     *IN97         DOWEQ     *OFF
     C*
     C     E2RCN         IFNE      KKRCN
     C                   ADD       E2CHRG        SMCHRG
     C                   ADD       E2PAID        SMPAID
     C                   ADD       E2MRA         SMMRA
     C                   ADD       E2NDIS        SMNDIS
     C                   ADD       E2OTHR        SMOTHR
     C                   ADD       E2OTH2        SMOTH2
     C                   ENDIF                                                  E2RCN NE KKRCN
     C*
     C     EAKY1         READE     PEA02                                  97
     C                   ENDDO
     C*
     C* Calculate difference between total for bill and previous totals
     C*
     C     TLCHRG        SUB       SMCHRG        DFCHRG
     C     TLPAID        SUB       SMPAID        DFPAID
     C     TLMRA         SUB       SMMRA         DFMRA
     C     TLNDIS        SUB       SMNDIS        DFNDIS
     C     TLOTHR        SUB       SMOTHR        DFOTHR
     C     TLOTH2        SUB       SMOTH2        DFOTH2
     C*
     C                   ENDSR
     C*================================================================
     C* MAINSR - Main Processing Subroutine.
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   EXSR      PGMSTR
     C                   EXSR      UPDATR
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* MVDATA - Move data into data base fields
     C*================================================================
     C     MVDATA        BEGSR
     C*
     C* Let's see if there are any adjudication errors
     C*
B01  C     @@CRPE        IFEQ      ' '
     C*** BEGIN DEL *** 10/11/02 **************************************
     C**********           EXSR CHKERR
B02  C********** ERRORS    IFEQ *ON
     C*** END   DEL *** 10/11/02 **************************************
     C                   MOVE      'P'           EOBLST
     C                   MOVE      'Y'           @@CRPE
     C                   MOVE      'CRP'         @@PCOD
     C                   MOVE      'Y'           EOCRPE
     C*
     C                   MOVEL(P)  'P'           KKTYPE
     C                   MOVEL(P)  @@PCOD        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSDESC        @@REAS
     C                   ELSE
     C                   MOVEL     *BLANKS       @@REAS
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 10/11/02 **************************************
S02  C**********           ELSE
     C**********           MOVE 'C'       EOBLST
     C**********           MOVE *BLANKS   EOCRPE
E02  C**********           ENDIF
     C*** END   DEL *** 10/11/02 **************************************
S01  C                   ELSE
     C                   MOVE      'P'           EOBLST
     C                   MOVE      @@CRPE        EOCRPE
E01  C                   ENDIF
     C*
     C                   MOVE      'Y'           EOCRDO
     C*
     C                   MOVEL     XXTRCD        EOTRCD
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL     DSTIME        EOTIME
     C*
     C                   EXSR      CKAMTS
     C*
     C*  Also, check to see if the payment is 0
     C*
     C     ##OUTS        IFEQ      'Y'
     C     DFPAID        ANDGT     0
     C     ##WCIF        ORNE      'Y'
     C     DFPAID        ANDGT     0
     C*
     C     EOBLST        IFEQ      'C'
     C     EOTAPE        ANDEQ     'Y'
     C                   MOVEL     EODT          EOWCCK
     C                   MOVE      *BLANKS       EOSBDT
     C                   ENDIF
     C                   ENDIF                                                  @@OUTS=Y
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C* Processing is done so let's remove the lockout record
     C*
     C     LOKY          CHAIN     PLO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1            WC INST
     C                   PARM      *BLANKS       ##MCIF            1            MC INST
     C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANKS       ##AR              1            MC AR INST
     C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
     C                   PARM      *BLANKS       ##UR              1            PRECERT INST
     C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
     C                   PARM      *BLANKS       ##EDI             1            EDI INST
     C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C*
     C* Get FAEOP record
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C* Load key fields
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKRCN
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C                   MOVE      EOCO#         ##CO#             3
     C                   MOVE      EOSUB#        ##SUB#            5
     C                   MOVE      EOFND         ##FND             3
     C                   MOVE      EOFYR         ##FYR             3
     C                   MOVE      EOCASE        ##CASE            7
     C                   MOVE      EOEOB#        ##EOB#            9
     C                   MOVE      EOFEIN        ##FEIN            9
     C                   MOVE      EOMOD         ##MOD             3
     C*
     C*  If it is an outside account, update EOWCCK & EAESDT
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM      *BLANKS       ##OUTS
     C*
     C* GET THE WORKERS COMP GROUP NUMBER
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       ##WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      ' '
     C                   MOVE      ##WCF#        KKWCF#
     C                   ENDIF
     C*
     C                   MOVEL(P)  'P'           KKTYPE
     C                   MOVEL(P)  EOSYS         KKSYS
     C*
     C     CIKY          CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL     CITRCD        XXTRCD
     C                   ELSE
     C                   MOVEL     CITRCV        XXTRCD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     CITRCD        XXTRCD
     C                   ENDIF
     C                   MOVEL     CISTS         XXCKST
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVDATA
     C*
     C* CALCULATE TOTAL AMOUNTS FOR THE BILL
     C*
     C                   Z-ADD     0             TLCHRG
     C                   Z-ADD     0             TLPAID
     C                   Z-ADD     0             TLMRA
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLOTH2
     C                   Z-ADD     0             EOFRDT
     C                   Z-ADD     0             EOTODT
     C*
     C     CAKY          CHAIN(N)  PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   ADD       CACHRG        TLCHRG
     C                   ADD       CAPAID        TLPAID
     C                   ADD       CAMRA         TLMRA
     C                   ADD       CANDIS        TLNDIS
     C                   ADD       CAOTHR        TLOTHR
     C                   ADD       CAOTH2        TLOTH2
     C*
     C*  Calculates TO and FROM service dates
     C*
     C     EOFRDT        IFEQ      *ZEROS
     C     EOTODT        ANDEQ     *ZEROS
     C                   Z-ADD     CAFDAT        EOFRDT
     C                   Z-ADD     CAFDAT        EOTODT
     C                   ENDIF
     C*
     C     CAFDAT        IFLT      EOFRDT
     C                   Z-ADD     CAFDAT        EOFRDT
     C                   ENDIF
     C*
     C     CAFDAT        IFGT      EOTODT
     C                   Z-ADD     CAFDAT        EOTODT
     C                   ENDIF
     C*
     C     EOAST         IFEQ      'PA'
     C     EOBTYP        ANDEQ     'H'
     C     CATDAT        ANDGT     EOTODT
     C                   Z-ADD     CATDAT        EOTODT
     C                   ENDIF
     C*
     C     CAKY          READE(N)  PCA01                                  99
     C                   ENDDO
     C*
     C                   Z-ADD     TLCHRG        EOCHRG
     C                   Z-ADD     TLPAID        EOPAID
     C                   Z-ADD     TLMRA         EOMRA
     C                   Z-ADD     TLNDIS        EONDIS
     C                   Z-ADD     TLOTHR        EOOTHR
     C                   Z-ADD     TLOTH2        EOOTH2
     C*
     C                   UPDATE    DBRCDF
     C*
     C* WRITE/UPDATE BILL HEADER AMOUNT RECORD FOR ORIGINAL BILL
     C*
     C     KKRCN         IFEQ      *ZEROS
     C     EAKY          CHAIN     PEA02                              98
     C     *IN98         IFEQ      *OFF
     C*
     C                   Z-ADD     EOCHRG        E2CHRG
     C                   Z-ADD     EOMRA         E2MRA
     C                   Z-ADD     EONDIS        E2NDIS
     C                   Z-ADD     EOOTHR        E2OTHR
     C                   Z-ADD     EOOTH2        E2OTH2
     C                   Z-ADD     EOPAID        E2PAID
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@CRKB        E2CRKB
     C*****              MOVEL(P)  @@PMKB        E2PMKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  '99'          E2MEKB
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@FQKB        E2FQKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  XXCKST        E2CKST
     C                   MOVEL(P)  XXTRCD        E2TRCD
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         E2PGM
     C                   MOVEL     Q1USER        E2USER
     C                   Z-ADD     DSTIME        E2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E2DT
     C                   MOVEL     SRDTO         E2TRDT
     C*
     C     @@CRPE        IFEQ      'N'
     C                   MOVEL     *BLANKS       E2EPFL
     C                   ELSE
     C                   MOVE      'E'           E2EPFL
     C                   ENDIF
     C*
     C                   Z-ADD     EOWCCK        E2ESDT
     C*
     C                   UPDATE    PEA02
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EOCHRG        EACHRG
     C                   Z-ADD     EOMRA         EAMRA
     C                   Z-ADD     EONDIS        EANDIS
     C                   Z-ADD     EOOTHR        EAOTHR
     C                   Z-ADD     EOOTH2        EAOTH2
     C                   Z-ADD     EOPAID        EAPAID
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@CRKB        EACRKB
     C*****              MOVEL(P)  @@PMKB        EAPMKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  '99'          EAMEKB
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@FQKB        EAFQKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  XXCKST        EACKST
     C                   MOVEL(P)  XXTRCD        EATRCD
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C                   MOVE      'A'           EASTAT
     C                   MOVE      EOSYS         EASYS
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C                   Z-ADD     EOWCCK        EAESDT
     C*
     C     EOSYS         IFEQ      'H'                                          hospital
     C     EOBTYP        ANDEQ     'H'
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EUEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C     EOSYS         IFEQ      'P'                                          physician
     C     EOBTYP        ANDEQ     'N'
     C     EHKY          CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EHEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     EOSYS         IFEQ      'V'                                          vendor fee
     C     EOBTYP        ANDEQ     'V'
     C*
     C                   MOVE      'P'           EABBLF
     C                   MOVE      'N'           EAEDI
     C*
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     EOSYS         IFEQ      'P'                                          pharmacy
     C     EOBTYP        ANDEQ     'X'
     C     EPKY          CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EPEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'P'
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C                   ELSE
     C     EOPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF                                                  EOPAID LE 0
     C                   ENDIF                                                  F1OUTA = 'Y'
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1USER        EAIUSR
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1PGM         EAIPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSTIME        EAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     @@CRPE        IFEQ      'N'
     C                   MOVEL     *BLANKS       EAEPFL
     C                   ELSE
     C                   MOVE      'E'           EAEPFL
     C                   ENDIF
     C*
     C                   Z-ADD     EOWCCK        EAESDT
     C*
     C                   WRITE     PEA01
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  KKRCN = *ZEROS
     C*
     C* WRITE/UPDATE BILL HEADER AMOUNT RECORD FOR RECONS
     C*
     C     KKRCN         IFNE      *ZEROS
     C*
     C     EAKY          CHAIN     PEA02                              98
     C     *IN98         IFEQ      *OFF
     C     EOCHRG        SUB       SMCHRG        E2CHRG
     C     EOPAID        SUB       SMPAID        E2PAID
     C     EOMRA         SUB       SMMRA         E2MRA
     C     EONDIS        SUB       SMNDIS        E2NDIS
     C     EOOTHR        SUB       SMOTHR        E2OTHR
     C     EOOTH2        SUB       SMOTH2        E2OTH2
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@CRKB        E2CRKB
     C*****              MOVEL(P)  @@PMKB        E2PMKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  '99'          E2MEKB
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@FQKB        E2FQKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  XXCKST        E2CKST
     C                   MOVEL(P)  XXTRCD        E2TRCD
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        E2USER
     C                   MOVEL     Q1PGM         E2PGM
     C                   Z-ADD     DSTIME        E2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E2DT
     C                   MOVEL     SRDTO         E2TRDT
     C*
     C     @@CRPE        IFEQ      'N'
     C                   MOVEL     *BLANKS       EAEPFL
     C                   ELSE
     C                   MOVE      'E'           EAEPFL
     C                   ENDIF
     C*
     C                   Z-ADD     EOWCCK        E2ESDT
     C*
     C                   UPDATE    PEA02
     C                   ELSE
     C*
     C                   CLEAR                   PEA01
     C     EOCHRG        SUB       SMCHRG        EACHRG
     C     EOPAID        SUB       SMPAID        EAPAID
     C     EOMRA         SUB       SMMRA         EAMRA
     C     EONDIS        SUB       SMNDIS        EANDIS
     C     EOOTHR        SUB       SMOTHR        EAOTHR
     C     EOOTH2        SUB       SMOTH2        EAOTH2
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@CRKB        EACRKB
     C*****              MOVEL(P)  @@PMKB        EAPMKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  '99'          EAMEKB
     C*** Beg Del *** 06/11/12 ******************************************
     C*****              MOVEL(P)  @@FQKB        EAFQKB
     C*** End Del *** 06/11/12 ******************************************
     C                   MOVEL(P)  XXCKST        EACKST
     C                   MOVEL(P)  XXTRCD        EATRCD
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     EOSYS         EASYS
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C*
     C     EOSYS         IFEQ      'H'
     C     EOBTYP        ANDEQ     'H'
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EUEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'N'
     C     EHKY          CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EHEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'X'
     C     EPKY          CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EPEDI         EAEDI
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOSYS         IFEQ      'V'                                          vendor fee
     C     EOBTYP        ANDEQ     'V'
     C*
     C                   MOVE      'P'           EABBLF
     C                   MOVE      'N'           EAEDI
     C*
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C                   ELSE
     C     EOPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF                                                  EOPAID LE 0
     C                   ENDIF                                                  F1OUTA = 'Y'
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1USER        EAIUSR
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1PGM         EAIPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSTIME        EAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     EOWCCK        EAESDT
     C*
     C     @@CRPE        IFEQ      'N'
     C                   MOVEL     *BLANKS       EAEPFL
     C                   ELSE
     C                   MOVE      'E'           EAEPFL
     C                   ENDIF
     C*
     C                   WRITE     PEA01
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C                   ENDIF                                                  KKRCN NE *ZEROS
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* Update transaction date in detail amount trans file with date
     C* in header amount file for the current recon.
     C*
     C     EAKY          CHAIN(N)  PEA02                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     DAKY          CHAIN     PDA01                              98
     C     *IN98         DOWEQ     *OFF
     C*
     C                   Z-ADD     E2TRDT        DATRDT
     C                   UPDATE    PDA01
     C*
     C     DAKY          READE     PDA01                                  98
     C                   ENDDO                                                  *IN98=OFF
     C                   ENDIF                                                  *IN99=OFF
     C*
     C* IF BILL STATUS IS PENDED, CLEAR CAFLAG FIELD IN FDCAP SO THAT
     C* ADJUDICATION WILL APPLY RULES, CCI, AND DUPLICATE EDITS
     C*
     C     EOBLST        IFEQ      'P'
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     EORCN         IFEQ      CARCN
     C*
     C*** BEG CHG *** 01/07/03 ****************************************
     C******************** Z-ADD*ZEROS    CACHK#
     C******************** MOVE 'P'       CAFLAG
     C                   Z-ADD     EOEOB#        CACHK#
     C                   MOVE      *BLANK        CAFLAG
     C*** END CHG *** 01/07/03 ****************************************
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C*
     C                   ENDIF                                                  EORCN=CARCN
     C*
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF                                                  EOBLST = 'P'
     C*
     C* IF BILL STATUS IS 'C', WRITE EOB# TO CHK# FIELD IN FDCAP
     C*   THIS WILL ALLOW LINE ITEMS TO APPEAR IN HISTORY
     C*
     C     EOBLST        IFEQ      'C'
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EOEOB#        CACHK#
     C                   MOVE      ' '           CAFLAG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF                                                  EOBLST = 'C'
     C*
     C* CALL F0395R02 TO WRITE BILL STATUS HISTORY RECORD
     C*
     C     @@REAS        IFEQ      *BLANKS
     C                   MOVEL(P)  REASON        ##REAS
     C                   ELSE
     C                   MOVEL(P)  @@REAS        ##REAS
     C                   ENDIF
     C*
     C                   CALL      'F0395R02'
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      EOBLST        ##BLST            1
     C                   PARM      @@PCOD        ##STCD            3
     C                   PARM                    ##REAS           60
     C*
     C* CALL WCMC CHECK REQUEST IF PENDING BILL AND THERE IS AN
     C* AMOUNT TO PAY FROM THE SAME DAY RECON
     C*
     C     ##BLST        IFEQ      'P'
     C     ##OUTS        ANDNE     'Y'
     C     ##WCIF        ANDEQ     'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*
     C* GET WCMS FUND NUMBER
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    ##CO#
     C                   PARM      *BLANKS       ##WCF#            3
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C*
     C* CALL WA040R TO DELETE ANY CHECK REQUESTS FOR THIS EOMB/RCN
     C*
     C                   MOVE      ##WCF#        #1WCF#
     C*
     C                   CALL      'WA040R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    EOCO#
     C                   PARM                    #1WCF#
     C                   PARM                    EOFYR
     C                   PARM                    EOCASE
     C                   PARM                    EOEOB#
     C                   PARM      EORCN         KKEOBS
     C*
     C                   ENDIF                                                  ##BLST=P
     C*
     C* CALL WCMC CHECK REQUEST IF COMPLETING BILL AND THERE IS AN
     C* AMOUNT TO PAY
     C*
     C     ##BLST        IFEQ      'C'
     C     ##OUTS        ANDNE     'Y'
     C     ##WCIF        ANDEQ     'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*
     C* DETERMINE IF A CHECK SHOULD BE ISSUED
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1FL04        ANDEQ     'Y'
     C*
     C* GET WCMS FUND NUMBER
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    ##CO#
     C                   PARM      *BLANKS       ##WCF#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C*
     C* CALL WA040R TO DELETE ANY CHECK REQUESTS FOR THIS EOMB/RCN
     C*
     C                   MOVE      ##WCF#        #1WCF#
     C*
     C                   CALL      'WA040R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    EOCO#
     C                   PARM                    #1WCF#
     C                   PARM                    EOFYR
     C                   PARM                    EOCASE
     C                   PARM                    EOEOB#
     C                   PARM      EORCN         KKEOBS            7 0
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'W0081R'
     C                   PARM      *BLANK        ##CALL
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##EOB#
     C                   PARM      *BLANK        ##RTCD
     C                   PARM      *ZEROS        ##NAMT
     C*
     C                   ENDIF                                                  *IN99
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##BLST = 'C'
     C*
     C                   ENDSR
