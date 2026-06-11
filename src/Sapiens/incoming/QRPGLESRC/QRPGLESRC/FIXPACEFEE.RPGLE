     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE table file setup                                                              *
     H**************************************************************************************
     H*  Description:  This program will create the fee checks to the department of aging  *
     H*                for files that have already been processed but will need to be      *
     H*                reprocessed.                                                        *
     H*                                                                                    *
     H*  CREATED: 09/14/10  PROGRAMMER: C:189 PROJECT:  114MXA                             *
     H*                                                                                    *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H**************************************************************************************
     FFAEOLG    IF   E           K DISK
     FFAU1P     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FWMNRL     IF   E           K DISK
     FWMCIL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FWARPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*
     FWARPLN    IF A E           K DISK
     FWTPSL     IF   E           K DISK
     Fqsysprt   o    f  132        printer oflind(*inof)
     D
     D*  Date/Time Data Structure.
     D                 ds
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
    D*
     D                 DS
     D  DATA                   1     50
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D*
     D psds          esds                  extname(szq1p)
     D TRANS           C                   CONST('Transaction ID:')
     D DIGITS          C                   CONST('0123456789')
     d USERID          C                   CONST('PDSPGMR   ')
     D COMAPI          C                   CONST('FA007R')
     D DLY5SEC         C                   CONST('DLYJOB DLY(3)')
     D ##CMD           S             14
     D ##CMDL          S             15  5
     D*
     IPCM03
     I              CMOSCL                      CWOSCL
    C
     C     *LIKE         DEFINE    RPSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C     *LIKE         DEFINE    PCKEY         KKKEY
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    RPEOBS        KKEOBS
     C     *LIKE         DEFINE    CIPTYP        KKPTYP
     C     *LIKE         DEFINE    CISYS         KKSYS
    C
     C*  KEY LISTS
     C*
     C     CMKY4         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C*
     C     eoky          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C*
     C     RPKY1         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     C*
     C     RPKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C*
     C     NRKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     CIKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKPTYP
     C                   KFLD                    KKSYS
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEADER
     C*
    C*MAINLINE STARTS HERE
     C*
     C                   Read      pu101                                  60
     C                   dow       *in60 = *off
     C*
     C                   If        *inof = *on
     C                   except    header
     c                   endif
     C
     C                   if        u1ntwk  = 'PACE' and u1eob# <> 0
     C                             and u1prfl = 'Y'
     C
     C     eoky          Chain     peo01                              60
     C                   if        *in60 = *off  and eoblst = 'C'
     C                   exsr      wrtrpx
     C                   else
     c                   If        eoblst = 'D' or eoblst = 'P'
     C                   except    detail
     C                   endif                                                  rcd found FAEOP
     C                   endif                                                  rcd found FAEOP
    C
     C                   else
    C
     c                   If        u1eob# = 0 and u1ntwk = 'PACE'
     C                   except    detail
     C                   endif
    C
     C                   endif                                                  ntwk = 'PACE'
    C
     C                   Read      pu101                                  60
     C                   Enddo
    C
    C                   seton                                        lr
    C                   return
     C*================================================================
     C* WRTRPX - Write WARPP record for PACE bills for client 114
     C*================================================================
     C     WRTRPX        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  99
     C                   MOVE      ##CO#         #1CO#
     C*
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKEOBS
     C     RPKY1         CHAIN     PRP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     RPMOD         ANDNE     001
     C     RPFEIN        ANDEQ     EOFEIN
     C*
     C     RPKY2         SETGT     PRP02
     C     RPKY2         READPE    PRP02                                  99
     C     *IN99         IFEQ      *OFF
     C     RPSEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PRP01
     C                   Z-ADD     EOCO#         RPCO#
     C                   Z-ADD     KKWCF#        RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   Z-ADD     XXSEQ#        RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   Z-ADD     EOFEIN        RPFEIN
     C                   Z-ADD     001           RPMOD
     C                   Z-ADD     U1COST        RPAMT
     C                   MOVEL     'EP'          RPTRCD
     C*
     C     CMKY4         CHAIN     PCM03                              99
     C                   MOVEL     'P'           KKPTYP
     C                   MOVEL     EOSYS         KKSYS
     C     CIKY          CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
     C                   ELSE
     C     EOBTYP        IFEQ      'X'
     C                   MOVE(P)   CIXBR2        RPBR2
     C                   ELSE
     C                   MOVE(P)   CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE(P)   CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE(P)   CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVEL(P)  CISTS         RPSTS
     C*
     C                   ELSE
     C*
     C     EOSYS         IFEQ      'P'
     C                   MOVEL(P)  'MD'          RPBR2
     C                   ELSE
     C                   MOVEL(P)  'HP'          RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'LT'          RPBR7
     C                   ELSE
     C                   MOVEL(P)  'NL'          RPBR7
     C                   ENDIF
     C*
     C                   MOVEL(P)  'PD'          RPSTS
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C     ##CLT#        IFEQ      '114'
     C                   MOVEL(P)  '98'          RPBR2
     C                   MOVEL(P)  'EX'          RPRECD
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'PC'          RPBR7
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'M'
     C                   MOVEL(P)  'PD'          RPBR7
     C                   ENDIF
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVEL(P)  'EP'          RPTRCD
     C                   MOVEL(P)  'EQ'          RPBR2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
     C*
     C                   MOVEL(P)  CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C*
     C     EOBTYP        IFEQ      'X'
     C     CMCVFL        ANDEQ     'Y'
     C                   MOVEL(P)  'EP'          RPTRCD
     C                   ENDIF                                                  ##CLT# = 114
     C*
     C                   MOVEL(P)  'M'           RPRETP
     C*
     C                   MOVEL(P)  RPTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL(P)  PCDESC        RPDESC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL(P)  RPRECD        KKKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF                                                  *IN60 = '0'
     C*
     C                   MOVEL(P)  EOPNAM        RPPNAM
     C                   MOVEL(P)  EOADR1        RPADR1
     C                   MOVEL(P)  EOADR2        RPADR2
     C                   MOVEL(P)  EOCTY         RPCTY
     C                   MOVEL(P)  EOST          RPST
     C                   Z-ADD     EOZIP         RPZIP
     C                   MOVEL     'P'           RPSTAT
     C                   MOVEL(P)  EOUSER        RPUSER
     C                   MOVEL(P)  EOPGM         RPPGM
     C                   Z-ADD     EOTIME        RPTIME
     C                   Z-ADD     EODT          RPDT
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   Z-ADD     EORCN         RPEOBS
     C                   Z-ADD     EODT          RPRCVD
     C*
     C     TRANS         CAT(P)    U1TRID:1      RPRMDS
     C*
     C                   MOVEL(P)  RPSTS         KKKEY
     C     TBKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   PSSRT         RPSORT
     C                   ENDIF
     C*
     C                   MOVEL(P)  'N'           RPSENT
     C                   MOVEL(P)  USERID        USER             10
     C                   MOVEL(P)  USER          RPRQST
     C*
     C*  Call API to determine the Bank "Code".
     C*
     C                   MOVE      RPCO#         $$CO#
     C                   MOVE      RPFND         $$FND
     C                   MOVE      RPFYR         $$FYR
     C                   MOVE      RPCASE        $$CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C*
     C                   CALL      'WA023R1'
     C                   PARM      '2'           $$MODE            1            Mode
     C                   PARM                    $$CO#             3            Company
     C                   PARM                    $$FND             3            Group #
     C                   PARM                    $$FYR             3            Year
     C                   PARM                    $$CASE            7            Case
     C                   PARM      'WC'          $$APPL            2            Applic Code
     C                   PARM      DATA          $$DATA           50            Selection criteria
     C                   PARM      *BLANKS       $$CKTP           10            Bank Acct Code
     C                   PARM      *BLANKS       $$RTCD            1            Return Code
     C*
     C                   MOVEL(P)  $$CKTP        RPCKTP                         Bank Acct# Code
     C*
     C                   MOVEL(P)  Q1USER        RPIUSR
     C                   MOVEL(P)  Q1PGM         RPIPGM
     C                   Z-ADD     DSTIME        RPITM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPIDT
     C*
     C     RPAMT         IFGT      *ZEROS
     C                   WRITE     PRP01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     OQSYSPRT   E            header         1 03
     O                       ##CONM              83
     O          E            header         1
     O                       dstime              08 '  :  :  '
     O                                           52 'Report of Records for'
     O                                           77 'which EOB# is not found '
     O                                           98 'or deleted/pended    '
     O                       dsdate             128 '  /  /    '
     O          E            header         3
     O                       Q1PGM               10
     O                                          124 'PAGE:'
     O                       PAGE               128
     O          E            header         1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            header         1
     O                                           25 'Control Number'
     O                                         +012 'Fee  '
     O                                         +008 'EOB#'
     O                                         +005 'EOB status '
     O          E            header         1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O          E            detail         1
     O                       u1trid              25
     O                       u1cost        4   +005
     O                       u1eob#        4   +005
     O                       eoblst            +008
