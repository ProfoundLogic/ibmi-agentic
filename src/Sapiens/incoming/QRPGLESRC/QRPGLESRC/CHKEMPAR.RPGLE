     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKEMPAR - CHECK FOR EMPLOYERS WITH WIPED A/R WITH WDEHP
     H*
     FWDEHL10   IF   E           K DISK
     FWDEHL5    IF   E           K DISK
     F                                     RENAME(PEH01:PEH02)
     FSMCOL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWC936     O    E             DISK    USROPN
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D QCA             S             56    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EHTRDT                 1      7  0
     D  YYMM                   1      5  0

     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     *LIKE         DEFINE    EHCO#         XXCLT
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHPFDT        XXFDT
     C     *LIKE         DEFINE    EHPFDT        XXTDT
     C     *LIKE         DEFINE    EHPFDT        SVPFDT
     C     *LIKE         DEFINE    EHTRDT        SVTRDT
     C     *LIKE         DEFINE    EHDIV         SVDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCLT             3
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* SET UP KEY INFORMATION
     C*
     C                   MOVE      QQCLT         XXCLT
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFDT         X1FDT             8 0
     C                   MOVE      QQTDT         X1TDT             8 0
     C*
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C*
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*  REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             Z                 2 0
     C     *BLANK        LOOKUP    CONM(Z)                            60
     C                   MOVEA     CONM(Z)       XXCONM           50
     C*
     C* CLEAR OUTFILE
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C* OPEN OUTFILE
     C*
     C                   OPEN      WC936
     C*
     C* PRINT HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C* READ EMPLOYER HISTORY
     C*
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* FIND A RECORD THAT FALLS WITHIN CRITERIA
     C*
     C     EHTRDT        IFGE      XXFDT
     C     EHTRDT        ANDLE     XXTDT
     C     EHFAF         ANDEQ     'E'
     C*
     C                   Z-ADD     EHPFDT        SVPFDT
     C                   Z-ADD     EHTRDT        SVTRDT
     C                   Z-ADD     EHDIV         SVDIV
     C                   Z-ADD     YYMM          SVYM              5 0
     C*
     C                   Z-ADD     EHFND         XXFND
     C                   Z-ADD     EHFYR         XXFYR
     C                   Z-ADD     EHEMP#        XXEMP#
     C*
     C* SEARCH FOR ANOTHER RECORD THAT OCCURS LATER AND HITS CRITERIA
     C*
     C     EHKY1         CHAIN     PEH02                              70
     C     *IN70         DOWEQ     *OFF
     C*
     C     EHTRDT        IFGT      SVTRDT
     C     EHTRDT        ANDLE     XXTDT
     C     EHFAF         ANDEQ     'E'
     C*
     C                   Z-ADD     EHPFDT        SVPFDT
     C                   Z-ADD     EHTRDT        SVTRDT
     C                   Z-ADD     EHDIV         SVDIV
     C                   Z-ADD     YYMM          SVYM              5 0
     C*
     C                   ENDIF                                                  EHTRDT IFGT
     C*
     C     EHKY1         READE     PEH02                                  70
     C                   ENDDO
     C*
     C* SEARCH FOR RECORDS POSTED IN PRIOR MONTH TO LATEST ORIGINAL BILL
     C*
     C     EHKY1         CHAIN     PEH02                              70
     C     *IN70         DOWEQ     *OFF
     C*
     C     YYMM          IFLT      SVYM
     C     EHFAF         ANDEQ     'E'
     C*
     C* PRINT EMPLOYER
     C*
     C                   EXSR      PRINT
     C*
     C                   ENDIF                                                  YYMM IFLT
     C*
     C     EHKY1         READE     PEH02                                  70
     C                   ENDDO
     C*
     C* JUMP TO NEXT FYR/EMP#
     C*
     C     EHKY1         SETGT     PEH01
     C*
     C                   ENDIF                                                  EHTRDT IFGE
     C*
     C* READ NEXT EMPLOYER HISTORY
     C*
     C     EHKY          READE     PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* PRINT TOTAL COUNT
     C*
     C     XXCNT         IFNE      *ZERO
     C                   EXCEPT    COUNT
     C                   ELSE
     C                   EXCEPT    NOCNT
     C                   ENDIF
     C*
     C* CLOSE OUTFILE
     C*
     C                   CLOSE     WC936
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*
     C* PRINT AN EMPLOYER TO THE REPORT
     C*
     C     PRINT         BEGSR
     C*
     C                   MOVE      *BLANK        XXTYPE
     C                   MOVEL(P)  EHBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C  N60              MOVE      BSSEFL        XXTYPE            1
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   Z-ADD     SVTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSTRDT            8 0
     C*
     C                   Z-ADD     EHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C                   Z-ADD     SVPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSPFDT            8 0
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   Z-ADD     EHCO#         ZZCO#
     C                   Z-ADD     EHFND         ZZFND
     C                   Z-ADD     EHFYR         ZZFYR
     C                   Z-ADD     EHEMP#        ZZEMP#
     C                   Z-ADD     EHDIV         ZZDIV
     C                   Z-ADD     EHTRDT        ZZTRDT
     C                   Z-ADD     EHPFDT        ZZPFDT
     C                   MOVE      EHFAF         ZZFAF
     C                   MOVE      XXTYPE        ZZTYPE
     C                   WRITE     PZZ01
     C*
     C                   ADD       1             XXCNT
     C*
     C                   ENDSR                                                  PRINT
     C*
     C*****************************************************************
     C*
     C*
     C*
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'CHKEMPAR: '
     O                                           36 'PGM TO CHECK IF EMPLR '
     O                                           56 'A/R HAS BEEN DELETED'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                            8 'CLT:'
     O                       XXCLT         4     12
     O                                           21 'CO#:'
     O                       XXCO#         4     25
     O                                           38 'FROM DT:'
     O                       X1FDT               49 '  /  /    '
     O                                           60 'TO DT:'
     O                       X1TDT               71 '  /  /    '
     O                       XXCONM             124
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           54 'BILL TYPE'
     O                                           65 'EHTRDT'
     O                                           78 'SVTRDT'
     O                                           88 'SVDIV'
     O                                          101 'EHPFDT'
     O                                          114 'SVPFDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHFAF               39
     O                       XXTYPE              50
     O                       XXTRDT              65 '  /  /    '
     O                       XSTRDT              78 '  /  /    '
     O                       SVDIV         4     88
     O                       XXPFDT             101 '  /  /    '
     O                       XSPFDT             114 '  /  /    '
     O          E            COUNT       1  1
     O                                           20 'TOTAL POLCIES:'
     O                       XXCNT         1     26
     O          E            NOCNT       1  1
     O                                           25 'NO POLCIES MATCHED'
**  QCMDEXC TABLE
CLRPFM WC936
