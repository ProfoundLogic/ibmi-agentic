     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKWDEHP - CHECK FOR WDEHP RECORDS NOT IN BALANCE
     H*
     FWDEHL6    UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     EHKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C*
     C     *LIKE         DEFINE    EHCO#         SVCO#
     C     *LIKE         DEFINE    EHFND         SVFND
     C     *LIKE         DEFINE    EHFYR         SVFYR
     C     *LIKE         DEFINE    EHEMP#        SVEMP#
     C     *LIKE         DEFINE    EHDIV         SVDIV
     C*
     C     *LIKE         DEFINE    EHWMPR        EQWMPR
     C     *LIKE         DEFINE    EHWSPR        EQWSPR
     C     *LIKE         DEFINE    EHWNMP        EQWNMP
     C     *LIKE         DEFINE    EHWAPR        EQWAPR
     C     *LIKE         DEFINE    EHWDC1        EQWDC1
     C     *LIKE         DEFINE    EHWDC2        EQWDC2
     C     *LIKE         DEFINE    EHWDC3        EQWDC3
     C     *LIKE         DEFINE    EHWXP1        EQWXP1
     C     *LIKE         DEFINE    EHWXP2        EQWXP2
     C     *LIKE         DEFINE    EHWXP3        EQWXP3
     C     *LIKE         DEFINE    EHWIN1        EQWIN1
     C     *LIKE         DEFINE    EHWIN2        EQWIN2
     C     *LIKE         DEFINE    EHWDCT        EQWDCT
     C     *LIKE         DEFINE    EHWDCM        EQWDCM
     C     *LIKE         DEFINE    EHWDCS        EQWDCS
     C     *LIKE         DEFINE    EHWDCR        EQWDCR
     C     *LIKE         DEFINE    EHWFRS        EQWFRS
     C     *LIKE         DEFINE    EHWDV2        EQWDV2
     C     *LIKE         DEFINE    EHIMPR        EQIMPR
     C     *LIKE         DEFINE    EHISPR        EQISPR
     C     *LIKE         DEFINE    EHINMP        EQINMP
     C     *LIKE         DEFINE    EHIAPR        EQIAPR
     C     *LIKE         DEFINE    EHIDC1        EQIDC1
     C     *LIKE         DEFINE    EHIDC2        EQIDC2
     C     *LIKE         DEFINE    EHIDC3        EQIDC3
     C     *LIKE         DEFINE    EHIXP1        EQIXP1
     C     *LIKE         DEFINE    EHIXP2        EQIXP2
     C     *LIKE         DEFINE    EHIXP3        EQIXP3
     C     *LIKE         DEFINE    EHIIN1        EQIIN1
     C     *LIKE         DEFINE    EHIIN2        EQIIN2
     C     *LIKE         DEFINE    EHIDCT        EQIDCT
     C     *LIKE         DEFINE    EHIDCM        EQIDCM
     C     *LIKE         DEFINE    EHIDCS        EQIDCS
     C     *LIKE         DEFINE    EHIDCR        EQIDCR
     C     *LIKE         DEFINE    EHIFRS        EQIFRS
     C     *LIKE         DEFINE    EHIDV2        EQIDV2
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   MOVE      *OFF          *IN67
     C     Q1PRMS        IFGT      1
     C                   MOVE      *ON           *IN67
     C                   MOVE      QQCO#         SVCO#
     C                   MOVE      QQFND         SVFND
     C                   MOVE      QQEMP#        SVEMP#
     C                   MOVE      QQDIV         SVDIV
     C                   MOVE      QQFYR         SVFYR
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEAD
     C*
     C  N67              READ      PEH01                                  60
     C   67EHKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANK        XXFIRS
     C*
     C     EHCO#         IFNE      XXCO#
     C     EHFND         ORNE      XXFND
     C     EHFYR         ORNE      XXFYR
     C     EHEMP#        ORNE      XXEMP#
     C     EHDIV         ORNE      XXDIV
     C*
     C                   Z-ADD     EHCO#         XXCO#
     C                   Z-ADD     EHFND         XXFND
     C                   Z-ADD     EHFYR         XXFYR
     C                   Z-ADD     EHEMP#        XXEMP#
     C                   Z-ADD     EHDIV         XXDIV
     C                   Z-ADD     0             XXWAPR           11 2
     C                   MOVE      'Y'           XXFIRS            1
     C                   Z-ADD     0             XXIAPR           11 2
     C                   Z-ADD     0             EQWMPR
     C                   Z-ADD     0             EQWSPR
     C                   Z-ADD     0             EQWNMP
     C                   Z-ADD     0             EQWAPR
     C                   Z-ADD     0             EQWDC1
     C                   Z-ADD     0             EQWDC2
     C                   Z-ADD     0             EQWDC3
     C                   Z-ADD     0             EQWXP1
     C                   Z-ADD     0             EQWXP2
     C                   Z-ADD     0             EQWXP3
     C                   Z-ADD     0             EQWIN1
     C                   Z-ADD     0             EQWIN2
     C                   Z-ADD     0             EQWDCT
     C                   Z-ADD     0             EQWDCM
     C                   Z-ADD     0             EQWDCS
     C                   Z-ADD     0             EQWDCR
     C                   Z-ADD     0             EQWFRS
     C                   Z-ADD     0             EQWDV2
     C*
     C                   Z-ADD     0             EQIMPR
     C                   Z-ADD     0             EQISPR
     C                   Z-ADD     0             EQINMP
     C                   Z-ADD     0             EQIAPR
     C                   Z-ADD     0             EQIDC1
     C                   Z-ADD     0             EQIDC2
     C                   Z-ADD     0             EQIDC3
     C                   Z-ADD     0             EQIXP1
     C                   Z-ADD     0             EQIXP2
     C                   Z-ADD     0             EQIXP3
     C                   Z-ADD     0             EQIIN1
     C                   Z-ADD     0             EQIIN2
     C                   Z-ADD     0             EQIDCT
     C                   Z-ADD     0             EQIDCM
     C                   Z-ADD     0             EQIDCS
     C                   Z-ADD     0             EQIDCR
     C                   Z-ADD     0             EQIFRS
     C                   Z-ADD     0             EQIDV2
     C*
     C                   ENDIF
     C*
     C     EHWAPR        SUB       XXWAPR        XXAMT            11 2
     C     EHIAPR        SUB       XXIAPR        XIAMT            11 2
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C     XXAMT         IFNE      EHXAPR
     C     XIAMT         ORNE      EHJAPR
     C*
     C     XXFIRS        IFEQ      'Y'
     C     EHXAPR        ANDNE     *ZERO
     C     XXFIRS        OREQ      'Y'
     C     EHJAPR        ANDNE     *ZERO
     C     XXFIRS        ORNE      'Y'
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C                   EXCEPT    DET
     C*
     C     *IN66         IFEQ      *ON
     C                   EXSR      FIX
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXFIRS IFEQ
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     EHWAPR        XXWAPR
     C                   Z-ADD     EHIAPR        XXIAPR
     C*
     C                   Z-ADD     EHWMPR        EQWMPR
     C                   Z-ADD     EHWSPR        EQWSPR
     C                   Z-ADD     EHWNMP        EQWNMP
     C                   Z-ADD     EHWAPR        EQWAPR
     C                   Z-ADD     EHWDC1        EQWDC1
     C                   Z-ADD     EHWDC2        EQWDC2
     C                   Z-ADD     EHWDC3        EQWDC3
     C                   Z-ADD     EHWXP1        EQWXP1
     C                   Z-ADD     EHWXP2        EQWXP2
     C                   Z-ADD     EHWXP3        EQWXP3
     C                   Z-ADD     EHWIN1        EQWIN1
     C                   Z-ADD     EHWIN2        EQWIN2
     C                   Z-ADD     EHWDCT        EQWDCT
     C                   Z-ADD     EHWDCM        EQWDCM
     C                   Z-ADD     EHWDCS        EQWDCS
     C                   Z-ADD     EHWDCR        EQWDCR
     C                   Z-ADD     EHWFRS        EQWFRS
     C                   Z-ADD     EHWDV2        EQWDV2
     C*
     C                   Z-ADD     EHIMPR        EQIMPR
     C                   Z-ADD     EHISPR        EQISPR
     C                   Z-ADD     EHINMP        EQINMP
     C                   Z-ADD     EHIAPR        EQIAPR
     C                   Z-ADD     EHIDC1        EQIDC1
     C                   Z-ADD     EHIDC2        EQIDC2
     C                   Z-ADD     EHIDC3        EQIDC3
     C                   Z-ADD     EHIXP1        EQIXP1
     C                   Z-ADD     EHIXP2        EQIXP2
     C                   Z-ADD     EHIXP3        EQIXP3
     C                   Z-ADD     EHIIN1        EQIIN1
     C                   Z-ADD     EHIIN2        EQIIN2
     C                   Z-ADD     EHIDCT        EQIDCT
     C                   Z-ADD     EHIDCM        EQIDCM
     C                   Z-ADD     EHIDCS        EQIDCS
     C                   Z-ADD     EHIDCR        EQIDCR
     C                   Z-ADD     EHIFRS        EQIFRS
     C                   Z-ADD     EHIDV2        EQIDV2
     C*
     C  N67              READ      PEH01                                  60
     C   67EHKY          READE     PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*
     C     FIX           BEGSR
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C     EHWMPR        SUB       EQWMPR        EHXMPR
     C     EHWSPR        SUB       EQWSPR        EHXSPR
     C     EHWNMP        SUB       EQWNMP        EHXNMP
     C     EHWAPR        SUB       EQWAPR        EHXAPR
     C     EHWDC1        SUB       EQWDC1        EHXDC1
     C     EHWDC2        SUB       EQWDC2        EHXDC2
     C     EHWDC3        SUB       EQWDC3        EHXDC3
     C     EHWXP1        SUB       EQWXP1        EHXXP1
     C     EHWXP2        SUB       EQWXP2        EHXXP2
     C     EHWXP3        SUB       EQWXP3        EHXXP3
     C     EHWIN1        SUB       EQWIN1        EHXIN1
     C     EHWIN2        SUB       EQWIN2        EHXIN2
     C     EHWDCT        SUB       EQWDCT        EHXDCT
     C     EHWDCM        SUB       EQWDCM        EHXDCM
     C     EHWDCS        SUB       EQWDCS        EHXDCS
     C     EHWDCR        SUB       EQWDCR        EHXDCR
     C     EHWFRS        SUB       EQWFRS        EHXFRS
     C     EHWDV2        SUB       EQWDV2        EHXDV2
     C*
     C*  INFORCED PREMIUM FIELDS
     C*
     C     EHIMPR        SUB       EQIMPR        EHJMPR
     C     EHISPR        SUB       EQISPR        EHJSPR
     C     EHINMP        SUB       EQINMP        EHJNMP
     C     EHIAPR        SUB       EQIAPR        EHJAPR
     C     EHIDC1        SUB       EQIDC1        EHJDC1
     C     EHIDC2        SUB       EQIDC2        EHJDC2
     C     EHIDC3        SUB       EQIDC3        EHJDC3
     C     EHIXP1        SUB       EQIXP1        EHJXP1
     C     EHIXP2        SUB       EQIXP2        EHJXP2
     C     EHIXP3        SUB       EQIXP3        EHJXP3
     C     EHIIN1        SUB       EQIIN1        EHJIN1
     C     EHIIN2        SUB       EQIIN2        EHJIN2
     C     EHIDCT        SUB       EQIDCT        EHJDCT
     C     EHIDCM        SUB       EQIDCM        EHJDCM
     C     EHIDCS        SUB       EQIDCS        EHJDCS
     C     EHIDCR        SUB       EQIDCR        EHJDCR
     C     EHIFRS        SUB       EQIFRS        EHJFRS
     C     EHIDV2        SUB       EQIDV2        EHJDV2
     C                   MOVEL(P)  Q1PGM         EHPGM
     C*
     C                   UPDATE    PEH01
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           24 'FIXWDEHP'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF WDEHP '
     O                                           42 'CHANGE # IS WRONG'
     O               66                          73 'UPDATE MODE IS ACTIVE'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           59 'EHWAPR'
     O                                           76 'EHXAPR'
     O                                           93 'PRIOR WAPR'
     O                                          110 'CORRECT XAPR'
     O                                          124 'EHTRDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHFAF               39
     O                       EHWAPR        J     59
     O                       EHXAPR        J     76
     O                       XXWAPR        J     93
     O                       XXAMT         J    110
     O                       XXTRDT             124 '  /  /    '
