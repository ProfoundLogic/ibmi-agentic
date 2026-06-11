     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRRVAUD - REVERSE AUDIT STATUS
     H*                                                               *
     H* PROGRAMER: 65                                                 *
     H* PROJECT:   WC609                                              *
     H* DATE:      5/11/00                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK
     FWMEPL     UF   E           K DISK
     FWDEHL12   UF A E           K DISK
     FWDEHL4    IF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDATE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQEMP#        XXEMP#            9 0
     C                   MOVE      QQDIV         XXDIV             5 0
     C                   MOVE      'A'           XXFAF             1
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYM             5 0
      *
      * REVERSE EMPLOYER DETAIL - WDELP
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     ELFAF         ANDEQ     'A'
      *
     C                   MOVE      'E'           ELFAF
     C                   MOVE      ' '           ELAMNP
     C                   MOVE      ' '           ELRAFL
     C                   Z-ADD     0             ELADC1
     C                   Z-ADD     0             ELADC2
     C                   Z-ADD     0             ELADC3
     C                   Z-ADD     0             ELAMPR
     C                   Z-ADD     0             ELAMPM
     C                   Z-ADD     0             ELASPR
     C                   Z-ADD     0             ELANMP
     C                   Z-ADD     0             ELAAPR
     C                   Z-ADD     0             ELAXP1
     C                   Z-ADD     0             ELAXP2
     C                   Z-ADD     0             ELAXP3
     C                   Z-ADD     0             ELADV2
     C                   Z-ADD     0             ELAIN1
     C                   Z-ADD     0             ELAIN2
     C                   Z-ADD     0             ELADCT
     C                   Z-ADD     0             ELADCM
     C                   Z-ADD     0             ELADCS
     C                   Z-ADD     0             ELADCR
     C                   Z-ADD     0             ELADC
     C                   Z-ADD     0             ELAFRS
     C                   Z-ADD     0             ELAWAV
     C                   Z-ADD     0             ELAMIN
     C                   Z-ADD     0             ELAMAX
     C                   Z-ADD     0             ELATCA
      *
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
      *
     C                   UPDATE    PEL01
      *
      * REVERSE EMPLOYER PAYROLL - WMEPP
      *
     C     EPKY          CHAIN     PEP01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      'E'           EPSAFL
     C                   Z-ADD     0             EPAAPR
     C                   Z-ADD     0             EPAMPR
     C                   Z-ADD     0             EPACNT
      *
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   Z-ADD     DSTIME        EPTIME
      *
     C                   UPDATE    PEP01
      *
     C     EPKY          READE     PEP01                                  60
      *
     C                   ENDDO                                                  *IN60 IFEQ
      *
      * REVERSE EMPLOYER HISTORY - WDEHP
      *
     C     EHKY          SETGT     PEH01
     C     EHKY          READPE    PEH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     EHREVF        ANDNE     'Y'
      *
     C                   MOVE      *ON           *IN60
      *
     C     EHFAF         IFEQ      'A'
      *
     C                   MOVEL     EHTRDT        XXTRDT            5 0
     C                   Z-ADD     EHTRDT        XXDATE            7 0
     C                   Z-ADD     EHTIME        XXTIME            6 0
      *
     C     XXTRDT        IFLT      XXSYM
      *
     C     EHKY1         SETGT     PEQ01
     C     EHKY          READPE    PEQ01                                  50
     C     *IN50         DOWEQ     *OFF
     C     EQTRDT        ANDEQ     XXDATE
     C     EQTIME        ANDGE     XXTIME
     C     EHKY          READPE    PEQ01                                  50
     C                   ENDDO
      *
     C     *IN50         IFEQ      *OFF
     C*
     C*  WRITTEN CHANGE PREMIUM FIELDS
     C*
     C     EQWMPR        SUB       EHWMPR        EHXMPR
     C     EQWSPR        SUB       EHWSPR        EHXSPR
     C     EQWNMP        SUB       EHWNMP        EHXNMP
     C     EQWAPR        SUB       EHWAPR        EHXAPR
     C     EQWDC1        SUB       EHWDC1        EHXDC1
     C     EQWDC2        SUB       EHWDC2        EHXDC2
     C     EQWDC3        SUB       EHWDC3        EHXDC3
     C     EQWXP1        SUB       EHWXP1        EHXXP1
     C     EQWXP2        SUB       EHWXP2        EHXXP2
     C     EQWXP3        SUB       EHWXP3        EHXXP3
     C     EQWIN1        SUB       EHWIN1        EHXIN1
     C     EQWIN2        SUB       EHWIN2        EHXIN2
     C     EQWDCT        SUB       EHWDCT        EHXDCT
     C     EQWDCM        SUB       EHWDCM        EHXDCM
     C     EQWDCS        SUB       EHWDCS        EHXDCS
     C     EQWDCR        SUB       EHWDCR        EHXDCR
     C     EQWFRS        SUB       EHWFRS        EHXFRS
     C     EQWDV2        SUB       EHWDV2        EHXDV2
     C     EQWWAV        SUB       EHWWAV        EHXWAV
     C     EQWADL        SUB       EHWADL        EHXADL
     C*
     C*  INFORCED CHANGE PREMIUM FIELDS
     C*
     C     EQIMPR        SUB       EHIMPR        EHJMPR
     C     EQISPR        SUB       EHISPR        EHJSPR
     C     EQINMP        SUB       EHINMP        EHJNMP
     C     EQIAPR        SUB       EHIAPR        EHJAPR
     C     EQIDC1        SUB       EHIDC1        EHJDC1
     C     EQIDC2        SUB       EHIDC2        EHJDC2
     C     EQIDC3        SUB       EHIDC3        EHJDC3
     C     EQIXP1        SUB       EHIXP1        EHJXP1
     C     EQIXP2        SUB       EHIXP2        EHJXP2
     C     EQIXP3        SUB       EHIXP3        EHJXP3
     C     EQIIN1        SUB       EHIIN1        EHJIN1
     C     EQIIN2        SUB       EHIIN2        EHJIN2
     C     EQIDCT        SUB       EHIDCT        EHJDCT
     C     EQIDCM        SUB       EHIDCM        EHJDCM
     C     EQIDCS        SUB       EHIDCS        EHJDCS
     C     EQIDCR        SUB       EHIDCR        EHJDCR
     C     EQIFRS        SUB       EHIFRS        EHJFRS
     C     EQIDV2        SUB       EHIDV2        EHJDV2
     C     EQIWAV        SUB       EHIWAV        EHJWAV
     C     EQIADL        SUB       EHIADL        EHJADL
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C                   Z-ADD     EQWMPR        EHWMPR
     C                   Z-ADD     EQWSPR        EHWSPR
     C                   Z-ADD     EQWNMP        EHWNMP
     C                   Z-ADD     EQWAPR        EHWAPR
     C                   Z-ADD     EQWDC1        EHWDC1
     C                   Z-ADD     EQWDC2        EHWDC2
     C                   Z-ADD     EQWDC3        EHWDC3
     C                   Z-ADD     EQWXP1        EHWXP1
     C                   Z-ADD     EQWXP2        EHWXP2
     C                   Z-ADD     EQWXP3        EHWXP3
     C                   Z-ADD     EQWIN1        EHWIN1
     C                   Z-ADD     EQWIN2        EHWIN2
     C                   Z-ADD     EQWDCT        EHWDCT
     C                   Z-ADD     EQWDCM        EHWDCM
     C                   Z-ADD     EQWDCS        EHWDCS
     C                   Z-ADD     EQWDCR        EHWDCR
     C                   Z-ADD     EQWFRS        EHWFRS
     C                   Z-ADD     EQWDV2        EHWDV2
     C                   Z-ADD     EQWWAV        EHWWAV
     C                   Z-ADD     EQWADL        EHWADL
     C*
     C*  INFORCED PREMIUM FIELDS
     C*
     C                   Z-ADD     EQIMPR        EHIMPR
     C                   Z-ADD     EQISPR        EHISPR
     C                   Z-ADD     EQINMP        EHINMP
     C                   Z-ADD     EQIAPR        EHIAPR
     C                   Z-ADD     EQIDC1        EHIDC1
     C                   Z-ADD     EQIDC2        EHIDC2
     C                   Z-ADD     EQIDC3        EHIDC3
     C                   Z-ADD     EQIXP1        EHIXP1
     C                   Z-ADD     EQIXP2        EHIXP2
     C                   Z-ADD     EQIXP3        EHIXP3
     C                   Z-ADD     EQIIN1        EHIIN1
     C                   Z-ADD     EQIIN2        EHIIN2
     C                   Z-ADD     EQIDCT        EHIDCT
     C                   Z-ADD     EQIDCM        EHIDCM
     C                   Z-ADD     EQIDCS        EHIDCS
     C                   Z-ADD     EQIDCR        EHIDCR
     C                   Z-ADD     EQIFRS        EHIFRS
     C                   Z-ADD     EQIDV2        EHIDV2
     C                   Z-ADD     EQIWAV        EHIWAV
     C                   Z-ADD     EQIADL        EHIADL
      *
     C                   ELSE
     C*
     C*  WRITTEN CHANGE PREMIUM FIELDS
     C*
     C                   Z-SUB     EHWMPR        EHXMPR
     C                   Z-SUB     EHWSPR        EHXSPR
     C                   Z-SUB     EHWNMP        EHXNMP
     C                   Z-SUB     EHWAPR        EHXAPR
     C                   Z-SUB     EHWDC1        EHXDC1
     C                   Z-SUB     EHWDC2        EHXDC2
     C                   Z-SUB     EHWDC3        EHXDC3
     C                   Z-SUB     EHWXP1        EHXXP1
     C                   Z-SUB     EHWXP2        EHXXP2
     C                   Z-SUB     EHWXP3        EHXXP3
     C                   Z-SUB     EHWIN1        EHXIN1
     C                   Z-SUB     EHWIN2        EHXIN2
     C                   Z-SUB     EHWDCT        EHXDCT
     C                   Z-SUB     EHWDCM        EHXDCM
     C                   Z-SUB     EHWDCS        EHXDCS
     C                   Z-SUB     EHWDCR        EHXDCR
     C                   Z-SUB     EHWFRS        EHXFRS
     C                   Z-SUB     EHWDV2        EHXDV2
     C                   Z-SUB     EHWWAV        EHXWAV
     C                   Z-SUB     EHWADL        EHXADL
     C*
     C*  INFORCED CHANGE PREMIUM FIELDS
     C*
     C                   Z-SUB     EHIMPR        EHJMPR
     C                   Z-SUB     EHISPR        EHJSPR
     C                   Z-SUB     EHINMP        EHJNMP
     C                   Z-SUB     EHIAPR        EHJAPR
     C                   Z-SUB     EHIDC1        EHJDC1
     C                   Z-SUB     EHIDC2        EHJDC2
     C                   Z-SUB     EHIDC3        EHJDC3
     C                   Z-SUB     EHIXP1        EHJXP1
     C                   Z-SUB     EHIXP2        EHJXP2
     C                   Z-SUB     EHIXP3        EHJXP3
     C                   Z-SUB     EHIIN1        EHJIN1
     C                   Z-SUB     EHIIN2        EHJIN2
     C                   Z-SUB     EHIDCT        EHJDCT
     C                   Z-SUB     EHIDCM        EHJDCM
     C                   Z-SUB     EHIDCS        EHJDCS
     C                   Z-SUB     EHIDCR        EHJDCR
     C                   Z-SUB     EHIFRS        EHJFRS
     C                   Z-SUB     EHIDV2        EHJDV2
     C                   Z-SUB     EHIWAV        EHJWAV
     C                   Z-SUB     EHIADL        EHJADL
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C                   Z-ADD     0             EHWMPR
     C                   Z-ADD     0             EHWSPR
     C                   Z-ADD     0             EHWNMP
     C                   Z-ADD     0             EHWAPR
     C                   Z-ADD     0             EHWDC1
     C                   Z-ADD     0             EHWDC2
     C                   Z-ADD     0             EHWDC3
     C                   Z-ADD     0             EHWXP1
     C                   Z-ADD     0             EHWXP2
     C                   Z-ADD     0             EHWXP3
     C                   Z-ADD     0             EHWIN1
     C                   Z-ADD     0             EHWIN2
     C                   Z-ADD     0             EHWDCT
     C                   Z-ADD     0             EHWDCM
     C                   Z-ADD     0             EHWDCS
     C                   Z-ADD     0             EHWDCR
     C                   Z-ADD     0             EHWFRS
     C                   Z-ADD     0             EHWDV2
     C                   Z-ADD     0             EHWWAV
     C                   Z-ADD     0             EHWADL
     C*
     C*  INFORCED PREMIUM FIELDS
     C*
     C                   Z-ADD     0             EHIMPR
     C                   Z-ADD     0             EHISPR
     C                   Z-ADD     0             EHINMP
     C                   Z-ADD     0             EHIAPR
     C                   Z-ADD     0             EHIDC1
     C                   Z-ADD     0             EHIDC2
     C                   Z-ADD     0             EHIDC3
     C                   Z-ADD     0             EHIXP1
     C                   Z-ADD     0             EHIXP2
     C                   Z-ADD     0             EHIXP3
     C                   Z-ADD     0             EHIIN1
     C                   Z-ADD     0             EHIIN2
     C                   Z-ADD     0             EHIDCT
     C                   Z-ADD     0             EHIDCM
     C                   Z-ADD     0             EHIDCS
     C                   Z-ADD     0             EHIDCR
     C                   Z-ADD     0             EHIFRS
     C                   Z-ADD     0             EHIDV2
     C                   Z-ADD     0             EHIWAV
     C                   Z-ADD     0             EHIADL
      *
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C*  LOAD THE TRANSACTION DATE AND UPDATE THE HISTORY RECORD
     C*
     C                   MOVE      'Y'           EHREVF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     EHTRDT        EHEFDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHTRDT
     C*
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
      *
     C                   WRITE     PEH01
      *
     C                   ELSE
      *
     C                   DELETE    PEH01
      *
     C                   ENDIF                                                  XXTRDT IFLT
      *
     C     EHKY          READPE    PEH01                                  60
      *
     C                   ENDIF                                                  EHFAF IFEQ
      *
     C                   ENDDO                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      *ON           *INLR
