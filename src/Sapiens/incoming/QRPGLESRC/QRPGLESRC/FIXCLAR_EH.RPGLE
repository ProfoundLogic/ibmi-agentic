     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXCLAR_EH - CREATE WDEHP RECORDS FOR CONVERTED EMPLRS.
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CONVERT WDELP TO WDEHP FOR   *
     H*                ANY CONVERTED EMPLOYERS IN A GROUP WHO HAVE    *
     H*                NO WDEHP RECORDS.                              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    COMMAND LINE 2 PARM - GROUP AND CONVERT PROGRAM ID         *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/08/02  PROGRAMMER: C:65  PROJECT: WC970          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*****************************************************************
     F*
     FWDELP     IF   E           K DISK
     FWDEHL     IF A E           K DISK
     F*
     F*  AUDIT REPORT
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOB)
     F*

     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C*  EMPLOYER DETAIL HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
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
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFND             3
     C                   PARM                    QQPGM            10
     C*
     C*  LIKE DEFINES
     C*
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHIMPR        EQIMPR
     C     *LIKE         DEFINE    EHISPR        EQISPR
     C     *LIKE         DEFINE    EHINMP        EQINMP
     C     *LIKE         DEFINE    EHIAPR        EQIAPR
     C     *LIKE         DEFINE    EHIMNP        EQIMNP
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
     C     *LIKE         DEFINE    EHIDC         EQIDC
     C     *LIKE         DEFINE    EHIWAV        EQIWAV
     C     *LIKE         DEFINE    EHIMPM        EQIMPM
     C     *LIKE         DEFINE    EHIADL        EQIADL
     C     *LIKE         DEFINE    EHISBJ        EQISBJ
     C     *LIKE         DEFINE    EHWMPR        EQWMPR
     C     *LIKE         DEFINE    EHWSPR        EQWSPR
     C     *LIKE         DEFINE    EHWNMP        EQWNMP
     C     *LIKE         DEFINE    EHWAPR        EQWAPR
     C     *LIKE         DEFINE    EHWMNP        EQWMNP
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
     C     *LIKE         DEFINE    EHWDC         EQWDC
     C     *LIKE         DEFINE    EHWWAV        EQWWAV
     C     *LIKE         DEFINE    EHWMPM        EQWMPM
     C     *LIKE         DEFINE    EHWADL        EQWADL
     C     *LIKE         DEFINE    EHWSBJ        EQWSBJ
     C*
     C                   MOVE      QQFND         XXFND
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             INCNT             7 0
     C                   Z-ADD     0             OTCNT             7 0
     C                   Z-ADD     0             OTCNT2            7 0
     C                   Z-ADD     0             ERCNT             7 0
     C                   Z-ADD     0             WRCNT             7 0
     C                   Z-ADD     0             WRCNT1            7 0
     C                   Z-ADD     0             WRCNT2            7 0
     C                   Z-ADD     0             WRCNT3            7 0
     C                   Z-ADD     0             WRCNT4            7 0
     C                   Z-ADD     0             WRCNT5            7 0
     C                   Z-ADD     0             WRCNT6            7 0
     C                   Z-ADD     0             WRCNT7            7 0
     C                   Z-ADD     0             WRCNT8            7 0
     C                   Z-ADD     0             WRCNT9            7 0
     C*
     C* READ THE INCOMING PC FILE
     C*
     C                   READ      WDELP                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C     ELFND         IFEQ      XXFND
     C     ELPGM         ANDEQ     QQPGM
     C*
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *ON
     C*
     C                   ADD       1             INCNT
     C*
     C                   CLEAR                   PEH01
     C*
     C                   MOVE      ELCO#         EHCO#
     C                   MOVE      ELFND         EHFND
     C                   MOVE      ELFYR         EHFYR
     C                   MOVE      ELEMP#        EHEMP#
     C                   MOVE      ELDIV         EHDIV
     C                   MOVE      ELPFDT        EHEFDT
     C                   MOVE      ELPFDT        EHTRDT
     C                   MOVE      'E'           EHFAF
     C                   MOVE      ELBSCD        EHBSCD
     C                   MOVE      ELPFDT        EHPFDT
     C                   MOVE      ELPTDT        EHPTDT
     C                   MOVE      ELANDT        EHANDT
     C                   MOVE      ELPROR        EHPROR
     C                   MOVE      ELMAPR        EHWMPR
     C                   MOVE      ELSTPR        EHWSPR
     C                   MOVE      ELENMP        EHWNMP
     C                   MOVE      ELANPR        EHWAPR
     C                   MOVE      ELPRTD        EHPRTD
     C                   MOVE      ELASP1        EHASP1
     C                   MOVE      ELCREF        EHCREF
     C                   MOVE      ELCREX        EHCREX
     C                   MOVE      ELEDC1        EHWDC1
     C                   MOVE      ELASP2        EHASP2
     C                   MOVE      ELDVEF        EHDVEF
     C                   MOVE      ELDVEX        EHDVEX
     C                   MOVE      ELEDC2        EHWDC2
     C                   MOVE      ELASP3        EHASP3
     C                   MOVE      ELDISC        EHDISC
     C                   MOVE      ELEDC3        EHWDC3
     C                   MOVE      ELEXM1        EHEXM1
     C                   MOVE      ELEMD1        EHEMD1
     C                   MOVE      ELEXP1        EHWXP1
     C                   MOVE      ELEXM2        EHEXM2
     C                   MOVE      ELEMD2        EHEMD2
     C                   MOVE      ELEXP2        EHWXP2
     C                   MOVE      ELEXM3        EHEXM3
     C                   MOVE      ELEMD3        EHEMD3
     C                   MOVE      ELEXP3        EHWXP3
     C                   MOVE      ELILCD        EHILCD
     C                   MOVE      ELILEF        EHILEF
     C                   MOVE      ELILEX        EHILEX
     C                   MOVE      ELEIN1        EHWIN1
     C                   MOVE      ELINL2        EHI2CD
     C                   MOVE      ELI2EF        EHI2EF
     C                   MOVE      ELI2EX        EHI2EX
     C                   MOVE      ELEIN2        EHWIN2
     C                   MOVE      ELDCDE        EHDCDE
     C                   MOVE      ELDDCT        EHDDCT
     C                   MOVE      ELCXBL        EHCXBL
     C                   MOVE      ELDCEF        EHDCEF
     C                   MOVE      ELDCEX        EHDCEX
     C                   MOVE      ELEDCT        EHWDCT
     C                   MOVE      ELASPM        EHASPM
     C                   MOVE      ELMCEF        EHMCEF
     C                   MOVE      ELMCEX        EHMCEX
     C                   MOVE      ELEDCM        EHWDCM
     C                   MOVE      ELASPS        EHASPS
     C                   MOVE      ELSCEF        EHSCEF
     C                   MOVE      ELSCEX        EHSCEX
     C                   MOVE      ELEDCS        EHWDCS
     C                   MOVE      ELRTCD        EHRTCD
     C                   MOVE      ELSTAT        EHSTAT
     C                   MOVE      ELEMNP        EHWMNP
     C*
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   Z-ADD     DSTIME        EHTIME
     C                   MOVE      Q1USER        EHIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         EHDT
     C                   MOVEL     SRDTO         EHIDT
     C*
     C                   MOVE      ELDEV2        EHDEV2
     C                   MOVE      ELEDV2        EHWDV2
     C                   MOVE      ELD2EF        EHD2EF
     C                   MOVE      ELD2EX        EHD2EX
     C                   MOVE      ELDBCR        EHDBCR
     C                   MOVE      ELEDCR        EHWDCR
     C                   MOVE      ELEDC         EHWDC
     C                   MOVE      ELESBJ        EHWSBJ
     C                   MOVE      ELEMPM        EHWMPM
     C                   MOVE      ELEFRS        EHWFRS
     C                   MOVE      ELTXPR        EHTXPR
     C                   MOVE      ELLFEF        EHLFEF
     C                   MOVE      ELAVCC        EHAVCC
     C                   MOVE      ELEIN2        EHWIN2
     C                   MOVE      ELSDEF        EHSDEF
     C                   MOVE      ELSDEX        EHSDEX
     C                   MOVE      ELEDCR        EHWDCR
     C                   Z-ADD     ELEWAV        EHWWAV
     C                   Z-ADD     0             EHWADL
     C                   MOVE      'E'           EHFAF2
     C                   MOVE      ELEMNP        EHWMNP
     C*                                                                *********
     C* WRITTEN CHG'D
     C     EHWMPR        SUB       0             EHXMPR
     C     EHWSPR        SUB       0             EHXSPR
     C     EHWNMP        SUB       0             EHXNMP
     C     EHWAPR        SUB       0             EHXAPR
     C     EHWDC1        SUB       0             EHXDC1
     C     EHWDC2        SUB       0             EHXDC2
     C     EHWDC3        SUB       0             EHXDC3
     C     EHWXP1        SUB       0             EHXXP1
     C     EHWXP2        SUB       0             EHXXP2
     C     EHWXP3        SUB       0             EHXXP3
     C     EHWIN1        SUB       0             EHXIN1
     C     EHWIN2        SUB       0             EHXIN2
     C     EHWDCT        SUB       0             EHXDCT
     C     EHWDCM        SUB       0             EHXDCM
     C     EHWDCS        SUB       0             EHXDCS
     C     EHWDCR        SUB       0             EHXDCR
     C     EHWDC         SUB       0             EHXDC
     C     EHWSBJ        SUB       0             EHXSBJ
     C     EHWMPM        SUB       0             EHXMPM
     C     EHWFRS        SUB       0             EHXFRS
     C     EHWDV2        SUB       0             EHXDV2
     C     EHWWAV        SUB       0             EHXWAV
     C     EHWADL        SUB       0             EHXADL
     C*
     C* INFORCE
     C     ELPROR        IFNE      0
     C     ELANPR        DIV(H)    ELPROR        XXFL12           11 2
     C     XXFL12        MULT(H)   1             XXFL11           11 0
     C                   Z-ADD     XXFL11        EHIAPR
     C                   END
     C                   MOVEL     EHWMNP        EHIMNP
     C                   Z-ADD     ELMAPR        EHIMPR
     C                   Z-ADD     ELSTPR        EHISPR
     C                   Z-ADD     ELENMP        EHINMP
     C                   Z-ADD     ELEDC1        EHIDC1
     C                   Z-ADD     ELEDC2        EHIDC2
     C                   Z-ADD     ELEDC3        EHIDC3
     C                   Z-ADD     ELEXP1        EHIXP1
     C                   Z-ADD     ELEXP2        EHIXP2
     C                   Z-ADD     ELEXP3        EHIXP3
     C                   Z-ADD     ELEIN1        EHIIN1
     C                   Z-ADD     ELEIN2        EHIIN2
     C                   Z-ADD     ELEDCT        EHIDCT
     C                   Z-ADD     ELEDCM        EHIDCM
     C                   Z-ADD     ELEDCS        EHIDCS
     C                   Z-ADD     ELEDCR        EHIDCR
     C                   Z-ADD     ELEFRS        EHIFRS
     C                   Z-ADD     ELEDV2        EHIDV2
     C                   Z-ADD     ELEDC         EHIDC
     C                   Z-ADD     ELEWAV        EHIWAV
     C                   Z-ADD     ELEXP1        EHIXP1
     C                   Z-ADD     ELEXP2        EHIXP2
     C                   Z-ADD     ELEXP3        EHIXP3
     C                   Z-ADD     ELESBJ        EHISBJ
     C                   Z-ADD     ELEMPM        EHIMPM
     C                   Z-ADD     0             EHIADL
     C*
     C**** SAVE INFORCED FIELDS FOR AUDIT RECORD
     C*
     C* INFORCE
     C                   Z-ADD     EHIMPR        EQIMPR
     C                   Z-ADD     EHISPR        EQISPR
     C                   Z-ADD     EHINMP        EQINMP
     C                   Z-ADD     EHIAPR        EQIAPR
     C                   MOVEL     EHIMNP        EQIMNP
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
     C                   Z-ADD     EHIDC         EQIDC
     C                   Z-ADD     EHIWAV        EQIWAV
     C                   Z-ADD     EHIXP1        EQIXP1
     C                   Z-ADD     EHIXP2        EQIXP2
     C                   Z-ADD     EHIXP3        EQIXP3
     C                   Z-ADD     EHISBJ        EQISBJ
     C                   Z-ADD     EHIMPM        EQIMPM
     C                   Z-ADD     EHIADL        EQIADL
     C*
     C                   Z-ADD     EHWMPR        EQWMPR
     C                   Z-ADD     EHWSPR        EQWSPR
     C                   Z-ADD     EHWNMP        EQWNMP
     C                   Z-ADD     EHWAPR        EQWAPR
     C                   MOVEL     EHWMNP        EQWMNP
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
     C                   Z-ADD     EHWDC         EQWDC
     C                   Z-ADD     EHWWAV        EQWWAV
     C                   Z-ADD     EHWXP1        EQWXP1
     C                   Z-ADD     EHWXP2        EQWXP2
     C                   Z-ADD     EHWXP3        EQWXP3
     C                   Z-ADD     EHWSBJ        EQWSBJ
     C                   Z-ADD     EHWMPM        EQWMPM
     C                   Z-ADD     EHWADL        EQWADL
     C*
     C*  INFORCED CHANGED PREMIUM FIELDS
     C*
     C     EHIMPR        SUB       0             EHJMPR
     C     EHISPR        SUB       0             EHJSPR
     C     EHINMP        SUB       0             EHJNMP
     C     EHIAPR        SUB       0             EHJAPR
     C     EHIDC         SUB       0             EHJDC
     C     EHIDC1        SUB       0             EHJDC1
     C     EHIDC2        SUB       0             EHJDC2
     C     EHIDC3        SUB       0             EHJDC3
     C     EHIXP1        SUB       0             EHJXP1
     C     EHIXP2        SUB       0             EHJXP2
     C     EHIXP3        SUB       0             EHJXP3
     C     EHIIN1        SUB       0             EHJIN1
     C     EHIIN2        SUB       0             EHJIN2
     C     EHIDCT        SUB       0             EHJDCT
     C     EHIDCM        SUB       0             EHJDCM
     C     EHIDCS        SUB       0             EHJDCS
     C     EHIDCR        SUB       0             EHJDCR
     C     EHIFRS        SUB       0             EHJFRS
     C     EHIDV2        SUB       0             EHJDV2
     C     EHIWAV        SUB       0             EHJWAV
     C     EHIADL        SUB       0             EHJADL
     C     EHISBJ        SUB       0             EHJSBJ
     C     EHIMPM        SUB       0             EHJMPM
     C*
     C* IF FLAT CX (ELPFDT = ELPTDT) SET EHTRDT TO 1 DAY BEFORE ELPFDT
     C*
     C     ELPFDT        IFEQ      ELPTDT
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-SUB     1             SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         EHDT
     C                   MOVEL     SRDTO         EHIDT
     C                   END
     C                   WRITE     PEH01
     C                   ADD       1             OTCNT
     C*
     C*
     C* IF EMPLOYER IS AUDITED, WRITE ANOTHER RECORD.
     C     ELFAF         IFEQ      'A'
     C                   CLEAR                   PEH01
     C*
     C                   MOVE      ELCO#         EHCO#
     C                   MOVE      ELFND         EHFND
     C                   MOVE      ELFYR         EHFYR
     C                   MOVE      ELEMP#        EHEMP#
     C                   MOVE      ELDIV         EHDIV
     C                   MOVE      ELPTDT        EHEFDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     XXDT          IFLE      ELPTDT
     C                   Z-ADD     XXDT          EHEFDT
     C                   END
     C*
     C                   MOVE      ELFAF         EHFAF
     C                   MOVE      ELBSCD        EHBSCD
     C                   MOVE      ELPFDT        EHPFDT
     C                   MOVE      ELPTDT        EHPTDT
     C                   MOVE      ELANDT        EHANDT
     C                   MOVE      ELPROR        EHPROR
     C                   MOVE      ELAMPR        EHWMPR
     C                   MOVE      ELASPR        EHWSPR
     C                   MOVE      ELANMP        EHWNMP
     C                   MOVE      ELAAPR        EHWAPR
     C                   MOVE      ELAMNP        EHWMNP
     C                   MOVE      ELPRTD        EHPRTD
     C                   MOVE      ELASP1        EHASP1
     C                   MOVE      ELCREF        EHCREF
     C                   MOVE      ELCREX        EHCREX
     C                   MOVE      ELADC1        EHWDC1
     C                   MOVE      ELASP2        EHASP2
     C                   MOVE      ELDVEF        EHDVEF
     C                   MOVE      ELDVEX        EHDVEX
     C                   MOVE      ELADC2        EHWDC2
     C                   MOVE      ELASP3        EHASP3
     C                   MOVE      ELDISC        EHDISC
     C                   MOVE      ELADC3        EHWDC3
     C                   MOVE      ELEXM1        EHEXM1
     C                   MOVE      ELEMD1        EHEMD1
     C                   MOVE      ELAXP1        EHWXP1
     C                   MOVE      ELEXM2        EHEXM2
     C                   MOVE      ELEMD2        EHEMD2
     C                   MOVE      ELAXP2        EHWXP2
     C                   MOVE      ELEXM3        EHEXM3
     C                   MOVE      ELEMD3        EHEMD3
     C                   MOVE      ELAXP3        EHWXP3
     C                   MOVE      ELILCD        EHILCD
     C                   MOVE      ELILEF        EHILEF
     C                   MOVE      ELILEX        EHILEX
     C                   MOVE      ELAIN1        EHWIN1
     C                   MOVE      ELINL2        EHI2CD
     C                   MOVE      ELI2EF        EHI2EF
     C                   MOVE      ELI2EX        EHI2EX
     C                   MOVE      ELAIN2        EHWIN2
     C                   MOVE      ELDCDE        EHDCDE
     C                   MOVE      ELDDCT        EHDDCT
     C                   MOVE      ELCXBL        EHCXBL
     C                   MOVE      ELDCEF        EHDCEF
     C                   MOVE      ELDCEX        EHDCEX
     C                   MOVE      ELADCT        EHWDCT
     C                   MOVE      ELASPM        EHASPM
     C                   MOVE      ELMCEF        EHMCEF
     C                   MOVE      ELMCEX        EHMCEX
     C                   MOVE      ELADCM        EHWDCM
     C                   MOVE      ELASPS        EHASPS
     C                   MOVE      ELSCEF        EHSCEF
     C                   MOVE      ELSCEX        EHSCEX
     C                   MOVE      ELADCS        EHWDCS
     C                   MOVE      ELRTCD        EHRTCD
     C                   MOVE      ELSTAT        EHSTAT
     C                   MOVE      ELUSER        EHUSER
     C                   MOVE      ELPGM         EHPGM
     C                   MOVE      ELTIME        EHTIME
     C                   MOVE      ELDT          EHDT
     C                   MOVE      EHUSER        EHIUSR
     C                   MOVE      EHDT          EHIDT
     C                   MOVE      ELDEV2        EHDEV2
     C                   MOVE      ELADV2        EHWDV2
     C                   MOVE      ELD2EF        EHD2EF
     C                   MOVE      ELD2EX        EHD2EX
     C                   MOVE      ELDBCR        EHDBCR
     C                   MOVE      ELADCR        EHWDCR
     C                   MOVE      ELADC         EHWDC
     C                   MOVE      ELASBJ        EHWSBJ
     C                   MOVE      ELAMPM        EHWMPM
     C                   MOVE      ELAFRS        EHWFRS
     C                   MOVE      ELTXPR        EHTXPR
     C                   MOVE      ELAVCC        EHAVCC
     C                   MOVE      ELSDEX        EHSDEX
     C                   MOVE      ELSDEF        EHSDEF
     C                   MOVE      ELLFEF        EHLFEF
     C                   Z-ADD     ELAWAV        EHWWAV
     C                   Z-ADD     0             EHWADL
     C*
     C* WRITTEN CHG'D
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
     C     EHWDC         SUB       EQWDC         EHXDC
     C     EHWWAV        SUB       EQWWAV        EHXWAV
     C*
     C* INFORCE
     C     ELPROR        IFNE      0
     C     EHWMPR        DIV(H)    EHPROR        XXWK10           11 0
     C                   Z-ADD     XXWK10        EHIMPR
     C     EHWSPR        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHISPR
     C     EHWNMP        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHINMP
     C     EHWAPR        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIAPR
     C                   MOVEL     EHWMNP        EHIMNP
     C     EHWDC1        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDC1
     C     EHWDC2        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDC2
     C     EHWDC3        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDC3
     C     EHWXP1        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIXP1
     C     EHWXP2        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIXP2
     C     EHWXP3        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIXP3
     C     EHWIN1        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIIN1
     C     EHWIN2        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIIN2
     C     EHWDCT        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDCT
     C     EHWDCM        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDCM
     C     EHWDCS        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDCS
     C     EHWDCR        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDCR
     C     EHWFRS        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIFRS
     C     EHWDV2        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDV2
     C     EHWDC         DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIDC
     C     EHWWAV        DIV(H)    EHPROR        XXWK10
     C                   Z-ADD     XXWK10        EHIWAV
     C                   ELSE
     C                   Z-ADD     EHWMPR        EHIMPR
     C                   Z-ADD     EHWSPR        EHISPR
     C                   Z-ADD     EHWNMP        EHINMP
     C                   Z-ADD     EHWAPR        EHIAPR
     C                   MOVEL     EHWMNP        EHIMNP
     C                   Z-ADD     EHWDC1        EHIDC1
     C                   Z-ADD     EHWDC2        EHIDC2
     C                   Z-ADD     EHWDC3        EHIDC3
     C                   Z-ADD     EHWXP1        EHIXP1
     C                   Z-ADD     EHWXP2        EHIXP2
     C                   Z-ADD     EHWXP3        EHIXP3
     C                   Z-ADD     EHWIN1        EHIIN1
     C                   Z-ADD     EHWIN2        EHIIN2
     C                   Z-ADD     EHWDCT        EHIDCT
     C                   Z-ADD     EHWDCM        EHIDCM
     C                   Z-ADD     EHWDCS        EHIDCS
     C                   Z-ADD     EHWDCR        EHIDCR
     C                   Z-ADD     EHWFRS        EHIFRS
     C                   Z-ADD     EHWDV2        EHIDV2
     C                   Z-ADD     EHWDC         EHIDC
     C                   Z-ADD     EHWWAV        EHIWAV
     C                   END
     C*
     C* INFORCE CHG'D
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
     C     EHIDC         SUB       EQIDC         EHJDC
     C     EHIWAV        SUB       EQIWAV        EHJWAV
     C*
     C                   Z-ADD     ELPTDT        EHTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         EHDT
     C                   MOVEL     SRDTO         EHIDT
     C                   WRITE     PEH01
     C                   ADD       1             OTCNT2
     C*
     C                   END
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELFND IFEQ
     C*
     C                   READ      WDELP                                  50
     C                   ENDDO                                                  *IN50
     C*
     C*  END THE PROGRAM
     C*
     C                   EXCEPT    TOTL
     C                   MOVE      '1'           *INLR
     C***************************************************************
     O*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           44 'LOAD OF WDEHP FILE     '
     O                       DSDATE              78 '  /  /    '
     O          E            HEAD           3
     O                       Q1PGM               10
     O                       PAGE          Z     78
     O                                           73 'PAGE'
     O          E            HEAD        2  2
     O                                           11 'AGCODE'
     O                                           39 'NOTATION'
     O******* E  1             DET
     O*
     O          E            TOTL        2  2
     O                                           30 '***** TOTALS ***** '
     O*
     O          E            TOTL        1  1
     O                                           22 'Total number of record'
     O                                           44 's read  . . . . . . . '
     O                       INCNT         Z     44
     O*
     O          E            TOTL        1  1
     O                                           22 'Total number records w'
     O                                           44 'ritten to WDEHP(EST). '
     O                       OTCNT         Z     44
     O*
     O          E            TOTL        1  1
     O                                           22 'Total number records w'
     O                                           44 'ritten to WDEHP(AUD). '
     O                       OTCNT2        Z     44
     O*
