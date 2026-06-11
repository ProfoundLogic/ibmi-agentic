     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXINPR - FIX INFORCED PREMIUM
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  FIX INFORCE PREMIUM FOR A TRANS DATE.   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDEHL6    UF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWAELL     IF A E           K DISK    USROPN
     F                                     RENAME(PEL01:PEL02)
     FWDDSL     IF   E           K DISK
     FWADSL     IF A E           K DISK    USROPN
     F                                     RENAME(PDS01:PDS02)
     FWDILL     IF   E           K DISK
     FWAILL     IF A E           K DISK    USROPN
     F                                     RENAME(PIL01:PIL02)
     FWDMPL     IF   E           K DISK
     FWAMPL     IF A E           K DISK    USROPN
     F                                     RENAME(PMP01:PMP02)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SSY01)
     F*

     D QCA             S             55    DIM(17) CTDATA PERRCD(1)             CMD EXEC/QCAEXEC
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  EMPLOYER DISCOUNT FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDATE
     C*
     C*  STATISTICAL FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
     C                   KFLD                    EHPFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  WORK COMP CONTROL FILE
     C*
     C     WCKY          KLIST
     C                   KFLD                    XCKEY
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEFDT        XXEFDT
     C     *LIKE         DEFINE    EHFAF         XXFAF
     C     *LIKE         DEFINE    WCKEY         XCKEY
     C     *LIKE         DEFINE    ELANDT        XXANDT
     C     *LIKE         DEFINE    ELANDT        XXLPDT
     C     *LIKE         DEFINE    ELANDT        XQFROM
     C     *LIKE         DEFINE    ELANDT        XQTO
     C     *LIKE         DEFINE    EPEAPR        XXEAPR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEFDT            8
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
     C*  IF EMPLOYER NUMBER IS NOT BLANK, SET UP FOR SINGLE RECORD PROCESS
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     999           XXDATE            7 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*  OVERRIDE DATABASE FILE WDELL TO WAELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDDSL TO WADSL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDILL TO WAILL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(12)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDMPL TO WAMPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(15)
     C                   PARM                    LENGTH           15 5
     C*
     C*  GET SYSTEM VALUE FOR SPREAD DISCOUNT FLAG
     C*
     C                   MOVEL(P)  'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        WCFL02
     C                   ENDIF
     C*
     C*  OPEN THE OVERRIDE FILES
     C*
     C                   OPEN      WAELL
     C                   OPEN      WADSL
     C                   OPEN      WAILL
     C                   OPEN      WAMPL
     C*
     C     ELKY1         CHAIN     PEL03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C     EHKY          CHAIN     PEH01                              60
     C  N60              EXSR      LDOVRF
     C     ELKY1         READE     PEL03                                  60
     C                   ENDDO
     C*
     C*  IF WE ARE SPREADING THE DISCOUNT, RUN THE BILLING CALC FOR
     C*  EACH DIVISION TO GET THE INFORCED STANDARD PREMIUM INTO EACH
     C*  DIVISION'S OVERRIDE RECORD.
     C*
     C                   MOVE      ' '           Q1SPRD            1
     C*
     C     WCFL02        IFEQ      'Y'
     C*
     C*  GET DISCOUNT VALUE FOR THIS EMPLOYER, DIV = 0
     C*
     C     ELKY1         CHAIN     PEL03                              60
     C*
     C     *IN60         IFEQ      '0'
     C     ELDISC        ANDNE     *BLANK
     C*
     C*  SEE IF ANOTHER DIVISION EXISTS
     C*
     C     ELKY1         READE     PEL03                                  60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      'Y'           Q1SPRD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  WCFL02 = Y
     C*
     C*  IF THE DISCOUNT IS SPREAD, LOAD OVERRIDE FILES WAELP,
     C*  WAEPP, AND WADSP FOR ALL DIVISIONS.
     C*
     C     Q1SPRD        IFEQ      'Y'
     C     ELKY2         CHAIN(N)  PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C     EHKY          CHAIN(N)  PEH01                              60
     C*
     C                   MOVE      XXCO#         ZZCO#             3
     C                   MOVE      XXFND         ZZFND             3
     C                   MOVE      XXFYR         ZZFYR             3
     C                   MOVE      XXEMP#        ZZEMP#            9
     C                   MOVE      XXDIV         ZZDIV             5
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZDATE            8
     C*
     C                   MOVE      *BLANKS       ZZESCD            2
     C                   MOVE      'Y'           ZZOVR             1
     C                   MOVE      *BLANKS       ZZSRCD            2
     C                   MOVE      *BLANKS       ZZPRT             1
     C                   MOVE      *BLANKS       ZZAJCD            2
     C                   MOVE      'W'           ZZQTE             1
     C                   MOVE      'H'           ZZRNB             1
     C*
     C                   Z-ADD     EHIADL        XX10             11 0
     C                   MOVE      XX10          QQADLD           11
     C                   Z-ADD     EHIAPR        XX10             11 0
     C                   MOVE      XX10          QQONE            11
     C                   Z-ADD     EHISPR        XX10             11 0
     C                   MOVE      XX10          QQTWO            11
     C                   CALL      'FIXIN80'
     C                   PARM                    ZZCO#
     C                   PARM                    ZZFND
     C                   PARM                    ZZFYR
     C                   PARM                    ZZEMP#
     C                   PARM                    ZZDIV
     C                   PARM                    ZZDATE
     C                   PARM                    ZZESCD
     C                   PARM                    ZZOVR
     C                   PARM                    ZZSRCD
     C                   PARM                    ZZPRT
     C                   PARM                    ZZAJCD
     C                   PARM                    ZZQTE
     C                   PARM                    ZZRNB
     C                   PARM                    QQADLD           11
     C                   PARM                    QQONE            11
     C                   PARM                    QQTWO            11
     C*
     C     ELKY2         READE(N)  PEL02                                  60
     C                   ENDDO
     C*
     C                   END                                                    Q1SPRD = Y
     C*
     C* RESET EMPLOYER HISTORY INFO
     C*
     C     ELKY2         CHAIN(N)  PEL02                              50
     C*
     C* RUN THE BILL CALCULATION TO GET THE INFORCED PREMIUM
     C*
     C     *IN50         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C     EHKY          CHAIN     PEH01                              60
     C*
     C                   MOVE      XXCO#         ZZCO#             3
     C                   MOVE      XXFND         ZZFND             3
     C                   MOVE      XXFYR         ZZFYR             3
     C                   MOVE      XXEMP#        ZZEMP#            9
     C                   MOVE      XXDIV         ZZDIV             5
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZDATE            8
     C*
     C                   MOVE      *BLANKS       ZZESCD            2
     C                   MOVE      'Y'           ZZOVR             1
     C                   MOVE      *BLANKS       ZZSRCD            2
     C                   MOVE      *BLANKS       ZZPRT             1
     C                   MOVE      *BLANKS       ZZAJCD            2
     C                   MOVE      'W'           ZZQTE             1
     C                   MOVE      'H'           ZZRNB             1
     C                   Z-ADD     EHIADL        XX10             11 0
     C                   MOVE      XX10          QQADLD           11
     C                   Z-ADD     EHIAPR        XX10             11 0
     C                   MOVE      XX10          QQONE            11
     C                   Z-ADD     EHISPR        XX10             11 0
     C                   MOVE      XX10          QQTWO            11
     C                   CALL      'FIXIN80'
     C                   PARM                    ZZCO#
     C                   PARM                    ZZFND
     C                   PARM                    ZZFYR
     C                   PARM                    ZZEMP#
     C                   PARM                    ZZDIV
     C                   PARM                    ZZDATE
     C                   PARM                    ZZESCD
     C                   PARM                    ZZOVR
     C                   PARM                    ZZSRCD
     C                   PARM                    ZZPRT
     C                   PARM                    ZZAJCD
     C                   PARM                    ZZQTE
     C                   PARM                    ZZRNB
     C                   PARM                    QQADLD           11
     C                   PARM                    QQONE            11
     C                   PARM                    QQTWO            11
     C*
     C*  LOAD INFORCED FIELDS FROM THE OVERRIDE DETAIL RECORD
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
     C                   MOVE      ELMAPR        EHIMPR
     C                   MOVE      ELSTPR        EHISPR
     C                   MOVE      ELENMP        EHINMP
     C                   Z-ADD     ELANPR        EHIAPR
     C                   MOVE      ELEMNP        EHIMNP
     C                   MOVE      ELEDC1        EHIDC1
     C                   MOVE      ELEDC2        EHIDC2
     C                   MOVE      ELEDC3        EHIDC3
     C                   MOVE      ELEXP1        EHIXP1
     C                   MOVE      ELEXP2        EHIXP2
     C                   MOVE      ELEXP3        EHIXP3
     C                   MOVE      ELEIN1        EHIIN1
     C                   MOVE      ELEIN2        EHIIN2
     C                   MOVE      ELEDCT        EHIDCT
     C                   MOVE      ELEDCM        EHIDCM
     C                   MOVE      ELEDCS        EHIDCS
     C                   MOVE      ELEDCR        EHIDCR
     C                   MOVE      ELEFRS        EHIFRS
     C                   MOVE      ELEDV2        EHIDV2
     C                   MOVE      ELEDC         EHIDC
     C                   Z-ADD     ELEWAV        EHIWAV
     C*
     C*  LOAD THE TRANSACTION DATE AND UPDATE THE HISTORY RECORD
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     XXEFDT        EHTRDT
     C*
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C*
     C                   UPDATE    PEH01
     C*
     C*  LOAD THE INFORCED PREMIUM INTO EMPLOYER DETAIL
     C*
     C     ELKY2         READE     PEL02                                  50
     C                   ENDDO
     C*
     C*  DELETE OVERRIDE FOR WDELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*
     C*  DELETE OVERRIDE FOR WDDSL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WDILL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(13)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C*
     C*  DELETE OVERRIDE FOR WDMPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(16)
     C                   PARM                    LENGTH           15 5
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*************************************************************
     C*   SUBROUTINE LDOVRF
     C*   LOAD DATA TO OVERRIDE FILES
     C*************************************************************
     C*
     C     LDOVRF        BEGSR
     C*
     C                   MOVE      EHRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C*  CALCULATE THE ANNIVERSARY RATING PERIOD START DATE.
     C*  IF IT FALLS OUTSIDE THE POLICY PERIOD, SET IT TO 999999.
     C*
     C                   Z-ADD     EHANDT        XXANDT
     C                   ADD       10000         XXANDT
     C*
     C     XXANDT        IFLT      EHPFDT
     C     XXANDT        ORGT      EHPTDT
     C                   Z-ADD     9999999       XXANDT
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXADLD           11 2
     C                   Z-ADD     EHWMPR        XXWMPR            9 0
     C                   Z-ADD     0             XXONE            11 0
     C                   Z-ADD     0             XXTWO            11 0
     C*
     C     EPKY1         CHAIN     PEP01                              61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C     EPIDT         IFLE      XXEFDT
     C     EPSTAT        ANDNE     'I'
     C*
     C*  FOR AUDITED INFORCED PREMIUM, LOAD THE AUDIT PAYROLL INTO
     C*  THE ESTIMATED PAYROLL FIELD.
     C*
     C     EHFAF         IFEQ      'A'
     C                   Z-ADD     EPAMPR        EPEMPR
     C                   ENDIF
     C*
     C                   Z-ADD     EPEMPR        XXWK90
     C                   Z-ADD     EPADLD        XXWK91
     C*
     C     EPEFDT        IFNE      EHPFDT
     C     EPEFDT        ANDNE     XXANDT
     C*
     C*  DETERMINE THE PAYROLL DAYS
     C*
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM            8 0
     C*
     C     EPEFDT        IFLT      XXANDT
     C     XXANDT        ANDNE     9999999
     C*
     C                   Z-ADD     XXANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO              8 0
     C                   ELSE
     C                   Z-ADD     EHPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO
     C                   ENDIF
     C                   MOVE      X1FROM        QQFROM            8 0
     C                   MOVE      X1TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     X1TO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1FROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        PRDAYS            5 0
     C*
     C*  IF THE POLICY IS ANNIVERSARY RATED, CALCULATE THE DAYS
     C*  IN EACH RATING PERIOD TO SPLIT THE ANNUALIZED PAYROLL
     C*
     C     XXANDT        IFNE      9999999
     C*
     C*  DETERMINE THE RATING DAYS
     C*
     C                   Z-ADD     0             XXLPDT
     C                   Z-ADD     EHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     XXANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   MOVE      X1FROM        QQFROM
     C                   MOVE      X1TO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     EHPFDT        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      EHPFDT
     C                   Z-ADD     XXANDT        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      EHPFDT
     C     XXLPDT        ORGT      XXANDT
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1FROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        RDAYS1            5 0
     C     365           SUB       RDAYS1        RDAYS2            5 0
     C*
     C*  IF THE PAYROLL EFFECTIVE DATE FALLS AFTER THE ANNIVERSARY
     C*  RATING DATE, WRITE THE SECOND PERIOD RECORD.
     C*
     C                   Z-ADD     0             XXWK92            9 0
     C                   Z-ADD     0             XAWK92            9 0
     C*
     C     EPEFDT        IFGT      XXANDT
     C*
     C     PRDAYS        IFNE      *ZERO
     C     RDAYS2        DIV       PRDAYS        XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C     EPADLD        IFEQ      *ZERO
     C*
     C                   Z-ADD     EPEMPR        XXEAPR
     C     EPEMPR        MULT(H)   XXFACT        XXWK92            9 0
     C                   Z-ADD     XXEAPR        EPEMPR
     C*
     C                   ELSE
     C*
     C     EPADLD        DIV(H)    EPEAPR        XXRATE            5 0
     C*
     C                   Z-ADD     EPEAPR        XXEAPR
     C                   Z-ADD     EPADLD        X1ADLD            9 0
     C     EPEAPR        MULT(H)   XXFACT        XAWK92            9 0
     C     XXRATE        MULT      XAWK92        XAWK92
     C                   Z-ADD     XXEAPR        EPEAPR
     C                   Z-ADD     X1ADLD        EPADLD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  ADJUST THE PAYROLL AMOUNT FOR THE FIRST PERIOD
     C*
     C     PRDAYS        IFNE      *ZERO
     C     RDAYS1        DIV       PRDAYS        XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C     EPADLD        IFEQ      *ZERO
     C*
     C     EPEMPR        MULT(H)   XXFACT        XXWK90            9 0
     C*
     C                   ELSE
     C*
     C     EPADLD        DIV(H)    EPEAPR        XXRATE            5 0
     C*
     C     EPEAPR        MULT(H)   XXFACT        XXWK91            9 0
     C     XXRATE        MULT      XXWK91        XXWK91
     C                   ENDIF
     C*
     C                   ADD       XXWK92        XXWK90
     C                   ADD       XAWK92        XXWK91
     C*
     C                   ELSE
     C*
     C*  IF THE POLICY IS NOT ANNIVERSARY RATED, ANNUALIZE THE PAYROLL
     C*
     C     PRDAYS        IFNE      *ZERO
     C     365           DIV       PRDAYS        XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C     EPADLD        IFEQ      *ZERO
     C*
     C     EPEMPR        MULT(H)   XXFACT        XXWK90            9 0
     C*
     C                   ELSE
     C*
     C     EPADLD        DIV(H)    EPEAPR        XXRATE            5 0
     C*
     C     EPEAPR        MULT(H)   XXFACT        XXWK91            9 0
     C     XXRATE        MULT      XXWK91        XXWK91
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXANDT <> 999999
     C*
     C                   ELSE
     C*
     C     EHFAF         IFEQ      'A'
     C     EHPROR        ANDNE     *ZERO
     C*
     C     EPADLD        IFEQ      *ZERO
     C*
     C     EPEMPR        DIV(H)    EHPROR        XXWK90            9 0
     C                   SUB       EPEMPR        XXWMPR
     C                   ADD       XXWK90        XXWMPR
     C*
     C                   ELSE
     C*
     C     EPADLD        DIV(H)    EPEAPR        XXRATE            5 0
     C*
     C     EPEAPR        DIV(H)    EHPROR        XXWK91            9 0
     C     XXRATE        MULT      XXWK91        XXWK91
     C                   ENDIF
     C*
     C                   ENDIF                                                  EHFAF IFEQ
     C*
     C                   ENDIF                                                  EPEFDT <> ELPFDT
     C*                                               AND  EPEFDT <> XXANDT
     C                   ADD       XXWK91        XXADLD
     C*                                               AND  EPEFDT <> XXANDT
     C     XXANDT        IFNE      9999999
     C     EPEFDT        ANDGE     XXANDT
     C                   ADD       XXWK90        XXTWO
     C                   ELSE
     C                   ADD       XXWK90        XXONE
     C                   ENDIF
     C*                                               AND  EPEFDT <> XXANDT
     C                   ENDIF
     C*                                               AND  EPEFDT <> XXANDT
     C     EPKY1         READE     PEP01                                  61
     C                   ENDDO
     C*
     C                   Z-ADD     XXADLD        EHIADL
     C                   Z-ADD     XXONE         EHIAPR
     C                   Z-ADD     XXTWO         EHISPR
     C*
     C                   UPDATE    PEH01
     C*
     C*  WRITE THE DETAIL RECORD IF IT IS NOT ALREADY WRITTEN
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C*  IF WE ARE LOADING THE CHANGED DIVISION, LOAD THE PREMIUM
     C*  FACTORS AND POLICY DATES FROM THE HISTORY RECORD.
     C*
     C                   MOVE      EHPFDT        ELPFDT
     C                   MOVE      EHPTDT        ELPTDT
     C                   MOVE      EHANDT        ELANDT
     C                   MOVE      EHASP1        ELASP1
     C                   MOVE      EHASP2        ELASP2
     C                   MOVE      EHASP3        ELASP3
     C                   MOVE      EHDISC        ELDISC
     C                   MOVE      EHEXM1        ELEXM1
     C                   MOVE      EHEXM2        ELEXM2
     C                   MOVE      EHEXM3        ELEXM3
     C                   MOVE      EHILCD        ELILCD
     C                   MOVE      EHI2CD        ELINL2
     C                   MOVE      EHDCDE        ELDCDE
     C                   MOVE      EHDDCT        ELDDCT
     C                   MOVE      EHCXBL        ELCXBL
     C                   MOVE      EHASPM        ELASPM
     C                   MOVE      EHASPS        ELASPS
     C                   MOVE      EHAVCC        ELAVCC
     C                   MOVE      EHDBCR        ELDBCR
     C*
     C*  ANNUALIZE THE OVERRIDE DETAIL RECORD.  MAKE THE MOST RECENT
     C*  EXPERIENCE MODIFIER THE ONLY EXPERIENCE MODIFIER.  IF THERE
     C*  IS A SECOND INCREASED LIMIT, USE FOR THE ONLY INCREASED LIMIT.
     C*  USE THE POLICY START DATE FOR ALL PREMIUM FACTOR EFFECTIVE
     C*  DATES.  SET ALL PREMIUM EXPIRATION DATES TO ZERO.
     C*
     C     ELEXM3        IFNE      *ZERO
     C                   Z-ADD     ELEXM3        ELEXM1
     C                   ELSE
     C*
     C     ELEXM2        IFNE      *ZERO
     C                   Z-ADD     ELEXM2        ELEXM1
     C                   ENDIF                                                  ELEXM2 <> 0
     C*
     C                   ENDIF                                                  ELEXM3 <> 0
     C*
     C     ELINL2        IFNE      *BLANK
     C                   MOVE      ELINL2        ELILCD
     C                   MOVE      *BLANKS       ELINL2
     C                   Z-ADD     0             ELI2EF
     C                   Z-ADD     0             ELI2EX
     C                   ENDIF
     C*
     C     ELPFDT        ADD       10000         ELPTDT
     C*
     C                   Z-ADD     *ZERO         ELEXM2
     C                   Z-ADD     *ZERO         ELEXM3
     C*
     C                   Z-ADD     ELPFDT        ELCREF
     C                   Z-ADD     *ZERO         ELCREX
     C                   Z-ADD     ELPFDT        ELDVEF
     C                   Z-ADD     *ZERO         ELDVEX
     C                   Z-ADD     ELPFDT        ELEMD1
     C                   Z-ADD     *ZERO         ELEMD2
     C                   Z-ADD     *ZERO         ELEMD3
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     *ZERO         ELILEX
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     *ZERO         ELDCEX
     C                   Z-ADD     ELPFDT        ELMCEF
     C                   Z-ADD     *ZERO         ELMCEX
     C                   Z-ADD     ELPFDT        ELSCEF
     C                   Z-ADD     *ZERO         ELSCEX
     C                   Z-ADD     ELPFDT        ELSDEF
     C                   Z-ADD     *ZERO         ELSDEX
     C*
     C                   WRITE     PEL02
     C*
     C                   ENDIF
     C*
     C*  IF WE ARE SPREADING THE DISCOUNT, LOAD THE DISCOUNT RECORD
     C*
     C     Q1SPRD        IFEQ      'Y'
     C*
     C     DSKY          CHAIN     PDS02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     DSKY          CHAIN     PDS01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     XXCO#         DSCO#
     C                   Z-ADD     XXFND         DSFND
     C                   Z-ADD     XXFYR         DSFYR
     C                   Z-ADD     XXEMP#        DSEMP#
     C                   Z-ADD     XXDIV         DSDIV
     C                   Z-ADD     ELSTPR        DSSTPR
     C                   Z-ADD     0             DSEDIS
     C                   Z-ADD     0             DSTEPR
     C                   Z-ADD     0             DSTEDS
     C                   Z-ADD     0             DSEPCT
     C                   Z-ADD     0             DSANSP
     C                   Z-ADD     0             DSACSP
     C                   Z-ADD     0             DSADIS
     C                   Z-ADD     0             DSTANP
     C                   Z-ADD     0             DSTACP
     C                   Z-ADD     0             DSAPCT
     C                   MOVE      *BLANKS       DSSTAT
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C                   WRITE     PDS02
     C                   ENDIF
     C*
     C                   ENDIF                                                  Q1SPRD = 'Y'
     C*** BEGIN ADD ***  03/20/00  ************************************
     C     DSKY          CHAIN     PIL02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     DSKY          CHAIN     PIL01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     XXCO#         ILCO#
     C                   Z-ADD     XXFND         ILFND
     C                   Z-ADD     XXFYR         ILFYR
     C                   Z-ADD     XXEMP#        ILEMP#
     C                   Z-ADD     XXDIV         ILDIV
     C                   Z-ADD     ELEIN1        ILEIN1
     C                   Z-ADD     0             ILAIN1
     C                   Z-ADD     ELEIN2        ILEIN2
     C                   Z-ADD     0             ILAIN2
     C                   MOVE      *BLANKS       ILSTAT
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ILDT
     C                   Z-ADD     DSTIME        ILTIME
     C                   WRITE     PIL02
     C                   ENDIF
     C*
     C     DSKY          CHAIN     PMP02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     DSKY          CHAIN     PMP01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     XXCO#         MPCO#
     C                   Z-ADD     XXFND         MPFND
     C                   Z-ADD     XXFYR         MPFYR
     C                   Z-ADD     XXEMP#        MPEMP#
     C                   Z-ADD     XXDIV         MPDIV
     C                   Z-ADD     ELANPR        MPANPR
     C                   Z-ADD     0             MPAAPR
     C                   MOVE      *BLANK        MPSTAT
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MPUSER
     C                   MOVEL     Q1PGM         MPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MPDT
     C                   Z-ADD     DSTIME        MPTIME
     C                   WRITE     PMP02
     C                   ENDIF
     C*
     C*** END ADD *****  03/20/00  ************************************
     C*
     C                   ENDSR
**  QCAEXEC TABLE
OVRDBF FILE(WDELL) TOFILE(QTEMP/WAELL)
OVRDBF FILE(WMEPL1) TOFILE(QTEMP/WAEPL1)
OVRDBF FILE(WMEPL) TOFILE(QTEMP/WAEPL)
DLTOVR FILE(WDELL)
DLTOVR FILE(WMEPL1)
DLTOVR FILE(WMEPL)
OVRDBF FILE(WDDSL) TOFILE(QTEMP/WADSL)
DLTOVR FILE(WDDSL)
CLRPFM FILE(QTEMP/WAELP)
CLRPFM FILE(QTEMP/WAEPP)
CLRPFM FILE(QTEMP/WADSP)
OVRDBF FILE(WDILL) TOFILE(QTEMP/WAILL)
DLTOVR FILE(WDILL)
CLRPFM FILE(QTEMP/WAILP)
OVRDBF FILE(WDMPL) TOFILE(QTEMP/WAMPL)
DLTOVR FILE(WDMPL)
CLRPFM FILE(QTEMP/WAMPP)
