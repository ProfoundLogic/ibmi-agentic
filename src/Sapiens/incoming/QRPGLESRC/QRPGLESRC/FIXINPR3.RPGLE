     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXINPR3 - FIX INFORCED PREMIUM
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   *
     H*                                                               *
     H*  DEVELOPED IN RESPONSE TO 51273 AT CAREWEST.                  *
     H*  THIS PROGRAM UPDATES INFORCE NUMBERS FOR WDEHP RECORDS WITH  *
     H*  TRANSACTION DATES AND THEN CALLS FIXWDEHP11 TO UPDATE        *
     H*  THE PREMIUM CHANGE NUMBERS. THIS PROGRAM CAN BE CALLED       *
     H*  FOR A SINGLE POLICY YEAR (THE FIRST FIVE PARMS MUST BE       *
     H*  POPULATED FOR THIS.) IF AN 'R' IS PASSED IN THE 7TH PARM     *
     H*  THE PROGRAM WILL START AT THE PARTICULAR TRDT PASSED IN      *
     H*  THE SIXTH PARM.                                              *
     H*  OTHERWISE, PASS IN ALL BLANKS TO RUN OVER ALL POLICIES.      *
     H*  THIS PROGRAM AND FIXWDEHP11 ONLY UPDATE RECORDS IN WHICH     *
     H*  EHPGM = 'SRINPR'                                             *
     H*                                                               *
     H*  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   FIXINPRC3                                                   *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   W8010R                                                      *
     H*   FIXWDEHP11                                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  20  PROCESS A SINGLE RECORD                                  *
     H*  50  PRIMARY READ INDICATOR                                   *
     H*  60  UTILITY INDICATOR                                        *
     H*  61  UTILITY INDICATOR                                        *
     H*****************************************************************
     FWDEHL6    UF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWAEPL     IF A E           K DISK    USROPN
     F                                     RENAME(PEP01:PEP02)
     FWDELL     UF   E           K DISK
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
      *** BEG DEL ***** 02/10/06 ************************************
     F**********WDEHL4  IF  E           K        DISK
      *** END DEL ***** 02/10/06 ************************************
     FWMEML     IF   E           K DISK
     F*** BEGIN ADD ***  11/14/02  ************************************
     FWMPXL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F*** END ADD *****  11/14/02  ************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SSY01)
     F*
     F*** BEG CHG *****  02/09/06  **********MLD***********************

     D*************       QCA     1  17 55               CMD EXEC/QCAEXEC
     D QCA             S             55    DIM(19) CTDATA PERRCD(1)             CMD EXEC/QCAEXEC
     F*** END CHG *****  02/09/06  **********MLD***********************
     D*
     D*
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXPGM           C                   CONST('FIXWDEHP11')
     D*
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
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
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
     C                   KFLD                    XXEFDT
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
     C*
     C     EHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EHKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  WORK COMP CONTROL FILE
     C*
     C     WCKY          KLIST
     C                   KFLD                    XCKEY
     C*
     C*** BEGIN ADD ***  11/14/02  ************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XPST              2
     C*** END ADD *****  11/14/02  ************************************
      *
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
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQFAF             1
      *
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
     C*
     C*  IF EMPLOYER NUMBER IS NOT BLANK, SET UP FOR SINGLE RECORD PROCESS
     C*
     C     QQEMP#        IFNE      *BLANK
     C     QQEMP#        ANDNE     *ZEROS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C*
     C                   MOVE      *ON           *IN20
     C                   ENDIF
     C                   TIME                    DSDTTM
     C*** BEG ADD *MLD* 02/09/06 **********************************
     C*
     C*  OVERRIDE DATABASE FILE WDELP TO WAELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(18)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *MLD* 02/09/06 **********************************
     C*
     C*  OVERRIDE DATABASE FILE WDELL TO WAELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL1 TO WAEPL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
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
     C*
     C*  BEGIN PROCESSING HISTORY RECORDS
     C*
     C     *IN20         IFEQ      *OFF
     C                   READ      PEH01                                  50
     C                   ELSE
     C*
     C     QQFAF         IFEQ      'R'
     C     EHKY1         CHAIN     PEH01                              50
     C                   ELSE
     C     EHKY2         CHAIN     PEH01                              50
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C     EHPGM         IFEQ      'SRINPR'
     C*
     C*  OPEN THE OVERRIDE FILES
     C*
     C                   OPEN      WAELL
     C                   OPEN      WAEPL
     C                   OPEN      WADSL
     C                   OPEN      WAILL
     C                   OPEN      WAMPL
     C*
     C*
     C*  ESTABLISH KEY FIELDS
     C*
     C                   Z-ADD     EHCO#         XXCO#
     C                   Z-ADD     EHFND         XXFND
     C                   Z-ADD     EHFYR         XXFYR
     C                   Z-ADD     EHEMP#        XXEMP#
     C                   Z-ADD     EHDIV         XXDIV
     C*
     C*  LOAD OVERRIDE FILES
     C*
     C     ELKEY         CHAIN(N)  PEL01                              60
     C*
     C                   EXSR      LDOVRF
     C*
     C*  DETERMINE IF SPREADING DISCOUNTS
     C*
     C                   MOVE      ' '           Q1SPRD            1
     C*
     C     WCFL02        IFEQ      'Y'
     C*
     C*  GET DISCOUNT VALUE FOR THIS EMPLOYER, DIV = 0
     C*
     C                   Z-ADD     0             XXDIV
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
     C*
     C                   Z-ADD     0             XXDIV
     C     ELKY1         CHAIN     PEL03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C                   EXSR      LDOVRF
     C     ELKY1         READE     PEL03                                  60
     C                   ENDDO
     C*
     C*  IF WE ARE SPREADING THE DISCOUNT, RUN THE BILLING CALC FOR
     C*  EACH DIVISION TO GET THE INFORCED STANDARD PREMIUM INTO EACH
     C*  DIVISION'S OVERRIDE RECORD.
     C*
     C                   Z-ADD     0             XXDIV
     C     ELKY1         CHAIN     PEL03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
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
     C                   CALL      'W8010R'
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
     C*
     C     ELKY1         READE     PEL03                                  60
     C                   ENDDO
     C*
     C                   END                                                    Q1SPRD = Y
     C*
     C* RUN THE BILL CALCULATION TO GET THE INFORCED PREMIUM
     C*
     C*** BEG ADD ***** 02/10/06 *********************************
     C*
     C*  ESTABLISH SAVE FIELDS
     C*
     C                   Z-ADD     EHCO#         SVCO#             3 0
     C                   Z-ADD     EHFND         SVFND             3 0
     C                   Z-ADD     EHFYR         SVFYR             3 0
     C                   Z-ADD     EHEMP#        SVEMP#            9 0
     C                   Z-ADD     EHDIV         SVDIV             5 0
     C*** END ADD ***** 02/10/06 *********************************
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C     EHPGM         IFEQ      'SRINPR'
     C*
      *** BEG ADD ****** 02/10/06 ************************************
     C     EHCO#         IFNE      SVCO#
     C     EHFND         ORNE      SVFND
     C     EHEMP#        ORNE      SVEMP#
     C     EHDIV         ORNE      SVDIV
     C     EHFYR         ORNE      SVFYR
     C                   MOVE(P)   SVCO#         ##CO#             3
     C                   MOVE(P)   SVFND         ##FND             3
     C                   MOVE(P)   SVEMP#        ##EMP#            9
     C                   MOVE(P)   SVDIV         ##DIV             5
     C                   MOVE(P)   SVFYR         ##FYR             3
     C                   CALL      XXPGM
     C                   PARM      'Y'           ##FLG             1
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##FYR
     C                   Z-ADD     EHCO#         SVCO#
     C                   Z-ADD     EHFND         SVFND
     C                   Z-ADD     EHFYR         SVFYR
     C                   Z-ADD     EHEMP#        SVEMP#
     C                   Z-ADD     EHDIV         SVDIV
     C                   ENDIF
      *** END ADD ****** 02/10/06 ************************************
     C*
     C                   Z-ADD     EHDIV         XXDIV
     C                   Z-ADD     EHFYR         XXFYR
     C     ELKEY         CHAIN(N)  PEL01                              60
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
     C                   CALL      'W8010R'
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
     C*
     C*  LOAD INFORCED FIELDS FROM THE OVERRIDE DETAIL RECORD
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   MOVE      *BLANK        SVCX              1
     C                   DO        5
     C     CD(XXX)       IFLE      ELPTDT
     C     CD(XXX)       ANDNE     0
     C     ED(XXX)       ANDNE     0
     C                   MOVE      'Y'           SVCX
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C     SVCX          IFEQ      'Y'
     C     EHFAF         ANDEQ     'A'
     C*
     C                   Z-ADD     0             EHIMPR
     C                   Z-ADD     0             EHISPR
     C                   Z-ADD     0             EHINMP
     C                   Z-ADD     0             EHIAPR
     C*** BEGIN ADD ***  02/09/06  ************************************
     C                   Z-ADD     0             EHISBJ
     C                   Z-ADD     0             EHIMPM
     C*** END ADD *****  02/09/06  ************************************
     C                   MOVE      *BLANK        EHIMNP
     C                   Z-ADD     0             EHIDC1
     C                   Z-ADD     0             EHIDC2
     C                   Z-ADD     0             EHIDC3
     C                   Z-ADD     0             EHIXP1
     C                   Z-ADD     0             EHIXP2
     C                   Z-ADD     0             EHIXP3
     C                   Z-ADD     0             EHIIN1
     C                   Z-ADD     0             EHIIN2
     C                   Z-ADD     0             EHIDCT
     C***  Begin Add  ***  05/04/05  **************************************
     C                   Z-ADD     0             EHIDD2
     C***  End   Add  ***  05/04/05  **************************************
     C                   Z-ADD     0             EHIDCM
     C                   Z-ADD     0             EHIDCS
     C                   Z-ADD     0             EHIDCR
     C                   Z-ADD     0             EHIFRS
     C                   Z-ADD     0             EHIDV2
     C                   Z-ADD     0             EHIDC
     C                   Z-ADD     0             EHIWAV
     C                   Z-ADD     0             EHIADL
     C*** BEGIN ADD ***  01/17/03  ************************************
     C                   Z-ADD     0             EHITER
     C*** END ADD *****  01/17/03  ************************************
     C*** BEGIN ADD ***  01/07/05  ************************************
     C                   Z-ADD     0             EHIDTR
     C*** END ADD *****  01/07/05  ************************************
     C*** BEGIN ADD ***  09/01/04  ************************************
     C                   Z-ADD     0             EHIVLC
     C*** END ADD *****  09/01/04  ************************************
     C*** BEGIN ADD ***  01/29/03  ************************************
     C                   Z-ADD     0             EHIRP1
     C                   Z-ADD     0             EHIRP2
     C                   Z-ADD     0             EHIRP3
     C*** END ADD *****  01/29/03  ************************************
     C*
     C                   ELSE
     C                   MOVE      ELMAPR        EHIMPR
     C*** BEGIN ADD ***  02/09/06  ************************************
     C                   MOVE      ELESBJ        EHISBJ
     C                   MOVE      ELEMPM        EHIMPM
     C*** END ADD *****  02/09/06  ************************************
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
     C***  Begin Add  ***  05/04/05  **************************************
     C                   MOVE      ELEDD2        EHIDD2
     C***  End   Add  ***  05/04/05  **************************************
     C                   MOVE      ELEDCM        EHIDCM
     C                   MOVE      ELEDCS        EHIDCS
     C                   MOVE      ELEDCR        EHIDCR
     C                   MOVE      ELEFRS        EHIFRS
     C                   MOVE      ELEDV2        EHIDV2
     C                   MOVE      ELEDC         EHIDC
     C                   Z-ADD     ELEWAV        EHIWAV
     C*** BEGIN ADD ***  01/17/03  ************************************
     C                   Z-ADD     ELETER        EHITER
     C*** END ADD *****  01/17/03  ************************************
     C*** BEGIN ADD ***  01/07/05  ************************************
     C                   Z-ADD     ELEDTR        EHIDTR
     C*** END ADD *****  01/07/05  ************************************
     C*** BEGIN ADD ***  09/01/04  ************************************
     C                   Z-ADD     ELVLCA        EHIVLC
     C*** END ADD *****  09/01/04  ************************************
     C*** BEGIN ADD ***  01/29/03  ************************************
     C                   Z-ADD     ELRPE1        EHIRP1
     C                   Z-ADD     ELRPE2        EHIRP2
     C                   Z-ADD     ELRPE3        EHIRP3
     C*** END ADD *****  01/29/03  ************************************
     C                   Z-ADD     0             XXADLD           11 2
     C     EPKY1         CHAIN     PEP02                              60
     C     *IN60         DOWEQ     *OFF
     C*** BEGIN CHG ***  11/14/02  ************************************
     C*********            ADD  EPADLD    XXADLD
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
     C                   ADD       EPADLD        XXADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C*** END CHG *****  11/14/02  ************************************
     C     EPKY1         READE     PEP02                                  60
     C                   ENDDO
     C                   Z-ADD     XXADLD        EHIADL
     C                   ENDIF                                                  SVCX IFEQ
      *** BEG DEL ****** 02/10/06 ************************************
     C*
     C* REMOVE LOGIC TO POPULATE CHANGE FIELDS BECAUSE IT RELIES ON TRDT = 0
     C* USE FIXWDEHP INSTEAD
     C*
     C***********ELKEY     SETGTPEQ01
     C***********ELKEY     REDPEPEQ01                    60
     C************
     C************IN60     IFEQ *OFF
     C************
     C***WRITTEN*PREMIUM FIELDS
     C************
     C***********EHWMPR    SUB  EQWMPR    EHXMPR
     C***********EHWSPR    SUB  EQWSPR    EHXSPR
     C***********EHWNMP    SUB  EQWNMP    EHXNMP
     C***********EHWAPR    SUB  EQWAPR    EHXAPR
     C****BEGIN*ADD ***  02/09/06  ************************************
     C***********EHWSBJ    SUB  EQWSBJ    EHXSBJ
     C***********EHWMPM    SUB  EQWMPM    EHXMPM
     C****END*ADD******  02/09/06  ************************************
     C***********EHWDC1    SUB  EQWDC1    EHXDC1
     C***********EHWDC2    SUB  EQWDC2    EHXDC2
     C***********EHWDC3    SUB  EQWDC3    EHXDC3
     C***********EHWXP1    SUB  EQWXP1    EHXXP1
     C***********EHWXP2    SUB  EQWXP2    EHXXP2
     C***********EHWXP3    SUB  EQWXP3    EHXXP3
     C***********EHWIN1    SUB  EQWIN1    EHXIN1
     C***********EHWIN2    SUB  EQWIN2    EHXIN2
     C***********EHWDCT    SUB  EQWDCT    EHXDCT
     C*****Begin*Add  ***  05/04/05  **************************************
     C***********EHWDD2    SUB  EQWDD2    EHXDD2
     C*****End***Add  ***  05/04/05  **************************************
     C***********EHWDCM    SUB  EQWDCM    EHXDCM
     C***********EHWDCS    SUB  EQWDCS    EHXDCS
     C***********EHWDCR    SUB  EQWDCR    EHXDCR
     C***********EHWFRS    SUB  EQWFRS    EHXFRS
     C***********EHWDV2    SUB  EQWDV2    EHXDV2
     C***********EHWWAV    SUB  EQWWAV    EHXWAV
     C***********EHWADL    SUB  EQWADL    EHXADL
     C****BEGIN*ADD ***  01/17/03  ************************************
     C***********EHWTER    SUB  EQWTER    EHXTER
     C****END*ADD******  01/17/03  ************************************
     C****BEGIN*ADD ***  01/07/05  ************************************
     C***********EHWDTR    SUB  EQWDTR    EHXDTR
     C****END*ADD******  01/07/05  ************************************
     C****BEGIN*ADD ***  01/29/03  ************************************
     C***********EHWRP1    SUB  EQWRP1    EHXRP1
     C***********EHWRP2    SUB  EQWRP2    EHXRP2
     C***********EHWRP3    SUB  EQWRP3    EHXRP3
     C****END*ADD******  01/29/03  ************************************
     C****BEG*ADD******  09/01/04  ************************************
     C***********EHWVLC    SUB  EQWVLC    EHXVLC
     C****END*ADD******  09/01/04  ************************************
     C************
     C***INFORCED*PREMIUM FIELDS
     C************
     C***********EHIMPR    SUB  EQIMPR    EHJMPR
     C***********EHISPR    SUB  EQISPR    EHJSPR
     C****BEGIN*ADD ***  02/09/06  ************************************
     C***********EHISBJ    SUB  EQISBJ    EHJSBJ
     C***********EHIMPM    SUB  EQIMPM    EHJMPM
     C****END*ADD******  02/09/06  ************************************
     C***********EHINMP    SUB  EQINMP    EHJNMP
     C***********EHIAPR    SUB  EQIAPR    EHJAPR
     C***********EHIDC     SUB  EQIDC     EHJDC
     C***********EHIDC1    SUB  EQIDC1    EHJDC1
     C***********EHIDC2    SUB  EQIDC2    EHJDC2
     C***********EHIDC3    SUB  EQIDC3    EHJDC3
     C***********EHIXP1    SUB  EQIXP1    EHJXP1
     C***********EHIXP2    SUB  EQIXP2    EHJXP2
     C***********EHIXP3    SUB  EQIXP3    EHJXP3
     C***********EHIIN1    SUB  EQIIN1    EHJIN1
     C***********EHIIN2    SUB  EQIIN2    EHJIN2
     C***********EHIDCT    SUB  EQIDCT    EHJDCT
     C*****Begin*Add  ***  05/04/05  **************************************
     C***********EHIDD2    SUB  EQIDD2    EHJDD2
     C*****End***Add  ***  05/04/05  **************************************
     C***********EHIDCM    SUB  EQIDCM    EHJDCM
     C***********EHIDCS    SUB  EQIDCS    EHJDCS
     C***********EHIDCR    SUB  EQIDCR    EHJDCR
     C***********EHIFRS    SUB  EQIFRS    EHJFRS
     C***********EHIDV2    SUB  EQIDV2    EHJDV2
     C***********EHIWAV    SUB  EQIWAV    EHJWAV
     C***********EHIADL    SUB  EQIADL    EHJADL
     C****BEGIN*ADD ***  01/17/03  ************************************
     C***********EHITER    SUB  EQITER    EHJTER
     C****END*ADD******  01/17/03  ************************************
     C****BEGIN*ADD ***  01/07/05  ************************************
     C***********EHIDTR    SUB  EQIDTR    EHJDTR
     C****END*ADD******  01/07/05  ************************************
     C****BEG*ADD******  09/01/04  ************************************
     C***********EHIVLC    SUB  EQIVLC    EHJVLC
     C****END*ADD******  09/01/04  ************************************
     C****BEGIN*ADD ***  01/29/03  ************************************
     C***********EHIRP1    SUB  EQIRP1    EHJRP1
     C***********EHIRP2    SUB  EQIRP2    EHJRP2
     C***********EHIRP3    SUB  EQIRP3    EHJRP3
     C****END*ADD******  01/29/03  ************************************
     C************         ELSE
     C************
     C***WRITTEN*PREMIUM FIELDS
     C************
     C************         Z-ADDEHWMPR    EHXMPR
     C************         Z-ADDEHWSPR    EHXSPR
     C************         Z-ADDEHWNMP    EHXNMP
     C************         Z-ADDEHWAPR    EHXAPR
     C****BEGIN*ADD ***  02/09/06  ************************************
     C************         Z-ADDEHWSBJ    EHXSBJ
     C************         Z-ADDEHWMPM    EHXMPM
     C****END*ADD******  02/09/06  ************************************
     C************         Z-ADDEHWDC1    EHXDC1
     C************         Z-ADDEHWDC2    EHXDC2
     C************         Z-ADDEHWDC3    EHXDC3
     C************         Z-ADDEHWXP1    EHXXP1
     C************         Z-ADDEHWXP2    EHXXP2
     C************         Z-ADDEHWXP3    EHXXP3
     C************         Z-ADDEHWIN1    EHXIN1
     C************         Z-ADDEHWIN2    EHXIN2
     C************         Z-ADDEHWDCT    EHXDCT
     C*****Begin*Add  ***  05/04/05  **************************************
     C************         Z-ADDEHWDD2    EHXDD2
     C*****End***Add  ***  05/04/05  **************************************
     C************         Z-ADDEHWDCM    EHXDCM
     C************         Z-ADDEHWDCS    EHXDCS
     C************         Z-ADDEHWDCR    EHXDCR
     C************         Z-ADDEHWFRS    EHXFRS
     C************         Z-ADDEHWDV2    EHXDV2
     C************         Z-ADDEHWWAV    EHXWAV
     C************         Z-ADDEHWADL    EHXADL
     C****BEGIN*ADD ***  01/17/03  ************************************
     C************         Z-ADDEHWTER    EHXTER
     C****END*ADD******  01/17/03  ************************************
     C****BEGIN*ADD ***  01/07/05  ************************************
     C************         Z-ADDEHWDTR    EHXDTR
     C****END*ADD******  01/07/05  ************************************
     C****BEGIN*ADD ***  01/29/03  ************************************
     C************         Z-ADDEHWRP1    EHXRP1
     C************         Z-ADDEHWRP2    EHXRP2
     C************         Z-ADDEHWRP3    EHXRP3
     C****END*ADD******  01/29/03  ************************************
     C****BEG*ADD******  09/01/04  ************************************
     C************         Z-ADDEHWVLC    EHXVLC
     C****END*ADD******  09/01/04  ************************************
     C************
     C***INFORCED*PREMIUM FIELDS
     C************
     C************         Z-ADDEHIMPR    EHJMPR
     C************         Z-ADDEHISPR    EHJSPR
     C************         Z-ADDEHINMP    EHJNMP
     C************         Z-ADDEHIAPR    EHJAPR
     C****BEGIN*ADD ***  02/09/06  ************************************
     C************         Z-ADDEHISBJ    EHJSBJ
     C************         Z-ADDEHIMPM    EHJMPM
     C****END*ADD******  02/09/06  ************************************
     C************         Z-ADDEHIDC1    EHJDC1
     C************         Z-ADDEHIDC     EHJDC
     C************         Z-ADDEHIDC2    EHJDC2
     C************         Z-ADDEHIDC3    EHJDC3
     C************         Z-ADDEHIXP1    EHJXP1
     C************         Z-ADDEHIXP2    EHJXP2
     C************         Z-ADDEHIXP3    EHJXP3
     C************         Z-ADDEHIIN1    EHJIN1
     C************         Z-ADDEHIIN2    EHJIN2
     C************         Z-ADDEHIDCT    EHJDCT
     C*****Begin*Add  ***  05/04/05  **************************************
     C************         Z-ADDEHIDD2    EHJDD2
     C*****End***Add  ***  05/04/05  **************************************
     C************         Z-ADDEHIDCM    EHJDCM
     C************         Z-ADDEHIDCS    EHJDCS
     C************         Z-ADDEHIDCR    EHJDCR
     C************         Z-ADDEHIFRS    EHJFRS
     C************         Z-ADDEHIDV2    EHJDV2
     C************         Z-ADDEHIWAV    EHJWAV
     C************         Z-ADDEHIADL    EHJADL
     C****BEGIN*ADD ***  01/17/03  ************************************
     C************         Z-ADDEHITER    EHJTER
     C****END*ADD******  01/17/03  ************************************
     C****BEGIN*ADD ***  01/07/05  ************************************
     C************         Z-ADDEHIDTR    EHJDTR
     C****END*ADD******  01/07/05  ************************************
     C****BEGIN*ADD ***  09/01/04  ************************************
     C************         Z-ADDEHIVLC    EHJVLC
     C****END*ADD******  09/01/04  ************************************
     C****BEGIN*ADD ***  01/29/03  ************************************
     C************         Z-ADDEHIRP1    EHJRP1
     C************         Z-ADDEHIRP2    EHJRP2
     C************         Z-ADDEHIRP3    EHJRP3
     C****END*ADD******  01/29/03  ************************************
     C************         ENDIF
      *** END DEL ****** 02/10/06 ************************************
     C                   UPDATE    PEH01
     C*
     C*  LOAD THE INFORCED PREMIUM INTO EMPLOYER DETAIL
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C                   Z-ADD     EHIAPR        ELINMP
     C  N60              UPDATE    PEL01
     C*
     C                   ENDIF
     C*
     C     *IN20         IFEQ      *OFF
     C     EHKY3         READE     PEH01                                  50
     C                   ELSE
     C     EHKY2         READE     PEH01                                  50
     C                   ENDIF
      *
      *** BEG ADD ****** 02/10/06 ************************************
     C     *IN50         IFEQ      *ON
     C                   MOVE(P)   SVCO#         ##CO#             3
     C                   MOVE(P)   SVFND         ##FND             3
     C                   MOVE(P)   SVEMP#        ##EMP#            9
     C                   MOVE(P)   SVDIV         ##DIV             5
     C                   MOVE(P)   SVFYR         ##FYR             3
     C                   CALL      XXPGM
     C                   PARM      'Y'           ##FLG             1
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##FYR
     C                   ENDIF
      *** END ADD ****** 02/10/06 ************************************
     C*
     C                   ENDDO
     C*
     C*  GET THE NEXT RECORD IF PROCESSING ALL RECORDS
     C*
     C     *IN20         IFEQ      *OFF
     C     EHKY3         SETGT     PEH01
     C                   READ      PEH01                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C*
     C*  CLOSE THE OVERRIDE FILES SO THEY CAN BE CLEARED
     C*
     C                   CLOSE     WAELL
     C                   CLOSE     WAEPL
     C                   CLOSE     WADSL
     C                   CLOSE     WAILL
     C                   CLOSE     WAMPL
     C*
     C*  CLEAR OVERRIDE FILE WAELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*
     C*  CLEAR OVERRIDE FILE WAEPP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH           15 5
     C*
     C*  CLEAR OVERRIDE FILE WADSP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(11)
     C                   PARM                    LENGTH           15 5
     C*
     C*  CLEAR OVERRIDE FILE WAILP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(14)
     C                   PARM                    LENGTH           15 5
     C*
     C*  CLEAR OVERRIDE FILE WAMPP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(17)
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   READ      PEH01                                  50
     C*
     C                   ENDIF                                                  EHUSER IFEQ
     C*
     C                   ENDDO
      *** BEG ADD *MLD* 02/09/06 *************************************
     C*
     C*  DELETE OVERRIDE FOR WDELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(19)
     C                   PARM                    LENGTH           15 5
      *** END ADD ***** 02/09/06 *************************************
     C*
     C*  DELETE OVERRIDE FOR WDELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WMEPL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WMEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
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
     C*  CALCULATE THE ANNIVERSARY RATING PERIOD START DATE.
     C*  IF IT FALLS OUTSIDE THE POLICY PERIOD, SET IT TO 999999.
     C*
     C                   Z-ADD     ELANDT        XXANDT
     C                   ADD       10000         XXANDT
     C*
     C     XXANDT        IFLT      ELPFDT
     C     XXANDT        ORGT      ELPTDT
     C                   Z-ADD     9999999       XXANDT
     C                   ENDIF
     C*
     C*  LOAD THE PAYROLL OVERRIDE RECORDS
     C*
     C     EPKY1         CHAIN     PEP02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     EPKY1         CHAIN     PEP01                              61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C*  FOR AUDITED INFORCED PREMIUM, LOAD THE AUDIT PAYROLL INTO
     C*  THE ESTIMATED PAYROLL FIELD.
     C*
     C     EHFAF         IFEQ      'A'
     C                   Z-ADD     EPAAPR        EPEAPR
     C                   ENDIF
     C*
     C*  IF THE PAYROLL RECORD EFFECTIVE DATE IS NOT THE SAME AS
     C*  THE POLICY START DATE OR THE ANNIVERSARY RATING DATE,
     C*  THE PAYROLL AMOUNT MUST BE ANNUALIZED TO COVER THE POLICY
     C*  PERIOD.  IF THE POLICY IS ANNIVERSARY RATED, WE MUST ALSO
     C*  WRITE A RECORD FOR THE FIRST PERIOD IF THE ORIGINAL RECORD
     C*  BECAME EFFECTIVE AFTER THE ANNIVERSARY RATING DATE.
     C*
     C     EPEFDT        IFNE      ELPFDT
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
     C                   Z-ADD     ELPTDT        SRDTI
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
     C                   Z-ADD     ELPFDT        SRDTI
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
     C                   Z-ADD     ELPFDT        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      ELPFDT
     C                   Z-ADD     XXANDT        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      ELPFDT
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
     C     EPEFDT        IFGT      XXANDT
     C*
     C     PRDAYS        IFNE      *ZERO
     C     RDAYS2        DIV       PRDAYS        XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C                   Z-ADD     EPEAPR        XXEAPR
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C                   Z-ADD     XXANDT        EPEFDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EPTIME
     C                   WRITE     PEP02
     C                   Z-ADD     XXEAPR        EPEAPR
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
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C                   Z-ADD     ELPFDT        EPEFDT
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
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C                   Z-ADD     ELPFDT        EPEFDT
     C*
     C                   ENDIF                                                  XXANDT <> 999999
     C*
     C                   ELSE
     C*
     C     EHFAF         IFEQ      'A'
     C     ELPROR        ANDNE     *ZERO
     C     EPEAPR        DIV(H)    ELPROR        XXWK90
     C                   Z-ADD     XXWK90        EPEAPR
     C                   ENDIF                                                  EHFAF IFEQ
     C*
     C                   ENDIF                                                  EPEFDT <> ELPFDT
     C*                                               AND  EPEFDT <> XXANDT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EPTIME
     C*
     C                   WRITE     PEP02
     C     EPKY1         READE     PEP01                                  61
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  WRITE THE DETAIL RECORD IF IT IS NOT ALREADY WRITTEN
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     ELKEY         CHAIN(N)  PEL01                              61
     C*
     C     *IN61         IFEQ      *OFF
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
     C*  ANNUALIZE THE OVERRIDE DETAIL RECORD.  MAKE THE MOST RECENT
     C*  EXPERIENCE MODIFIER THE ONLY EXPERIENCE MODIFIER.  IF THERE
     C*  IS A SECOND INCREASED LIMIT, USE FOR THE ONLY INCREASED LIMIT.
     C*  USE THE POLICY START DATE FOR ALL PREMIUM FACTOR EFFECTIVE
     C*  DATES.  SET ALL PREMIUM EXPIRATION DATES TO ZERO.
     C*
      *** BEG CHG ***** 02/10/06 *************************************
     C********** ELEXM3    IFNE *ZERO
     C**********           Z-ADDELEXM3    ELEXM1
     C**********           ELSE
     C**********
     C********** ELEXM2    IFNE *ZERO
     C**********           Z-ADDELEXM2    ELEXM1
     C**********           ENDIF                           ELEXM2 <> 0
     C*
     C     EHEXM3        IFNE      *ZERO
     C                   Z-ADD     EHEXM3        ELEXM1
     C                   ELSE
     C*
     C     EHEXM2        IFNE      *ZERO
     C                   Z-ADD     EHEXM2        ELEXM1
     C                   ENDIF                                                  ELEXM2 <> 0
      *** END CHG ***** 02/10/06 *************************************
     C*
     C                   ENDIF                                                  ELEXM3 <> 0
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
     C                   ENDIF
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
OVRDBF FILE(WDELP) TOFILE(QTEMP/WAELP)                               02/09/06MLD
DLTOVR FILE(WDELP)                                                   02/09/06MLD
