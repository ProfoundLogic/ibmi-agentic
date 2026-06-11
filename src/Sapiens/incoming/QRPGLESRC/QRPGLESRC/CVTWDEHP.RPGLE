     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CVTWDEHP  - LOAD WDEHP RECORDS BASED ON EMPLOYER DETAIL
     H*****************************************************************
     H*
     H*  CREATED:  07/01/99  C:35
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*****************************************************************
     FWDEHLT1   UF   E           K DISK
     FWDEHLT2   UF   E           K DISK
     F                                     RENAME(PEH01:PEH02)
     FWDEHL     O    E           K DISK
     F                                     RENAME(PEH01:PEH03)
     FWDELL     IF   E           K DISK
     FWMEAL4    IF   E           K DISK
     FWMFNL     IF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      * KEY LISTS FOR THE EMPLOYER PREMIUM HISTORY FILE
      *
     C     EHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C     EHKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFAF
      *
     C     EHKY3         KLIST
     C                   KFLD                    XXCO#
      *
     C     EHKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * KEY LIST FOR THE A/R FILE
      *
     C     EAKY          KLIST
      **** BEGIN CHG **** 10/06/98 *********************************************
     C******************** KFLD           ELCO#
     C******************** KFLD           ELFND
     C******************** KFLD           ELEMP#
     C******************** KFLD           ELDIV
     C******************** KFLD           ELFYR
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
     C                   KFLD                    EHFYR
      **** END   CHG **** 10/06/98 *********************************************
     C                   KFLD                    XXARTY
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     *LIKE         DEFINE    EAARTY        XXARTY
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
      *
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
      *
      *  BEGIN MAIN LOOP
      *
     C     QQFND         IFEQ      '000'
     C     ELKY          CHAIN     PEL01                              60
     C                   ELSE
     C     ELKY1         CHAIN     PEL01                              60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ELFAF         IFNE      *BLANK
      *
     C     EHKY1         CHAIN     PEH01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   EXSR      SREREC
      *
     C                   ELSE
      *
     C                   EXCEPT    PEH01E
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  ELFAF IFNE *BLANK
      *
     C     QQFND         IFEQ      '000'
     C     ELKY          READE     PEL01                                  60
     C                   ELSE
     C     ELKY1         READE     PEL01                                  60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     QQFND         IFEQ      '000'
     C     FNKY          CHAIN     PFN01                              60
     C                   ELSE
     C     FNKY1         CHAIN     PFN01                              60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      FNCO#         QQCO#
     C                   MOVE      FNFND         QQFND
      *
     C                   CALL      'SRINPRC'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      *ZEROS        QQEMP#            9
     C                   PARM      '00000'       QQDIV             5
     C                   PARM      '000'         QQFYR             3
     C                   PARM      '00000000'    QQDATE            8
     C                   PARM      'E'           QQFAF             1
      *
     C     QQFND         IFEQ      '000'
     C     FNKY          READE     PFN01                                  60
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     QQFND         IFEQ      '000'
     C     ELKY          CHAIN     PEL01                              60
     C                   ELSE
     C     ELKY1         CHAIN     PEL01                              60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ELFAF         IFEQ      'A'
      *
     C     EHKY2         CHAIN     PEH02                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   EXSR      SRAREC
      *
     C                   ELSE
      *
     C                   EXCEPT    PEH02E
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  ELFAF IFEQ 'A'
      *
     C     QQFND         IFEQ      '000'
     C     ELKY          READE     PEL01                                  60
     C                   ELSE
     C     ELKY1         READE     PEL01                                  60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     QQFND         IFEQ      '000'
     C     FNKY          CHAIN     PFN01                              60
     C                   ELSE
     C     FNKY1         CHAIN     PFN01                              60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      FNCO#         QQCO#
     C                   MOVE      FNFND         QQFND
      *
     C                   CALL      'SRINPRC'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND
     C                   PARM      *ZEROS        QQEMP#            9
     C                   PARM      '00000'       QQDIV             5
     C                   PARM      '000'         QQFYR             3
     C                   PARM      '00000000'    QQDATE            8
     C                   PARM      'A'           QQFAF             1
      *
     C     QQFND         IFEQ      '000'
     C     FNKY          READE     PFN01                                  60
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     QQFND         IFEQ      '000'
     C     EHKY3         CHAIN     PEH01                              60
     C                   ELSE
     C     EHKY4         CHAIN     PEH01                              60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C     *IN60         DOWEQ     *OFF
      *
      ********************************************************
      *
      * NEED TO CHANGE THIS SECTION TO SET APPROPRIATE TRANSACTION AND
      * POSTING DATES
      *
     C     EHIUSR        IFEQ      'CONVERT'
      *
     C     EHFAF         IFEQ      'A'
      *
     C                   MOVE      'D'           XXARTY
      *
     C     EAKY          CHAIN     PEA01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     EATRDT        EHTRDT
      *
     C                   ELSE
      *
     C                   Z-ADD     EHEFDT        EHTRDT
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ELSE
      *
     C                   Z-ADD     EHEFDT        EHTRDT
      *
     C                   ENDIF                                                  EHFAF IFEQ 'A'
      *
     C                   UPDATE    PEH01
      *
     C                   ENDIF                                                  EHIUSR IFEQ 'CONVERT
      *
     C     QQFND         IFEQ      '000'
     C     EHKY3         READE     PEH01                                  60
     C                   ELSE
     C     EHKY4         READE     PEH01                                  60
     C                   ENDIF                                                  QQFND IFEQ '000'
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      '1'           *INLR
      *
     C     SREREC        BEGSR
      *
     C                   CLEAR                   PEH03
      *
     C                   MOVE      ELCO#         EHCO#
     C                   MOVE      ELFND         EHFND
     C                   MOVE      ELFYR         EHFYR
     C                   MOVE      ELEMP#        EHEMP#
     C                   MOVE      ELDIV         EHDIV
     C                   MOVE      ELPFDT        EHEFDT
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
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
      *
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
     C                   MOVEL(P)  'FIX'         EHIUSR
     C                   MOVE      EHDT          EHIDT
      *
     C                   WRITE     PEH03
      *
     C                   ENDSR                                                  SREREC
      *
      **************************************************************************
      *
     C     SRAREC        BEGSR
      *
     C                   CLEAR                   PEH03
      *
     C                   MOVE      ELCO#         EHCO#
     C                   MOVE      ELFND         EHFND
     C                   MOVE      ELFYR         EHFYR
     C                   MOVE      ELEMP#        EHEMP#
     C                   MOVE      ELDIV         EHDIV
     C                   MOVE      'D'           XXARTY
      *
     C     EAKY          CHAIN     PEA01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     EAPODT        EHEFDT
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHEFDT
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
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
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
      *
     C                   MOVEL(P)  'CONVERT'     EHIUSR
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
      *
     C                   WRITE     PEH03
      *
     C                   ENDSR                                                  SRAREC
      *
     OPEH01     E            PEH01E
     OPEH02     E            PEH02E
