     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRRVEH - REVERSE OUT WDEHP RECORDS FOR CHG IN POLICY DATES
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM WILL REVERSE OUT EXISTING WDEHP RECORDS WHEN   *
     H*   A CHANGE IN THE POLICY DATES OCCURS.                        *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W8011R                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:57                                              *
     H* CREATED: 12/15/97               PROJECT: CM036                *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/31/20 C:65  SUPPORT: Phw#87085 (Sunz)                     *
     H*                 ADD LOGIC FOR THE NEW FIELDS ADDED TO WDEHP   *
     H*                 SINCE 2008 THAT THE CEU USES.                 *
     H*                                                               *
     H*  08/16/08 C:136 SUPPORT: WA093 64885                          *
     H*                 ADD LOGIC FOR THE NEW FIELDS ADDED TO WDEHP   *
     H*                 SINCE 2002 THAT THE CEU USES.                 *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT SUPP#33855                              *
     H*               LOAD ANNUAL EXPENSE CONSTANT INTO EMPLR HISTORY *
     H*                                                               *
     H*  01/19/98 C:65 PROJECT: WC633                                 *
     H*                ADD 2ND INC LIMITS TO EMP HISTORY FILE.        *
     H*                                                               *
     H*****************************************************************
     F*
     FWDEHL9    UF A E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  EMPLOYER DETAIL HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPFDT
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHPFDT        XXPFDT
     C     *LIKE         DEFINE    EHPFDT        XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQPFDT            7
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQPFDT        XXPFDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*  BEGIN PROCESSING HISTORY RECORDS
     C*
     C     EHKY          SETLL     PEH01
     C     EHKY          READE     PEH01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C*  ONLY PROCESS RECORDS THAT MATCH THE OLD POLICY EFFECTIVE
     C*  DATE AND DO NOT HAVE A REWRITTEN FLAG SET
     C*
     C     EHRWFL        IFEQ      'Y'
     C                   GOTO      NEXTEH
     C                   ENDIF
     C*
     C*  UPDATE THE RECORD'S REWRITTEN FLAG, AND REWRITTEN DATE
     C*
     C                   MOVE      'Y'           EHRWFL                         REWRITE FLAG
     C                   MOVE      XXDATE        EHRWDT                         REWRITE DATE
     C                   UPDATE    PEH01
     C*
     C*  NEGATE THE AMOUNTS
     C*
     C                   MOVE      XXDATE        EHTRDT                         TRANS DT
     C                   MULT      -1            EHWMPR                         WRT MAN PREM
     C                   MULT      -1            EHXMPR                         WRT CHG MAN PREM
     C                   MULT      -1            EHIMPR                         INF MAN PREM
     C                   MULT      -1            EHJMPR                         INF CHG MAN PREM
     C                   MULT      -1            EHWSPR                         WRT STD PREM
     C                   MULT      -1            EHXSPR                         WRT CHG STD PREM
     C                   MULT      -1            EHISPR                         INF STD PREM
     C                   MULT      -1            EHJSPR                         INF CHG STD PREM
     C                   MULT      -1            EHWNMP                         WRT NORM PREM
     C                   MULT      -1            EHXNMP                         WRT CHG NORM PRM
     C                   MULT      -1            EHINMP                         INF NORM PREM
     C                   MULT      -1            EHJNMP                         INF CHG NORM PRM
     C                   MULT      -1            EHWAPR                         WRT ANNL PREM
     C                   MULT      -1            EHXAPR                         WRT CHG ANNL PRM
     C                   MULT      -1            EHIAPR                         INF ANNL PREM
     C                   MULT      -1            EHJAPR                         INF CHG ANNL PRM
     C                   MULT      -1            EHPRTD                         EXP CONST
     C*** BEGIN ADD ***  11/14/02  ************************************
     C                   MULT      -1            EHPRTF
     C*** END ADD *****  11/14/02  ************************************
     C                   MULT      -1            EHWDC1                         WRT CONTR CRD
     C                   MULT      -1            EHXDC1                         WRT CHG CONTR CRD
     C                   MULT      -1            EHIDC1                         INF CONTR CRED
     C                   MULT      -1            EHJDC1                         INF CHG CONTR CRED
     C                   MULT      -1            EHWDC2                         WRT DEVIATION
     C                   MULT      -1            EHXDC2                         WRT CHG DEVIATIN
     C                   MULT      -1            EHIDC2                         INF DEVIATION
     C                   MULT      -1            EHJDC2                         INF CHG DEVIATIN
     C                   MULT      -1            EHWDC3                         WRT DISC
     C                   MULT      -1            EHXDC3                         WRT CHG DISC
     C                   MULT      -1            EHIDC3                         INF DISC
     C                   MULT      -1            EHJDC3                         INF CHG DISC
     C                   MULT      -1            EHWXP1                         WRT MOD PREM 1
     C                   MULT      -1            EHXXP1                         WRT CHG MOD PRM1
     C                   MULT      -1            EHIXP1                         INF MOD PREM 1
     C                   MULT      -1            EHJXP1                         INF CHG MOD PRM1
     C                   MULT      -1            EHWXP2                         WRT MOD PREM 2
     C                   MULT      -1            EHXXP2                         WRT CHG MOD PRM2
     C                   MULT      -1            EHIXP2                         INF MOD PREM 2
     C                   MULT      -1            EHJXP2                         INF CHG MOD PRM2
     C                   MULT      -1            EHWXP3                         WRT MOD PREM 3
     C                   MULT      -1            EHXXP3                         WRT CHG MOD PRM3
     C                   MULT      -1            EHIXP3                         INF MOD PREM 3
     C                   MULT      -1            EHJXP3                         INF CHG MOD PRM3
     C*** BEGIN ADD ***  01/19/98  ************************************
     C                   MULT      -1            EHWIN2                         WRT INCR LIM PR1  2N
     C                   MULT      -1            EHXIN2                         WR CG INC LIM PR1 2N
     C                   MULT      -1            EHIIN2                         IN INCR LIM PREM1 2N
     C                   MULT      -1            EHJIN2                         IN CG INC LIM PR1 2N
     C*** END ADD *****  01/19/98  ************************************
     C                   MULT      -1            EHWIN1                         WRT INCR LIM PR1
     C                   MULT      -1            EHXIN1                         WR CG INC LIM PR1
     C                   MULT      -1            EHIIN1                         IN INCR LIM PREM1
     C                   MULT      -1            EHJIN1                         IN CG INC LIM PR1
     C                   MULT      -1            EHWDCT                         WRT DED CRED
     C                   MULT      -1            EHXDCT                         WRT CHG DED CRED
     C                   MULT      -1            EHIDCT                         INF DED CRED
     C                   MULT      -1            EHJDCT                         INF CHG DED CRED
     C                   MULT      -1            EHWDCM                         WRT MAN CARE CRD
     C                   MULT      -1            EHXDCM                         WRT CHG MC CRED
     C                   MULT      -1            EHIDCM                         INF MAN CARE CRD
     C                   MULT      -1            EHJDCM                         INF CHG MC CRED
     C                   MULT      -1            EHWDCS                          WRT SAFETY CRED
     C                   MULT      -1            EHXDCS                         WRT CHG SFTY CRD
     C                   MULT      -1            EHIDCS                         INF SAFETY CRED
     C                   MULT      -1            EHJDCS                         INF CHG SFTY CRD
     C                   MULT      -1            EHWDCR                         WRT SCHED DB/CR
     C                   MULT      -1            EHXDCR                         WRT CG SCH DB/CR
     C                   MULT      -1            EHIDCR                         INF SCHED DB/CR
     C                   MULT      -1            EHJDCR                         INF CG SCH DB/CR
     C                   MULT      -1            EHWDC                          WRT SCHED DB/CR
     C                   MULT      -1            EHXDC                          WRT CG SCH DB/CR
     C                   MULT      -1            EHIDC                          INF SCHED DB/CR
     C                   MULT      -1            EHJDC                          INF CG SCH DB/CR
     C                   MULT      -1            EHWSBJ                         WRT SUBJECT PREM
     C                   MULT      -1            EHXSBJ                         WRT CG SUBJECT PREM
     C                   MULT      -1            EHISBJ                         INF SUBJECT PREM
     C                   MULT      -1            EHJSBJ                         INF CG SUBJECT PREM
     C                   MULT      -1            EHWMPM                         WRT MOD PREM
     C                   MULT      -1            EHXMPM                         WRT CG MOD PREM
     C                   MULT      -1            EHIMPM                         INF MOD PREM
     C                   MULT      -1            EHJMPM                         INF CG MOD PREM
     C                   MULT      -1            EHWFRS                         WRT FACTOR RESULT
     C                   MULT      -1            EHXFRS                         WRT CG FACTOR RESULT
     C                   MULT      -1            EHIFRS                         INF FACTOR RESULT
     C                   MULT      -1            EHJFRS                         INF CG FACTOR RESULT
     C                   MULT      -1            EHWDV2                         WRT FACTOR RESULT
     C                   MULT      -1            EHXDV2                         WRT CG FACTOR RESULT
     C                   MULT      -1            EHIDV2                         INF FACTOR RESULT
     C                   MULT      -1            EHJDV2                         INF CG FACTOR RESULT
     C                   MULT      -1            EHTXPR                         TAXABLE PREMIUM
      *** BEG ADD ***** 08/16/08 *****************************************
     C                   MULT      -1            EHWTER                         WRT FOR TERR
     C                   MULT      -1            EHXTER                         CHG FOR TERR
     C                   MULT      -1            EHITER                         INF FOR TERR
     C                   MULT      -1            EHJTER                         INF CHG FOR TERR
     C                   MULT      -1            EHWDTR                         WRT DOM TERR
     C                   MULT      -1            EHXDTR                         CHG DOM TERR
     C                   MULT      -1            EHIDTR                         INF DOM TERR
     C                   MULT      -1            EHJDTR                         INF CHG DOM TERR
     C                   MULT      -1            EHWVLC                         WRT VOL LABOR
     C                   MULT      -1            EHXVLC                         CHG VOL LABOR
     C                   MULT      -1            EHIVLC                         INF VOL LABOR
     C                   MULT      -1            EHJVLC                         INF CHG VOL LABOR
      *** END ADD ***** 08/16/08 *****************************************
      *** BEG ADD ***** 01/31/20 *****************************************
     C                   MULT      -1            EHWRPR
     C                   MULT      -1            EHXRPR
     C                   MULT      -1            EHWHXM
     C                   MULT      -1            EHXHXM
     C                   MULT      -1            EHIHXM
     C                   MULT      -1            EHJHXM
     C                   MULT      -1            EHWARC
     C                   MULT      -1            EHXARC
     C                   MULT      -1            EHIARC
     C                   MULT      -1            EHJARC
     C                   MULT      -1            EHWSRA
     C                   MULT      -1            EHXSRA
     C                   MULT      -1            EHD1W2
     C                   MULT      -1            EHD1C2
     C                   MULT      -1            EHD1I2
     C                   MULT      -1            EHD1H2
     C                   MULT      -1            EHD2W2
     C                   MULT      -1            EHD2C2
     C                   MULT      -1            EHD2I2
     C                   MULT      -1            EHD2H2
     C                   MULT      -1            EHWHCN
     C                   MULT      -1            EHXHCN
     C                   MULT      -1            EHIHCN
     C                   MULT      -1            EHJHCN
     C                   MULT      -1            EHWUSA
     C                   MULT      -1            EHXUSA
     C                   MULT      -1            EHW1AS
     C                   MULT      -1            EHX1AS
     C                   MULT      -1            EHI1AS
     C                   MULT      -1            EHJ1AS
     C                   MULT      -1            EHW2AS
     C                   MULT      -1            EHX2AS
     C                   MULT      -1            EHI2AS
     C                   MULT      -1            EHJ2AS
     C                   MULT      -1            EHW3AS
     C                   MULT      -1            EHX3AS
     C                   MULT      -1            EHI3AS
     C                   MULT      -1            EHJ3AS
     C                   MULT      -1            EHW4AS
     C                   MULT      -1            EHX4AS
     C                   MULT      -1            EHI4AS
     C                   MULT      -1            EHJ4AS
     C                   MULT      -1            EHW5AS
     C                   MULT      -1            EHX5AS
     C                   MULT      -1            EHI5AS
     C                   MULT      -1            EHJ5AS
     C                   MULT      -1            EHW6AS
     C                   MULT      -1            EHX6AS
     C                   MULT      -1            EHI6AS
     C                   MULT      -1            EHJ6AS
     C                   MULT      -1            EHW7AS
     C                   MULT      -1            EHX7AS
     C                   MULT      -1            EHI7AS
     C                   MULT      -1            EHJ7AS
     C                   MULT      -1            EHWSF
     C                   MULT      -1            EHXSF
     C                   MULT      -1            EHISF
     C                   MULT      -1            EHJSF
     C                   MULT      -1            EHWMF
     C                   MULT      -1            EHXMF
     C                   MULT      -1            EHIMF
     C                   MULT      -1            EHJMF
     C                   MULT      -1            EHWANC
     C                   MULT      -1            EHXANC
     C                   MULT      -1            EHIANC
     C                   MULT      -1            EHJANC
     C                   MULT      -1            EHDEV2WBP
     C                   MULT      -1            EHDEV2WAP
     C                   MULT      -1            EHDEV2IBP
     C                   MULT      -1            EHDEV2IAP
     C                   MULT      -1            EHWPF
     C                   MULT      -1            EHXPF
     C                   MULT      -1            EHIPF
     C                   MULT      -1            EHJPF
      *** END ADD ***** 01/31/20 *****************************************
     C                   MOVE      Q1USER        EHUSER                         UPD BY USER
     C                   MOVE      Q1PGM         EHPGM                          UPD BY PGM
     C                   MOVE      DSTIME        EHTIME                         UPD TIME
     C                   MOVE      XXDATE        EHDT
     C                   MOVE      Q1USER        EHIUSR                         REC CRTD BY
     C                   MOVE      XXDATE        EHIDT
     C                   MOVE      'Y'           EHRWFL                         REWRITE FLAG
     C                   MOVE      XXDATE        EHRWDT                         REWRITE DATE
     C*
     C*  WRITE THE REVERSING RECORD
     C*
     C                   WRITE     PEH01
     C*
     C     NEXTEH        TAG
     C     EHKY          READE     PEH01                                  50
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
