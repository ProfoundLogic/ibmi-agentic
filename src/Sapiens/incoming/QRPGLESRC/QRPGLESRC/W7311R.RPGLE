     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7311R - CANCELATION NOTICE
     FSMCOL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWXCNL1    UF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFCT. DATE
     D  EMEFD5               001    007  0
     D  EMEFD4               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD2               022    028  0
     D  EMEFD1               029    035  0
     D                 DS
     D  EMPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
     D                 DS
     D  EMMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D                 DS
     D  COZIP                  1      9  0
     D  XXZPC1                 1      5  0
     D  XXZPC2                 6      9  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    CODT          XXCHEF
     C     *LIKE         DEFINE    CODT          XXEFFD
     C     *LIKE         DEFINE    CODT          XXYMD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CANCELATION NOTICE WORK FILE
     C*
     C     CNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             X                 2 0
     C     *BLANK        LOOKUP    CONM(X)                            60
     C                   MOVEA     CONM(X)       XXCONM           40
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE COMPANY ADDRESS
     C*
     C                   MOVE      COST          XXSTAT            2
     C                   MOVE      XXZPC1        XXZIP1            5
     C                   MOVE      XXZPC2        XXZIP2            4 0
     C                   MOVE      COCTY         XXCITY           25
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVE      XXCSTZ        XXCCSZ           40
     C*
     C*   CHAIN TO THE CANCELATION FILE
     C*
     C     CNKEY         CHAIN     PCN01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   START READING THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXADR1           40
     C                   MOVE      *BLANKS       XXADR2           40
     C                   MOVE      *BLANKS       XXCSZ            40
     C     EMMPFL        IFEQ      'M'
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
     C*
     C                   MOVE      EMMST         XXSTAT            2
     C                   MOVE      XXZPM1        XXZIP1            5
     C                   MOVE      XXZPM2        XXZIP2            4 0
     C                   MOVE      EMMCTY        XXCITY           25
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVE      XXCSTZ        XXCSZ
     C                   MOVE      EMMAD1        XXADR1
     C                   MOVE      EMMAD2        XXADR2
     C*
     C*
     C                   ELSE
     C*
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE PHYSICAL ADDRESS
     C*
     C                   MOVE      EMPST         XXSTAT            2
     C                   MOVE      XXZPP1        XXZIP1            5
     C                   MOVE      XXZPP2        XXZIP2            4 0
     C                   MOVE      EMPCTY        XXCITY           25
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVE      XXCSTZ        XXCSZ
     C                   MOVE      EMPAD1        XXADR1
     C                   MOVE      EMPAD2        XXADR2
     C*
     C*
     C                   END
     C*
     C*   FIND THE EFFECTIVE DATE
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                              60
     C                   MOVE      ED(Y)         XXEFFD
     C*
     C*   GET NAME OF MONTH FOR EFFECTIVE DATE
     C*
     C                   MOVE      *BLANKS       XXEFNM           09
     C                   MOVE      *BLANKS       XXEFYR           03
     C                   MOVE      *BLANKS       XXEFDY           02
     C                   MOVE      *ZEROS        XXECMO            2 0
     C                   MOVEL     XXEFFD        XXEFYR
     C                   MOVE      XXEFFD        XXEFDY
     C                   MOVE      XXEFFD        XXCHEF
     C*
     C                   Z-ADD     XXCHEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMDY             8 0
     C*
     C                   MOVEL     XXMDY         XXECMO            2 0
     C*
     C                   EXSR      SR1ST
     C*
     C                   MOVE      XXNMMO        XXEFNM
     C*
     C*   GET THE FUTURE DAYS
     C*
     C                   MOVE      *BLANKS       XXERR            01
     C                   MOVE      *ZEROS        XXFUTD            5 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXMDY
     C*
     C                   Z-ADD     32            XXFUTD
     C                   CALL      'SRFUTR8'
     C                   PARM                    XXMDY
     C                   PARM                    XXFUTD
     C                   PARM                    XXERR
     C*
     C*   GET THE NAME OF THE FUTURE MONTH
     C*
     C                   MOVE      *BLANKS       XXCNNM           09
     C                   MOVE      *BLANKS       XXCNYR           03
     C                   MOVE      *BLANKS       XXCNDY           02
     C                   MOVE      *ZEROS        XXECMO
     C                   MOVEL     XXMDY         XXECMO
     C*
     C                   EXSR      SR1ST
     C*
     C                   MOVE      XXNMMO        XXCNNM
     C                   MOVE      XXMDY         XXCHCN            8 0
     C*
     C                   Z-ADD     XXCHCN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C                   MOVE      XXYMD         XXCNDY
     C                   MOVEL     XXYMD         XXCNYR
     C*
     C*
     C*   GET THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C*
     C*   GET THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AGKEY         CHAIN     PAG01                              60
     C*
     C*   CHANGE THE PROCESSED STAT
     C*
     C                   MOVE      'P'           CNSTAT
     C                   UPDATE    PCN01
     C*
     C*
     C*   PRINT THE FORM PAGE
     C*
     C                   EXCEPT    DET1
     C     XXADR2        IFEQ      *BLANKS
     C                   EXCEPT    DET2
     C                   ELSE
     C                   EXCEPT    DET3
     C                   END
     C                   EXCEPT    DET4
     C                   END
     C     CNKEY         READE     PCN01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*******************************************************
     C*        -SUBROUTINE TO FIND NAME OF MONTH-           *
     C*******************************************************
     CSR   SR1ST         BEGSR
     C                   MOVE      *BLANKS       XXNMMO           09
     C                   MOVE      *BLANKS       XXERR            01
     C                   CALL      'SRMTHD'
     C                   PARM                    XXECMO
     C                   PARM                    XXNMMO
     C                   PARM                    XXERR
     C                   ENDSR
     C*
     C******************************************************
     C*    -SUBROUTINE TO LOAD THE CITY, STATE, AND ZIP    *
     C******************************************************
     CSR   SR2ND         BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSTZ           40
     C                   ENDSR
     C*
     OQSYSPRT   E            DET1           2 11
     O                       EMFEIN              68
     O          E            DET1           1
     O                       EMNAME              47
     O          E            DET1           1
     O                       XXADR1              47
     O                       DSDATE        Y     66
     O          E            DET2                24
     O                       XXCSZ               47
     O          E            DET3           1
     O                       XXADR2              47
     O          E            DET3                24
     O                       XXCSZ               47
     O          E            DET4           2
     O                       CNFND               68
     O                                           69 '-'
     O                       CNEMP#              78
     O          E            DET4           2
     O                       XXEFNM              27
     O                                           31 ','
     O                       XXEFDY              30
     O                       XXEFYR              39
     O                       XXCNNM              67
     O                                           71 ','
     O                       XXCNDY              70
     O                       XXCNYR              78
     O          E            DET4                35
     O                       CNFND               63
     O                                           64 '-'
     O                       CNEMP#              73
     O          E            DET4           2
     O                       XXCONM              49
     O          E            DET4           2
     O                       COADR1              49
     O          E            DET4                58
     O                       XXCCSZ              49
     O          E            DET4                61
     O                                           52 'XX'
     O          E            DET4           1
     O                       AGNAME              54
     O          E            DET4           1
     O                                           33 'CERTIFICATE HOLDERS'
