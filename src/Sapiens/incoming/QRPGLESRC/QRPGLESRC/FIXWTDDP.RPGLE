     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWTDDP - COPY GROUP 600 DEDUCTIBLE RECORDS TO ALL OTHER GRPS
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 07/25/07  C:65  PROJECT SUPP#59016                  *
     H*                                                               *
     H*  THIS PROGRAM WILL COPY THE DEDUCTIBLE TABLE RECORDS FROM     *
     H*  GROUP 600 TO ALL OTHER GROUPS IF THEY DO NOT ALREADY EXIST.  *
     H*                                                               *
     H*****************************************************************
     H*
     FWMFNL     IF   E           K DISK
     FWTDDL     IF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FWTDDL2    IF A E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPDD02
     I              DDCO#                       D2CO#
     I              DDFND                       D2FND
     I              DDFYR                       D2FYR
     I              DDCODE                      D2CODE
     I              DDEFDT                      D2EFDT
     I              DDTYPE                      D2TYPE
     I              DDPCT                       D2PCT
     I              DDAMPO                      D2AMPO
     I              DDAMAG                      D2AMAG
     I              DDSTAT                      D2STAT
     I              DDCLAS                      D2CLAS
     I              DDUSER                      D2USER
     I              DDPGM                       D2PGM
     I              DDTIME                      D2TIME
     I              DDDT                        D2DT
     I              DDST                        D2ST
     I              DDDESC                      D2DESC
     I              DDDC01                      D2DC01
     I              DDDC02                      D2DC02
     I              DDDC03                      D2DC03
     I              DDDC04                      D2DC04
     I              DDDC05                      D2DC05
     I              DDDC06                      D2DC06
     I              DDDC07                      D2DC07
     I              DDDC08                      D2DC08
     I              DDDC09                      D2DC09
     I              DDDC10                      D2DC10
     I              DDIUSR                      D2IUSR
     I              DDIPGM                      D2IPGM
     I              DDITIM                      D2ITIM
     I              DDIDT                       D2IDT
     I              DDPLAN                      D2PLAN
     I              DDVERB                      D2VERB
     I              DDRNW                       D2RNW
     I              DDLRGF                      D2LRGF
     I              DDTOTI                      D2TOTI
     I              DDLRGD                      D2LRGD

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     DDKY2         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    D2ST
     C                   KFLD                    D2CODE
     C                   KFLD                    D2EFDT
     C*
     C     *LIKE         DEFINE    DSTIME        XXTIME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C                   Z-ADD     777           XXCO#             3 0
     C                   Z-ADD     600           XXFND             3 0
     C*
     C                   READ      PFN01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     FNFND         IFNE      600
     C*
     C     DDKY          CHAIN     PDD02                              50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C     DDKY2         CHAIN     PDD01                              40
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   CLEAR                   PDD01
     C*
     C                   Z-ADD     FNCO#         DDCO#
     C                   Z-ADD     FNFND         DDFND
     C*
     C                   MOVE      D2FYR         DDFYR
     C                   MOVE      D2ST          DDST
     C                   MOVE      D2STAT        DDSTAT
     C                   MOVEL(P)  D2DESC        DDDESC
     C                   MOVE      D2TYPE        DDTYPE
     C                   MOVE      D2CLAS        DDCLAS
     C                   MOVEL     D2CODE        DDCODE
     C                   Z-ADD     D2PCT         DDPCT
     C                   Z-ADD     D2AMPO        DDAMPO
     C                   Z-ADD     D2AMAG        DDAMAG
     C                   Z-ADD     D2EFDT        DDEFDT
     C                   MOVE      D2DC01        DDDC01
     C                   MOVE      D2DC02        DDDC02
     C                   MOVE      D2DC03        DDDC03
     C                   MOVE      D2DC04        DDDC04
     C                   MOVE      D2DC05        DDDC05
     C                   MOVE      D2DC06        DDDC06
     C                   MOVE      D2DC07        DDDC07
     C                   MOVE      D2DC08        DDDC08
     C                   MOVE      D2DC09        DDDC09
     C                   MOVE      D2DC10        DDDC10
     C                   MOVE      D2PLAN        DDPLAN
     C                   MOVE      D2VERB        DDVERB
     C                   MOVE      D2TOTI        DDTOTI
     C                   MOVE      D2RNW         DDRNW
     C                   MOVE      D2LRGF        DDLRGF
     C                   MOVE      D2LRGD        DDLRGD
     C*
     C                   MOVEL     Q1PGM         DDPGM
     C                   MOVEL     Q1USER        DDUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   MOVEL     DSTIME        DDTIME
     C                   MOVEL     Q1PGM         DDIPGM
     C                   MOVEL     Q1USER        DDIUSR
     C                   MOVEL     SRDTO         DDIDT
     C                   MOVEL     DSTIME        DDITIM
     C*
     C                   WRITE     PDD01
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C*
     C     DDKY          READE     PDD02                                  50
     C                   ENDDO
     C*
     C                   ENDIF                                                  FNFND IFNE
     C*
     C                   READ      PFN01                                  60
     C                   ENDDO                                                  DOW *IN60
     C*
     C                   MOVE      *ON           *INLR
