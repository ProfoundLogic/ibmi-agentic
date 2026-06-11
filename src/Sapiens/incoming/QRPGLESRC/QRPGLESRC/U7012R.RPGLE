     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE U7012R - PRE-CERTIFICATION REPORT WORKFILE BUILD PROGRAM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    WORKFILE BUILD PROGAM FOR PRE-CERTIFICATION REPORT         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7010C                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAA                *
     H*  CREATED: 11/04/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/02/09 C:232 PROJECT: 139WMR                                *
     H*                OFAC Reporting-Add Pending Status Indicator.   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FUXPCP     O  A E           K DISK
     FUMPCL3    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FUMPCL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC03)
     FFDCAL5    IF   E           K DISK
     FFAEOL1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPPC03
     I              PCCO#                       PCCO#3
     I              PCSUB#                      PCSUB3
     I              PCFND                       PCFND3
     I              PCFYR                       PCFYR3
     I              PCCASE                      PCCAS3
     I              PCLIAB                      PCLIA3
     I              PCSTAT                      PCSTA3
     I              PCADDT                      PCADD3
     I              PCRVDT                      PCRVD3
     I              PCCERT                      PCCER3
     I              PCEXT                       PCEXT3
     I              PCFEIN                      PCFEI3
     I              PCMOD                       PCMOD3
     I              PCICD1                      PCIC13
     I              PCICD2                      PCIC23
     I              PCICD3                      PCIC33
     I              PCICD4                      PCIC43
     I              PCICD5                      PCIC53
     I              PCCPT1                      PCCP13
     I              PCCPT2                      PCCP23
     I              PCCPT3                      PCCP33
     I              PCCPT4                      PCCP43
     I              PCCPT5                      PCCP53
     I              PCCPT6                      PCCP63
     I              PCCPT7                      PCCP73
     I              PCCPT8                      PCCP83
     I              PCSPD1                      PCSP13
     I              PCSPD2                      PCSP23
     I              PCSPD3                      PCSP33
     I              PCSPD4                      PCSP43
     I              PCSPD5                      PCSP53
     I              PCSPD6                      PCSP63
     I              PCSPD7                      PCSP73
     I              PCSPD8                      PCSP83
     I              PCEPD1                      PCEP13
     I              PCEPD2                      PCEP23
     I              PCEPD3                      PCEP33
     I              PCEPD4                      PCEP43
     I              PCEPD5                      PCEP53
     I              PCEPD6                      PCEP63
     I              PCEPD7                      PCEP73
     I              PCEPD8                      PCEP83
     I              PCA#V1                      PCA#13
     I              PCA#V2                      PCA#23
     I              PCA#V3                      PCA#33
     I              PCA#V4                      PCA#43
     I              PCA#V5                      PCA#53
     I              PCA#V6                      PCA#63
     I              PCA#V7                      PCA#73
     I              PCA#V8                      PCA#83
     I              PCTOS1                      PCTO13
     I              PCTOS2                      PCTO23
     I              PCTOS3                      PCTO33
     I              PCTOS4                      PCTO43
     I              PCTOS5                      PCTO53
     I              PCTOS6                      PCTO63
     I              PCTOS7                      PCTO73
     I              PCTOS8                      PCTO83
     I              PCINTR                      PCINT3
     I              PCPASG                      PCPAS3
     I              PCREQD                      PCREQ3
     I              PCCERD                      PCCED3
     I              PCRECD                      PCREC3
     I              PCPOS                       PCPOS3
     I              PCUSER                      PCUSE3
     I              PCPGM                       PCPGM3
     I              PCTIME                      PCTIM3
     I              PCDT                        PCDT3
     I              PCIUSR                      PCIUS3
     I              PCIDT                       PCIDT3
     I              PCENTD                      PCENT3
     I              PCADST                      PCADS3
     I              PCTYAD                      PCTYA3
     I              PCPARF                      PCPAR3
     I              PCTPHY                      PCTPH3
     I              PCTPHM                      PCTPM3
     I              PC2OPN                      PC2OP3
     I              PC2DT                       PC2DT3
     I              PC2PHY                      PC2PH3
     I              PC2PHM                      PC2PM3
     I              PC2CSC                      PC2CS3
     I              PCIME                       PCIME3
     I              PCIMDT                      PCIMD3
     I              PCIPHY                      PCIPH3
     I              PCIPHM                      PCIPM3
     I              PCICSC                      PCICS3
     I              PCRINJ                      PCRIN3
     I              PCHMHL                      PCHMH3
     I              PCCAT                       PCCAT3
     I              PCLOST                      PCLOS3
     I              PCAGE                       PCAGE3

     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C*   KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
     C*
     C*   KEY LIST FOR THE PRE-CERT FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    EOEOB#
     C*
     C*   KEY LIST FOR THE EOMB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQSTAT            1
     C                   PARM                    QQSORT            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CAFDAT        XXFDAT                         6.0N
     C     *LIKE         DEFINE    CATDAT        XXTDAT                         6.0N
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    PCTACD        XXTACD                         5.0N
     C     *LIKE         DEFINE    PCTSAV        XXTSAV                         5.0N
     C     *LIKE         DEFINE    PCRECD        XXRECD                         3.0N
     C                   Z-ADD     *ZEROS        XXDIFF            5 0          5.0N
     C*
     C* READ PRE-CERT FILE AND CHECK AGAINST CLAIM RECORDS
     C* THE FILES HAVE BEEN QUERIED BY THE PRVIOUS CLP PROGRAM SO WE
     C* ARE ONLY LOOKING AT PRE-CERT RECORDS THAT MEET THE SELECTION
     C* STATUS SELECTED ON PROMPT, AND WE ARE ONLY LOOKING AT CLAIM
     C* RECORDS THAT MEET THE CRITERIA SELECTED ON THE PROMPT
     C*
     C                   READ      PPC02                                  60
     C     *IN60         DOWEQ     '0'
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C                   EXSR      WRTWK
     C                   END
     C                   READ      PPC02                                  60
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* WRTWK - SUBROUTINE TO WRITE RECORDS TO WORKFILE
     C*****************************************************************
     C     WRTWK         BEGSR
     C*
     C* GET THE CLAIMANT NAME AND CONDENSE INTO ONE FIELD FOR WORKFILE
     C*
     C                   MOVE      CMSS#         PCSS#
     C                   Z-ADD     CMACDT        PCACDT
     C     CMSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        PCCNAM
     C                   END                                                    END CDFSN
     C                   END                                                    END *IN60
     C*
     C                   Z-ADD     PCCO#         PCCO#
     C                   Z-ADD     PCSUB#        PCSUB#
     C                   Z-ADD     PCFND         PCFND
     C                   Z-ADD     PCFYR         PCFYR
     C                   Z-ADD     PCCASE        PCCASE
     C                   MOVE      PCLIAB        PCLIAB
     C                   MOVE      PCSTAT        PCSTAT
     C                   Z-ADD     PCADDT        PCADDT
     C                   Z-ADD     PCRVDT        PCRVDT
     C*
     C* GET THE PROVIDER NAME AND CONDENSE INTO ONE FIELD FOR WORKFILE
     C*
     C                   Z-ADD     PCFEIN        PCFEIN
     C                   Z-ADD     PCMOD         PCMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    ANDNE'I'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C     PDSPCD        IFEQ      'CHI'
     C     PDSPCD        OREQ      'PHT'
     C                   MOVE      'Y'           PCPTCH
     C                   ELSE
     C                   MOVE      *BLANKS       PCPTCH
     C                   END
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        PCNAME
     C                   ELSE
     C                   MOVEL     PDNAME        PCNAME
     C                   END                                                    END CDFSN
     C                   END                                                    END *IN60
     C*
     C                   MOVE      PCINTR        PCINTR
     C                   MOVE      PCICD1        PCICD1
     C                   MOVE      PCICD2        PCICD2
     C                   MOVE      PCICD3        PCICD3
     C                   MOVE      PCICD4        PCICD4
     C                   MOVE      PCICD5        PCICD5
     C                   MOVE      PCPOS         PCPOS
     C                   MOVE      PCPASG        PCPASG
     C                   Z-ADD     PCCAT         PCCAT
     C                   Z-ADD     PCAGE         PCAGE
     C                   MOVE      PCLOST        PCLOST
     C                   Z-ADD     PCREQD        PCREQD
     C                   Z-ADD     PCCERD        PCCERD
     C                   Z-ADD     PCRECD        PCRECD
     C                   Z-ADD     PCRECD        XXRECD
     C                   MOVE      Q1USER        PCUSER
     C                   MOVE      Q1PGM         PCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C*
     C                   Z-ADD     DSTIME        PCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCIDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCENTD
     C*
     C                   MOVE      Q1USER        PCIUSR
     C                   MOVE      PCCPT1        PCCPT1
     C                   MOVE      PCCPT2        PCCPT2
     C                   MOVE      PCCPT3        PCCPT3
     C                   MOVE      PCCPT4        PCCPT4
     C                   MOVE      PCCPT5        PCCPT5
     C                   MOVE      PCCPT6        PCCPT6
     C                   MOVE      PCCPT7        PCCPT7
     C                   MOVE      PCCPT8        PCCPT8
     C                   MOVE      PCA#V1        PCA#V1
     C                   MOVE      PCA#V2        PCA#V2
     C                   MOVE      PCA#V3        PCA#V3
     C                   MOVE      PCA#V4        PCA#V4
     C                   MOVE      PCA#V5        PCA#V5
     C                   MOVE      PCA#V6        PCA#V6
     C                   MOVE      PCA#V7        PCA#V7
     C                   MOVE      PCA#V8        PCA#V8
     C                   MOVE      PCTOS1        PCTOS1
     C                   MOVE      PCTOS2        PCTOS2
     C                   MOVE      PCTOS3        PCTOS3
     C                   MOVE      PCTOS4        PCTOS4
     C                   MOVE      PCTOS5        PCTOS5
     C                   MOVE      PCTOS6        PCTOS6
     C                   MOVE      PCTOS7        PCTOS7
     C                   MOVE      PCTOS8        PCTOS8
     C                   Z-ADD     PCSPD1        PCSPD1
     C                   Z-ADD     PCSPD2        PCSPD2
     C                   Z-ADD     PCSPD3        PCSPD3
     C                   Z-ADD     PCSPD4        PCSPD4
     C                   Z-ADD     PCSPD5        PCSPD5
     C                   Z-ADD     PCSPD6        PCSPD6
     C                   Z-ADD     PCSPD7        PCSPD7
     C                   Z-ADD     PCSPD8        PCSPD8
     C                   Z-ADD     PCEPD1        PCEPD1
     C                   Z-ADD     PCEPD2        PCEPD2
     C                   Z-ADD     PCEPD3        PCEPD3
     C                   Z-ADD     PCEPD4        PCEPD4
     C                   Z-ADD     PCEPD5        PCEPD5
     C                   Z-ADD     PCEPD6        PCEPD6
     C                   Z-ADD     PCEPD7        PCEPD7
     C                   Z-ADD     PCEPD8        PCEPD8
     C                   Z-ADD     PCEXT         PCEXT
     C                   MOVE      PCADST        PCADST
     C                   MOVE      PCTYAD        PCTYAD
     C                   MOVE      PCPARF        PCPARF
     C                   MOVE      PCRINJ        PCRINJ
     C                   MOVE      PCHMHL        PCHMHL
     C                   MOVEL     PCTPHY        PCTPHY
     C                   Z-ADD     PCTPHM        PCTPHM
     C                   MOVE      PC2OPN        PC2OPN
     C                   Z-ADD     PC2DT         PC2DT
     C                   MOVE      PC2PHY        PC2PHY
     C                   Z-ADD     PC2PHM        PC2PHM
     C                   MOVE      PC2CSC        PC2CSC
     C                   MOVE      PCIME         PCIME
     C                   Z-ADD     PCIMDT        PCIMDT
     C                   MOVE      PCIPHY        PCIPHY
     C                   Z-ADD     PCIPHM        PCIPHM
     C                   MOVE      PCICSC        PCICSC
     C                   MOVE      PCCERT        PCCERT
     C*
     C* GET THE TOTAL DAYS FROM ALL RE-CERTIFICATION RECORDS
     C*
     C                   MOVE      *BLANKS       XXCERT
     C                   MOVE      PCCERT        XXCERT
     C                   Z-ADD     PCCO#         XXCO#
     C                   Z-ADD     PCSUB#        XXSUB#
     C                   Z-ADD     PCFND         XXFND
     C                   Z-ADD     *ZEROS        XXCERD            5 0
     C     PCKEY         CHAIN     PPC03                              60
     C     *IN60         DOWEQ     '0'
     C     PCEXT3        IFNE      *ZEROS
     C                   ADD       PCREC3        XXCERD
     C                   END                                                    END PCEXT
     C     PCKEY         READE     PPC03                                  60
     C                   END                                                    END *IN60
     C                   Z-ADD     XXCERD        PCTCED
     C*
     C* GET THE TOTAL NUMBER OF ACTUAL DAYS FROM THE DETAIL CHARGE FILE
     C*
     C     EOKEY         CHAIN     PEO01                              20
     C     *IN20         DOWEQ     '0'
     C     EOCERT        IFEQ      PCCERT
     C*
     C                   Z-ADD     0             XXTDAT
     C                   Z-ADD     0             XXFDAT
     C     CAKEY         CHAIN     PCA01                              60
     C     *IN60         DOWEQ     '0'
     C     CAFDAT        IFNE      XXFDAT
     C     CATDAT        ANDNE     XXTDAT
     C     CATDAT        SUB       CAFDAT        XXDIFF
     C     XXDIFF        IFEQ      0
     C                   ADD       1             XXTACD
     C                   ELSE
     C                   ADD       XXDIFF        XXTACD
     C                   END
     C                   Z-ADD     0             XXDIFF
     C                   Z-ADD     CATDAT        XXTDAT
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   END                                                    END CAFDAT
     C     CAKEY         READE     PCA01                                  60
     C                   END                                                    END *IN60
     C                   END                                                    END EOCERT
     C     EOKEY         READE     PEO01                                  20
     C                   END                                                    END *IN20
     C*
     C* GET THE TOTAL DAYS SAVED BY PCRECD(REC. DAYS) MINUS PCTACD
     C* (ACTUAL DAYS)
     C*
     C     XXTACD        IFGT      0
     C     XXRECD        SUB       XXTACD        XXTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXTSAV
     C                   END
     C*
     C* IF THE TOTAL SAVED DAYS IS LESS THAN ZERO THEN ADJUST TO ZERO
     C*
     C     XXTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        XXTSAV
     C                   END                                                    END PCTSAV
     C                   Z-ADD     XXTACD        PCTACD
     C                   Z-ADD     XXTSAV        PCTSAV
     C                   Z-ADD     *ZEROS        XXTACD
     C                   Z-ADD     *ZEROS        XXTSAV
     C                   Z-ADD     *ZEROS        XXRECD
     C*
     C                   WRITE     PPC01
     C*
     C                   ENDSR
