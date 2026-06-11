     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry   dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U7090R - MANAGED CARE REPORT
     F*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  08    Batch Mode                                             *
     H*  09    Demand Mode                                            *
     H*  60    General Chain/Usage                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*                Retrieve brand Name                            *
     H*                                                               *
     H*  03/07/00 C:62 PROJ: UR025                                    *
     H*                - CHANGED OUTPUT TO HANDLE 9.0 EMP#            *
     H*                - CHANGED FYR IN OUTPUT TO 3.0                 *
     H*                - CHANGED DATES IN OUTPUT TO 8.0.              *
     H*                                                               *
     H*  03/01/95 C:32 PROJ: URMNT                                    *
     H*                CHGD TITLE 'CASE MANAGEMENT' TO 'HOME HEALTH'  *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FUMPCL     IF   E           K DISK
     FUMPCL3    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FUMPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC03)
     FFMEML     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFAEOL1    IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D*
     D                 DS
     D*** BEGIN CHG *** 03/07/00 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   CHG *** 03/07/00 **************************************
     D                 DS
     D  XXSS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F*

     IPPC02
     I              PCCO#                       P2CO#
     I              PCSUB#                      P2SUB#
     I              PCFND                       P2FND
     I              PCFYR                       P2FYR
     I              PCCASE                      P2CASE
     I              PCLIAB                      P2LIAB
     I              PCSTAT                      P2STAT
     I              PCADDT                      P2ADDT
     I              PCRVDT                      P2RVDT
     I              PCCERT                      P2CERT
     I              PCEXT                       P2EXT
     I              PCFEIN                      P2FEIN
     I              PCMOD                       P2MOD
     I              PCICD1                      P2ICD1
     I              PCICD2                      P2ICD2
     I              PCICD3                      P2ICD3
     I              PCICD4                      P2ICD4
     I              PCICD5                      P2ICD5
     I              PCCPT1                      P2CPT1
     I              PCCPT2                      P2CPT2
     I              PCCPT3                      P2CPT3
     I              PCCPT4                      P2CPT4
     I              PCCPT5                      P2CPT5
     I              PCCPT6                      P2CPT6
     I              PCCPT7                      P2CPT7
     I              PCCPT8                      P2CPT8
     I              PCSPD1                      P2SPD1
     I              PCSPD2                      P2SPD2
     I              PCSPD3                      P2SPD3
     I              PCSPD4                      P2SPD4
     I              PCSPD5                      P2SPD5
     I              PCSPD6                      P2SPD6
     I              PCSPD7                      P2SPD7
     I              PCSPD8                      P2SPD8
     I              PCEPD1                      P2EPD1
     I              PCEPD2                      P2EPD2
     I              PCEPD3                      P2EPD3
     I              PCEPD4                      P2EPD4
     I              PCEPD5                      P2EPD5
     I              PCEPD6                      P2EPD6
     I              PCEPD7                      P2EPD7
     I              PCEPD8                      P2EPD8
     I              PCA#V1                      P2A#V1
     I              PCA#V2                      P2A#V2
     I              PCA#V3                      P2A#V3
     I              PCA#V4                      P2A#V4
     I              PCA#V5                      P2A#V5
     I              PCA#V6                      P2A#V6
     I              PCA#V7                      P2A#V7
     I              PCA#V8                      P2A#V8
     I              PCTOS1                      P2TOS1
     I              PCTOS2                      P2TOS2
     I              PCTOS3                      P2TOS3
     I              PCTOS4                      P2TOS4
     I              PCTOS5                      P2TOS5
     I              PCTOS6                      P2TOS6
     I              PCTOS7                      P2TOS7
     I              PCTOS8                      P2TOS8
     I              PCINTR                      P2INTR
     I              PCPASG                      P2PASG
     I              PCREQD                      P2REQD
     I              PCCERD                      P2CERD
     I              PCRECD                      P2RECD
     I              PCPOS                       P2POS
     I              PCUSER                      P2USER
     I              PCPGM                       P2PGM
     I              PCTIME                      P2TIM
     I              PCDT                        P2DT
     I              PCIUSR                      P2IUSR
     I              PCIDT                       P2IDT
     I              PCENTD                      P2ENTD
     I              PCADST                      P2ADST
     I              PCTYAD                      P2TYAD
     I              PCPARF                      P2PARF
     I              PCTPHY                      P2TPHY
     I              PCTPHM                      P2TPHM
     I              PC2OPN                      P22OPN
     I              PC2DT                       P22DT
     I              PC2PHY                      P22PHY
     I              PC2PHM                      P22PHM
     I              PC2CSC                      P22CSC
     I              PCIME                       P2IME
     I              PCIMDT                      P2IMDT
     I              PCIPHY                      P2IPHY
     I              PCIPHM                      P2IPHM
     I              PCICSC                      P2ICSC
     I              PCRINJ                      P2RINJ
     I              PCHMHL                      P2HMHL
     I              PCCAT                       P2CAT
     I              PCLOST                      P2LOST
     I              PCAGE                       P2AGE
     I*
     IPPC03
     I              PCCO#                       P3CO#
     I              PCSUB#                      P3SUB#
     I              PCFND                       P3FND
     I              PCFYR                       P3FYR
     I              PCCASE                      P3CASE
     I              PCLIAB                      P3LIAB
     I              PCSTAT                      P3STAT
     I              PCADDT                      P3ADDT
     I              PCRVDT                      P3RVDT
     I              PCCERT                      P3CERT
     I              PCEXT                       P3EXT
     I              PCFEIN                      P3FEIN
     I              PCMOD                       P3MOD
     I              PCICD1                      P3ICD1
     I              PCICD2                      P3ICD2
     I              PCICD3                      P3ICD3
     I              PCICD4                      P3ICD4
     I              PCICD5                      P3ICD5
     I              PCCPT1                      P3CPT1
     I              PCCPT2                      P3CPT2
     I              PCCPT3                      P3CPT3
     I              PCCPT4                      P3CPT4
     I              PCCPT5                      P3CPT5
     I              PCCPT6                      P3CPT6
     I              PCCPT7                      P3CPT7
     I              PCCPT8                      P3CPT8
     I              PCSPD1                      P3SPD1
     I              PCSPD2                      P3SPD2
     I              PCSPD3                      P3SPD3
     I              PCSPD4                      P3SPD4
     I              PCSPD5                      P3SPD5
     I              PCSPD6                      P3SPD6
     I              PCSPD7                      P3SPD7
     I              PCSPD8                      P3SPD8
     I              PCEPD1                      P3EPD1
     I              PCEPD2                      P3EPD2
     I              PCEPD3                      P3EPD3
     I              PCEPD4                      P3EPD4
     I              PCEPD5                      P3EPD5
     I              PCEPD6                      P3EPD6
     I              PCEPD7                      P3EPD7
     I              PCEPD8                      P3EPD8
     I              PCA#V1                      P3A#V1
     I              PCA#V2                      P3A#V2
     I              PCA#V3                      P3A#V3
     I              PCA#V4                      P3A#V4
     I              PCA#V5                      P3A#V5
     I              PCA#V6                      P3A#V6
     I              PCA#V7                      P3A#V7
     I              PCA#V8                      P3A#V8
     I              PCTOS1                      P3TOS1
     I              PCTOS2                      P3TOS2
     I              PCTOS3                      P3TOS3
     I              PCTOS4                      P3TOS4
     I              PCTOS5                      P3TOS5
     I              PCTOS6                      P3TOS6
     I              PCTOS7                      P3TOS7
     I              PCTOS8                      P3TOS8
     I              PCINTR                      P3INTR
     I              PCPASG                      P3PASG
     I              PCREQD                      P3REQD
     I              PCCERD                      P3CERD
     I              PCRECD                      P3RECD
     I              PCPOS                       P3POS
     I              PCUSER                      P3USER
     I              PCPGM                       P3PGM
     I              PCTIME                      P3TIM
     I              PCDT                        P3DT
     I              PCIUSR                      P3IUSR
     I              PCIDT                       P3IDT
     I              PCENTD                      P3ENTD
     I              PCADST                      P3ADST
     I              PCTYAD                      P3TYAD
     I              PCPARF                      P3PARF
     I              PCTPHY                      P3TPHY
     I              PCTPHM                      P3TPHM
     I              PC2OPN                      P32OPN
     I              PC2DT                       P32DT
     I              PC2PHY                      P32PHY
     I              PC2PHM                      P32PHM
     I              PC2CSC                      P32CSC
     I              PCIME                       P3IME
     I              PCIMDT                      P3IMDT
     I              PCIPHY                      P3IPHY
     I              PCIPHM                      P3IPHM
     I              PCICSC                      P3ICSC
     I              PCRINJ                      P3RINJ
     I              PCHMHL                      P3HMHL
     I              PCCAT                       P3CAT
     I              PCLOST                      P3LOST
     I              PCAGE                       P3AGE

     C*
     C* COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
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
     C*   KEY LIST FOR THE CLAIM DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    P2CO#
     C                   KFLD                    P2SUB#
     C                   KFLD                    P2FND
     C                   KFLD                    P2FYR
     C                   KFLD                    P2CASE
     C                   KFLD                    EOEOB#
     C*
     C*   KEY LIST FOR THE EOMB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    P2CO#
     C                   KFLD                    P2SUB#
     C                   KFLD                    P2FND
     C                   KFLD                    P2FYR
     C                   KFLD                    P2CASE
     C                   KFLD                    P2FEIN
     C                   KFLD                    P2MOD
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE PRE-CERT FILE (PPC02)
     C*
     C     PCKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*   KEY LIST FOR THE PRE-CERT FILE (PPC03)
     C*
     C     PCKY1         KLIST
     C                   KFLD                    PCCERT
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    EMEMP#        HLEMP#                         5.0N
     C     *LIKE         DEFINE    SMNAME        XXSBNM                         40A
     C     *LIKE         DEFINE    FNNAME        XXFNNM                         40A
     C     *LIKE         DEFINE    EMNAME        XXEMNM                         40A
     C     *LIKE         DEFINE    CMCASE        HLCASE                         7.0N
     C     *LIKE         DEFINE    CAFDAT        XXFDAT                         7.0N
     C     *LIKE         DEFINE    CATDAT        XXTDAT                         7.0N
     C*
     C                   MOVE      *BLANKS       XXPRT             1            1A
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           MOVE *BLANKS   XXCNAM 40        40A
     C                   MOVE      *BLANKS       XXCNAM           39            40A
     C*** END   CHG *** 03/07/00 **************************************
     C                   MOVE      *BLANKS       XXMGMT            1            1A
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           Z-ADD*ZEROS    XXFRDT  60       6.0N
     C**********           Z-ADD*ZEROS    XXTODT  60       6.0N
     C                   Z-ADD     *ZEROS        XXFRDT            8 0          6.0N
     C                   Z-ADD     *ZEROS        XXTODT            8 0          6.0N
     C*** END   CHG *** 03/07/00 **************************************
     C                   Z-ADD     *ZEROS        CSCNT             5 0          5.0N
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           Z-ADD*ZEROS    XXACDT  60       6.0N
     C                   Z-ADD     *ZEROS        XXACDT            8 0          6.0N
     C*** END   CHG *** 03/07/00 **************************************
     C                   Z-ADD     *ZEROS        XXCSRC            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXDIFF            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXTACD            5 0          5.0N
     C* PRE-CERT
     C                   Z-ADD     *ZEROS        XPCERT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PUREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PURECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        PEREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PERECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PASURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PACHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRIREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRIREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PA2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        PD2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        PP2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        PDIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        PPIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PALIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TP2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        TPTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PUTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PUTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PETACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PETSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRIACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRISAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMSAV            5 0          5.0N
     C* RE-CERT
     C                   Z-ADD     *ZEROS        XRCERT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RUREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RURECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        REREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RERECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RASURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RACHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRIREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRIREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RA2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        RD2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        RP2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        RDIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        RPIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RALIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TREMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TR2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        TRTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RUTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RUTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RETACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RETSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRIACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRISAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMSAV            5 0          5.0N
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           MOVE QQFRDT    X2FRDT  80
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C*** END   CHG *** 03/07/00 **************************************
     C*
     C*** BEGIN DEL *** 03/07/00 **************************************
     C**********           Z-ADDX2FRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFRDT  60
     C*** END   DEL *** 03/07/00 **************************************
     C*
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           MOVE QQTODT    X2TODT  80
     C                   MOVE      QQTODT        XXTODT            8 0
     C*** END   CHG *** 03/07/00 **************************************
     C*
     C*** BEGIN DEL *** 03/07/00 **************************************
     C**********           Z-ADDX2TODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTODT  60
     C*** END   DEL *** 03/07/00 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C* SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     XXEMP#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN25                          EMP# EQ 0
     C                   ELSE                                                   ELSE XXEMP#
     C                   MOVE      '0'           *IN25                          EMP# NE 0
     C                   END                                                    END XXEMP#
     C*
     C                   MOVE      '0'           *IN26
     C*
     C* GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* GET THE SUBSCRIBER INFORMATION FOR THE HEADING
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C* GET THE FUND INFORMATION FOR THE HEADING
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C* GET THE EMPLOYER INFORMATION FOR THE HEADING
     C*
     C     *IN25         IFNE      '0'
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   END
     C                   END
     C*
     C* GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C* PRINT HEADINGS
     C*
     C  N25              EXCEPT    HEAD
     C*
     C* PROCESS THE CORRECT LOGICAL
     C*
     C                   READ      PPC01                                  50
     C*
     C* IF NO RECORDS PRINT ERROR MESSAGE
     C*
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C*
     C* START LOOP
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* CHAIN TO CLAIM FILE TO COMPARE EMP# TO SELECTED EMP#
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C   25CMEMP#        IFNE      HLEMP#
     C   26              EXCEPT    TOT
     C   26              EXSR      PTOTAL
     C   26              EXSR      RTOTAL
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMEMP#        HLEMP#
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   END
     C   26              EXCEPT    HEAD
     C                   MOVE      '1'           *IN26
     C                   END
     C*
     C     CMEMP#        IFEQ      XXEMP#
     C*
     C* ACCUMULATE THE TOTAL CASES PROCESSED FOR PRINTING
     C*
     C     CMCASE        IFNE      HLCASE
     C                   ADD       1             CSCNT
     C                   END                                                    END CMCASE
     C*
     C* CHAIN TO CLAIMANT DEMOGRAPHICS FILE AND GET THE CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* COMBINE CLAIMANT FIRST AND LAST NAME
     C*
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCNAM
     C                   ELSE                                                   ELSE *IN60
     C                   MOVEL     *BLANKS       XXCNAM
     C                   END                                                    END *IN60
     C*
     C* CONVERT FORMAT OF ACCIDENT DATE FROM YY/MM/DD TO MM/DD/YY
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END   CHG *** 03/07/00 **************************************
     C                   MOVEL     SRDTO         XXACDT
     C*
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           Z-ADDCMFYR     XXYR    20
     C                   Z-ADD     CMFYR         XXYR              3 0
     C*** END   CHG *** 03/07/00 **************************************
     C*
     C* MOVE CMSS# TO DATA STRUCTURE
     C*
     C                   MOVE      CMSS#         XXSS#
     C*
     C* CHAIN TO PRE-CERT FILE (PPC03) TO ACCUMULATE # OF RE-CERTS
     C*
     C                   Z-ADD     *ZEROS        XXCSRC
     C     PCKY1         CHAIN     PPC03                              40
     C     *IN40         DOWEQ     '0'
     C     P3EXT         IFNE      0
     C                   ADD       1             XXCSRC
     C                   END
     C     PCKY1         READE     PPC03                                  40
     C                   END
     C*
     C* PRINT THE DETAIL LINE FOR 1ST PART OF REPORT
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END                                                    END *INOA
     C                   EXCEPT    DET
     C*
     C* CHECK THE CASE MANAGEMENT FLAG
     C*
     C     CMMGMT        IFEQ      'Y'
     C                   MOVE      'Y'           XXMGMT
     C                   ELSE                                                   ELSE EQ 'Y'
     C                   MOVE      *BLANKS       XXMGMT
     C                   END                                                    END CMMGMT
     C*
     C* CHAIN TO PRE-CERT FILE (PPC02) TO ACCUMULATE ALL PRE/RE CERT
     C* AMOUNTS FOR THIS CASE
     C*
     C     CMCASE        IFNE      HLCASE
     C     PCKY          CHAIN     PPC02                              40
     C     *IN40         DOWEQ     '0'
     C*
     C     P2EXT         IFEQ      0
     C*PRE-CERT
     C*
     C* GET THE TOTAL NUMBER OF ACTUAL DAYS FROM THE DETAIL CHARGE FILE
     C*
     C                   Z-ADD     *ZEROS        XXTACD
     C     EOKEY         CHAIN     PEO01                              20
     C     *IN20         DOWEQ     '0'
     C     EOCERT        IFEQ      P2CERT
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
     C                   ADD       1             XPCERT                         PRE-CERT
     C                   ADD       P2REQD        TPREQD
     C                   ADD       P2RECD        TPRECD
     C                   ADD       XXTACD        TPTACD
     C*
     C* CHECK THE PRE-CERT LIABILITY AND ACCUMULATE AMOUNTS FOR EACH
     C* TYPE OF LIBAILITY
     C*
     C     P2LIAB        IFEQ      'A'
     C*
     C                   ADD       1             PALIAB
     C*
     C* IF THIS CASE HAS CASE MANAGEMENT ASSIGNED ACCUMULATE TOTALS
     C*
     C     XXMGMT        IFEQ      'Y'
     C                   ADD       1             PAMGMT
     C                   ADD       P2REQD        PMGREQ
     C                   ADD       P2RECD        PMGREC
     C                   ADD       XXTACD        PMGACD
     C                   END                                                    END XXMGMT
     C*
     C* CHECK AND ACCUMULATE ADMISSION STATUS
     C*
     C     P2ADST        IFEQ      'L'
     C                   ADD       1             PAELEC
     C                   ADD       P2REQD        PLREQD
     C                   ADD       P2RECD        PLRECD
     C                   ADD       XXTACD        PLTACD
     C                   ELSE
     C     P2ADST        IFEQ      'U'
     C                   ADD       1             PAURGT
     C                   ADD       P2REQD        PUREQD
     C                   ADD       P2RECD        PURECD
     C                   ADD       XXTACD        PUTACD
     C                   ELSE
     C     P2ADST        IFEQ      'E'
     C                   ADD       1             PAEMER
     C                   ADD       P2REQD        PEREQD
     C                   ADD       P2RECD        PERECD
     C                   ADD       XXTACD        PETACD
     C                   END                                                    END EQ 'E'
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'L'
     C*
     C* CHECK AND ACCUMULATE THE TYPE OF ADMISSION
     C*
     C     P2TYAD        IFEQ      'M'
     C                   ADD       1             PAMEDI
     C                   ADD       P2REQD        PMREQD
     C                   ADD       P2RECD        PMRECD
     C                   ADD       XXTACD        PMTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'S'
     C                   ADD       1             PASURG
     C                   ADD       P2REQD        PSREQD
     C                   ADD       P2RECD        PSRECD
     C                   ADD       XXTACD        PSTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'Y'
     C                   ADD       1             PAPSYC
     C                   ADD       P2REQD        PYREQD
     C                   ADD       P2RECD        PYRECD
     C                   ADD       XXTACD        PYTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'P'
     C                   ADD       1             PAPHYS
     C                   ADD       P2REQD        PPREQD
     C                   ADD       P2RECD        PPRECD
     C                   ADD       XXTACD        PPTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'C'
     C                   ADD       1             PACHIR
     C                   ADD       P2REQD        PHREQD
     C                   ADD       P2RECD        PHRECD
     C                   ADD       XXTACD        PHTACD
     C                   END                                                    END EQ 'C'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'Y'
     C                   END                                                    END EQ 'S'
     C                   END                                                    END EQ 'M'
     C*
     C* CHECK FOR P/A/ REFFERAL
     C*
     C     P2PARF        IFEQ      'Y'
     C                   ADD       1             PAPARF
     C                   ADD       P2REQD        PARREQ
     C                   ADD       P2RECD        PARREC
     C                   ADD       XXTACD        PARACD
     C                   END                                                    END P2PARY=Y
     C*
     C* CHECK FOR RE-INJURY
     C*
     C     P2RINJ        IFEQ      'Y'
     C                   ADD       1             PARINJ
     C                   ADD       P2REQD        PRIREQ
     C                   ADD       P2RECD        PRIREC
     C                   ADD       XXTACD        PRIACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR 2ND OPINION
     C*
     C     P22OPN        IFEQ      'Y'
     C                   ADD       1             PA2OPN
     C                   ADD       P2REQD        P2OREQ
     C                   ADD       P2RECD        P2OREC
     C                   ADD       XXTACD        P2OACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR IME
     C*
     C     P2IME         IFEQ      'Y'
     C                   ADD       1             PAIME
     C                   ADD       P2REQD        PIMREQ
     C                   ADD       P2RECD        PIMREC
     C                   ADD       XXTACD        PIMACD
     C                   END                                                    END P2RINJ=Y
     C*
     C                   ELSE                                                   ELSE P2LIAB
     C*
     C     P2LIAB        IFEQ      'D'
     C*
     C                   ADD       1             PDLIAB
     C*
     C* IF THIS CASE HAS CASE MANAGEMENT ASSIGNED ACCUMULATE TOTALS
     C*
     C     XXMGMT        IFEQ      'Y'
     C                   ADD       1             PDMGMT
     C                   ADD       P2REQD        PMGREQ
     C                   ADD       P2RECD        PMGREC
     C                   ADD       XXTACD        PMGACD
     C                   END                                                    END XXMGMT
     C*
     C* CHECK AND ACCUMULATE ADMISSION STATUS
     C*
     C     P2ADST        IFEQ      'L'
     C                   ADD       1             PDELEC
     C                   ADD       P2REQD        PLREQD
     C                   ADD       P2RECD        PLRECD
     C                   ADD       XXTACD        PLTACD
     C                   ELSE
     C     P2ADST        IFEQ      'U'
     C                   ADD       1             PDURGT
     C                   ADD       P2REQD        PUREQD
     C                   ADD       P2RECD        PURECD
     C                   ADD       XXTACD        PUTACD
     C                   ELSE
     C     P2ADST        IFEQ      'E'
     C                   ADD       1             PDEMER
     C                   ADD       P2REQD        PEREQD
     C                   ADD       P2RECD        PERECD
     C                   ADD       XXTACD        PETACD
     C                   END                                                    END EQ 'E'
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'L'
     C*
     C* CHECK AND ACCUMULATE THE TYPE OF ADMISSION
     C*
     C     P2TYAD        IFEQ      'M'
     C                   ADD       1             PDMEDI
     C                   ADD       P2REQD        PMREQD
     C                   ADD       P2RECD        PMRECD
     C                   ADD       XXTACD        PMTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'S'
     C                   ADD       1             PDSURG
     C                   ADD       P2REQD        PSREQD
     C                   ADD       P2RECD        PSRECD
     C                   ADD       XXTACD        PSTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'Y'
     C                   ADD       1             PDPSYC
     C                   ADD       P2REQD        PYREQD
     C                   ADD       P2RECD        PYRECD
     C                   ADD       XXTACD        PYTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'P'
     C                   ADD       1             PDPHYS
     C                   ADD       P2REQD        PPREQD
     C                   ADD       P2RECD        PPRECD
     C                   ADD       XXTACD        PPTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'C'
     C                   ADD       1             PDCHIR
     C                   ADD       P2REQD        PHREQD
     C                   ADD       P2RECD        PHRECD
     C                   ADD       XXTACD        PHTACD
     C                   END                                                    END EQ 'C'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'Y'
     C                   END                                                    END EQ 'S'
     C                   END                                                    END EQ 'M'
     C*
     C* CHECK FOR P/A/ REFFERAL
     C*
     C     P2PARF        IFEQ      'Y'
     C                   ADD       1             PDPARF
     C                   ADD       P2REQD        PARREQ
     C                   ADD       P2RECD        PARREC
     C                   ADD       XXTACD        PARACD
     C                   END                                                    END P2PARY=Y
     C*
     C* CHECK FOR RE-INJURY
     C*
     C     P2RINJ        IFEQ      'Y'
     C                   ADD       1             PDRINJ
     C                   ADD       P2REQD        PRIREQ
     C                   ADD       P2RECD        PRIREC
     C                   ADD       XXTACD        PRIACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR 2ND OPINION
     C*
     C     P22OPN        IFEQ      'Y'
     C                   ADD       1             PD2OPN
     C                   ADD       P2REQD        P2OREQ
     C                   ADD       P2RECD        P2OREC
     C                   ADD       XXTACD        P2OACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR IME
     C*
     C     P2IME         IFEQ      'Y'
     C                   ADD       1             PDIME
     C                   ADD       P2REQD        PIMREQ
     C                   ADD       P2RECD        PIMREC
     C                   ADD       XXTACD        PIMACD
     C                   END                                                    END P2RINJ=Y
     C*
     C                   ELSE                                                   ELSE P2LIAB
     C*
     C     P2LIAB        IFEQ      'P'
     C*
     C                   ADD       1             PPLIAB
     C*
     C* IF THIS CASE HAS CASE MANAGEMENT ASSIGNED ACCUMULATE TOTALS
     C*
     C     XXMGMT        IFEQ      'Y'
     C                   ADD       1             PPMGMT
     C                   ADD       P2REQD        PMGREQ
     C                   ADD       P2RECD        PMGREC
     C                   ADD       XXTACD        PMGACD
     C                   END                                                    END XXMGMT
     C*
     C* CHECK AND ACCUMULATE ADMISSION STATUS
     C*
     C     P2ADST        IFEQ      'L'
     C                   ADD       1             PPELEC
     C                   ADD       P2REQD        PLREQD
     C                   ADD       P2RECD        PLRECD
     C                   ADD       XXTACD        PLTACD
     C                   ELSE
     C     P2ADST        IFEQ      'U'
     C                   ADD       1             PPURGT
     C                   ADD       P2REQD        PUREQD
     C                   ADD       P2RECD        PURECD
     C                   ADD       XXTACD        PUTACD
     C                   ELSE
     C     P2ADST        IFEQ      'E'
     C                   ADD       1             PPEMER
     C                   ADD       P2REQD        PEREQD
     C                   ADD       P2RECD        PERECD
     C                   ADD       XXTACD        PETACD
     C                   END                                                    END EQ 'E'
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'L'
     C*
     C* CHECK AND ACCUMULATE THE TYPE OF ADMISSION
     C*
     C     P2TYAD        IFEQ      'M'
     C                   ADD       1             PPMEDI
     C                   ADD       P2REQD        PMREQD
     C                   ADD       P2RECD        PMRECD
     C                   ADD       XXTACD        PMTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'S'
     C                   ADD       1             PPSURG
     C                   ADD       P2REQD        PSREQD
     C                   ADD       P2RECD        PSRECD
     C                   ADD       XXTACD        PSTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'Y'
     C                   ADD       1             PPPSYC
     C                   ADD       P2REQD        PYREQD
     C                   ADD       P2RECD        PYRECD
     C                   ADD       XXTACD        PYTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'P'
     C                   ADD       1             PPPHYS
     C                   ADD       P2REQD        PPREQD
     C                   ADD       P2RECD        PPRECD
     C                   ADD       XXTACD        PPTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'C'
     C                   ADD       1             PPCHIR
     C                   ADD       P2REQD        PHREQD
     C                   ADD       P2RECD        PHRECD
     C                   ADD       XXTACD        PHTACD
     C                   END                                                    END EQ 'C'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'Y'
     C                   END                                                    END EQ 'S'
     C                   END                                                    END EQ 'M'
     C*
     C* CHECK FOR P/A/ REFFERAL
     C*
     C     P2PARF        IFEQ      'Y'
     C                   ADD       1             PPPARF
     C                   ADD       P2REQD        PARREQ
     C                   ADD       P2RECD        PARREC
     C                   ADD       XXTACD        PARACD
     C                   END                                                    END P2PARY=Y
     C*
     C* CHECK FOR RE-INJURY
     C*
     C     P2RINJ        IFEQ      'Y'
     C                   ADD       1             PPRINJ
     C                   ADD       P2REQD        PRIREQ
     C                   ADD       P2RECD        PRIREC
     C                   ADD       XXTACD        PRIACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR 2ND OPINION
     C*
     C     P22OPN        IFEQ      'Y'
     C                   ADD       1             PP2OPN
     C                   ADD       P2REQD        P2OREQ
     C                   ADD       P2RECD        P2OREC
     C                   ADD       XXTACD        P2OACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR IME
     C*
     C     P2IME         IFEQ      'Y'
     C                   ADD       1             PPIME
     C                   ADD       P2REQD        PIMREQ
     C                   ADD       P2RECD        PIMREC
     C                   ADD       XXTACD        PIMACD
     C                   END                                                    END P2RINJ=Y
     C*
     C                   END                                                    END P2LIAB=P
     C                   END                                                    END P2LIAB=D
     C                   END                                                    END P2LIAB=A
     C*
     C                   ELSE
     C*RE-CERT
     C*
     C* GET THE TOTAL NUMBER OF ACTUAL DAYS FROM THE DETAIL CHARGE FILE
     C*
     C     EOKEY         CHAIN     PEO01                              20
     C     *IN20         DOWEQ     '0'
     C     EOCERT        IFEQ      P2CERT
     C     EOEXT         ANDEQ     P2EXT
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
     C                   ADD       1             XRCERT                         RE-CERT
     C                   ADD       P2REQD        TRREQD
     C                   ADD       P2RECD        TRRECD
     C                   ADD       XXTACD        TRTACD
     C*
     C* CHECK THE PRE-CERT LIABILITY AND ACCUMULATE AMOUNTS FOR EACH
     C* TYPE OF LIBAILITY
     C*
     C     P2LIAB        IFEQ      'A'
     C*
     C                   ADD       1             RALIAB
     C*
     C* IF THIS CASE HAS CASE MANAGEMENT ASSIGNED ACCUMULATE TOTALS
     C*
     C     XXMGMT        IFEQ      'Y'
     C                   ADD       1             RAMGMT
     C                   ADD       P2REQD        RMGREQ
     C                   ADD       P2RECD        RMGREC
     C                   ADD       XXTACD        RMGACD
     C                   END                                                    END XXMGMT
     C*
     C* CHECK AND ACCUMULATE ADMISSION STATUS
     C*
     C     P2ADST        IFEQ      'L'
     C                   ADD       1             RAELEC
     C                   ADD       P2REQD        RLREQD
     C                   ADD       P2RECD        RLRECD
     C                   ADD       XXTACD        RLTACD
     C                   ELSE
     C     P2ADST        IFEQ      'U'
     C                   ADD       1             RAURGT
     C                   ADD       P2REQD        RUREQD
     C                   ADD       P2RECD        RURECD
     C                   ADD       XXTACD        RUTACD
     C                   ELSE
     C     P2ADST        IFEQ      'E'
     C                   ADD       1             RAEMER
     C                   ADD       P2REQD        REREQD
     C                   ADD       P2RECD        RERECD
     C                   ADD       XXTACD        RETACD
     C                   END                                                    END EQ 'E'
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'L'
     C*
     C* CHECK AND ACCUMULATE THE TYPE OF ADMISSION
     C*
     C     P2TYAD        IFEQ      'M'
     C                   ADD       1             RAMEDI
     C                   ADD       P2REQD        RMREQD
     C                   ADD       P2RECD        RMRECD
     C                   ADD       XXTACD        RMTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'S'
     C                   ADD       1             RASURG
     C                   ADD       P2REQD        RSREQD
     C                   ADD       P2RECD        RSRECD
     C                   ADD       XXTACD        RSTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'Y'
     C                   ADD       1             RAPSYC
     C                   ADD       P2REQD        RYREQD
     C                   ADD       P2RECD        RYRECD
     C                   ADD       XXTACD        RYTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'P'
     C                   ADD       1             RAPHYS
     C                   ADD       P2REQD        RPREQD
     C                   ADD       P2RECD        RPRECD
     C                   ADD       XXTACD        RPTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'C'
     C                   ADD       1             RACHIR
     C                   ADD       P2REQD        RHREQD
     C                   ADD       P2RECD        RHRECD
     C                   ADD       XXTACD        RHTACD
     C                   END                                                    END EQ 'C'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'Y'
     C                   END                                                    END EQ 'S'
     C                   END                                                    END EQ 'M'
     C*
     C* CHECK FOR P/A/ REFFERAL
     C*
     C     P2PARF        IFEQ      'Y'
     C                   ADD       1             RAPARF
     C                   ADD       P2REQD        RARREQ
     C                   ADD       P2RECD        RARREC
     C                   ADD       XXTACD        RARACD
     C                   END                                                    END P2PARY=Y
     C*
     C* CHECK FOR RE-INJURY
     C*
     C     P2RINJ        IFEQ      'Y'
     C                   ADD       1             RARINJ
     C                   ADD       P2REQD        RRIREQ
     C                   ADD       P2RECD        RRIREC
     C                   ADD       XXTACD        RRIACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR 2ND OPINION
     C*
     C     P22OPN        IFEQ      'Y'
     C                   ADD       1             RA2OPN
     C                   ADD       P2REQD        R2OREQ
     C                   ADD       P2RECD        R2OREC
     C                   ADD       XXTACD        R2OACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR IME
     C*
     C     P2IME         IFEQ      'Y'
     C                   ADD       1             RAIME
     C                   ADD       P2REQD        RIMREQ
     C                   ADD       P2RECD        RIMREC
     C                   ADD       XXTACD        RIMACD
     C                   END                                                    END P2RINJ=Y
     C*
     C                   ELSE                                                   ELSE P2LIAB
     C*
     C     P2LIAB        IFEQ      'D'
     C*
     C                   ADD       1             RDLIAB
     C*
     C* IF THIS CASE HAS CASE MANAGEMENT ASSIGNED ACCUMULATE TOTALS
     C*
     C     XXMGMT        IFEQ      'Y'
     C                   ADD       1             RDMGMT
     C                   ADD       P2REQD        RMGREQ
     C                   ADD       P2RECD        RMGREC
     C                   ADD       XXTACD        RMGACD
     C                   END                                                    END XXMGMT
     C*
     C* CHECK AND ACCUMULATE ADMISSION STATUS
     C*
     C     P2ADST        IFEQ      'L'
     C                   ADD       1             RDELEC
     C                   ADD       P2REQD        RLREQD
     C                   ADD       P2RECD        RLRECD
     C                   ADD       XXTACD        RLTACD
     C                   ELSE
     C     P2ADST        IFEQ      'U'
     C                   ADD       1             RDURGT
     C                   ADD       P2REQD        RUREQD
     C                   ADD       P2RECD        RURECD
     C                   ADD       XXTACD        RUTACD
     C                   ELSE
     C     P2ADST        IFEQ      'E'
     C                   ADD       1             RDEMER
     C                   ADD       P2REQD        REREQD
     C                   ADD       P2RECD        RERECD
     C                   ADD       XXTACD        RETACD
     C                   END                                                    END EQ 'E'
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'L'
     C*
     C* CHECK AND ACCUMULATE THE TYPE OF ADMISSION
     C*
     C     P2TYAD        IFEQ      'M'
     C                   ADD       1             RDMEDI
     C                   ADD       P2REQD        RMREQD
     C                   ADD       P2RECD        RMRECD
     C                   ADD       XXTACD        RMTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'S'
     C                   ADD       1             RDSURG
     C                   ADD       P2REQD        RSREQD
     C                   ADD       P2RECD        RSRECD
     C                   ADD       XXTACD        RSTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'Y'
     C                   ADD       1             RDPSYC
     C                   ADD       P2REQD        RYREQD
     C                   ADD       P2RECD        RYRECD
     C                   ADD       XXTACD        RYTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'P'
     C                   ADD       1             RDPHYS
     C                   ADD       P2REQD        RPREQD
     C                   ADD       P2RECD        RPRECD
     C                   ADD       XXTACD        RPTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'C'
     C                   ADD       1             RDCHIR
     C                   ADD       P2REQD        RHREQD
     C                   ADD       P2RECD        RHRECD
     C                   ADD       XXTACD        RHTACD
     C                   END                                                    END EQ 'C'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'Y'
     C                   END                                                    END EQ 'S'
     C                   END                                                    END EQ 'M'
     C*
     C* CHECK FOR P/A/ REFFERAL
     C*
     C     P2PARF        IFEQ      'Y'
     C                   ADD       1             RDPARF
     C                   ADD       P2REQD        RARREQ
     C                   ADD       P2RECD        RARREC
     C                   ADD       XXTACD        RARACD
     C                   END                                                    END P2PARY=Y
     C*
     C* CHECK FOR RE-INJURY
     C*
     C     P2RINJ        IFEQ      'Y'
     C                   ADD       1             RDRINJ
     C                   ADD       P2REQD        RRIREQ
     C                   ADD       P2RECD        RRIREC
     C                   ADD       XXTACD        RRIACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR 2ND OPINION
     C*
     C     P22OPN        IFEQ      'Y'
     C                   ADD       1             RD2OPN
     C                   ADD       P2REQD        R2OREQ
     C                   ADD       P2RECD        R2OREC
     C                   ADD       XXTACD        R2OACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR IME
     C*
     C     P2IME         IFEQ      'Y'
     C                   ADD       1             RDIME
     C                   ADD       P2REQD        RIMREQ
     C                   ADD       P2RECD        RIMREC
     C                   ADD       XXTACD        RIMACD
     C                   END                                                    END P2RINJ=Y
     C*
     C                   ELSE                                                   ELSE P2LIAB
     C*
     C     P2LIAB        IFEQ      'P'
     C*
     C                   ADD       1             RPLIAB
     C*
     C* IF THIS CASE HAS CASE MANAGEMENT ASSIGNED ACCUMULATE TOTALS
     C*
     C     XXMGMT        IFEQ      'Y'
     C                   ADD       1             RPMGMT
     C                   ADD       P2REQD        RMGREQ
     C                   ADD       P2RECD        RMGREC
     C                   ADD       XXTACD        RMGACD
     C                   END                                                    END XXMGMT
     C*
     C* CHECK AND ACCUMULATE ADMISSION STATUS
     C*
     C     P2ADST        IFEQ      'L'
     C                   ADD       1             RPELEC
     C                   ADD       P2REQD        RLREQD
     C                   ADD       P2RECD        RLRECD
     C                   ADD       XXTACD        RLTACD
     C                   ELSE
     C     P2ADST        IFEQ      'U'
     C                   ADD       1             RPURGT
     C                   ADD       P2REQD        RUREQD
     C                   ADD       P2RECD        RURECD
     C                   ADD       XXTACD        RUTACD
     C                   ELSE
     C     P2ADST        IFEQ      'E'
     C                   ADD       1             RPEMER
     C                   ADD       P2REQD        REREQD
     C                   ADD       P2RECD        RERECD
     C                   ADD       XXTACD        RETACD
     C                   END                                                    END EQ 'E'
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'L'
     C*
     C* CHECK AND ACCUMULATE THE TYPE OF ADMISSION
     C*
     C     P2TYAD        IFEQ      'M'
     C                   ADD       1             RPMEDI
     C                   ADD       P2REQD        RMREQD
     C                   ADD       P2RECD        RMRECD
     C                   ADD       XXTACD        RMTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'S'
     C                   ADD       1             RPSURG
     C                   ADD       P2REQD        RSREQD
     C                   ADD       P2RECD        RSRECD
     C                   ADD       XXTACD        RSTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'Y'
     C                   ADD       1             RPPSYC
     C                   ADD       P2REQD        RYREQD
     C                   ADD       P2RECD        RYRECD
     C                   ADD       XXTACD        RYTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'P'
     C                   ADD       1             RPPHYS
     C                   ADD       P2REQD        RPREQD
     C                   ADD       P2RECD        RPRECD
     C                   ADD       XXTACD        RPTACD
     C                   ELSE
     C     P2TYAD        IFEQ      'C'
     C                   ADD       1             RPCHIR
     C                   ADD       P2REQD        RHREQD
     C                   ADD       P2RECD        RHRECD
     C                   ADD       XXTACD        RHTACD
     C                   END                                                    END EQ 'C'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'Y'
     C                   END                                                    END EQ 'S'
     C                   END                                                    END EQ 'M'
     C*
     C* CHECK FOR P/A/ REFFERAL
     C*
     C     P2PARF        IFEQ      'Y'
     C                   ADD       1             RPPARF
     C                   ADD       P2REQD        RARREQ
     C                   ADD       P2RECD        RARREC
     C                   ADD       XXTACD        RARACD
     C                   END                                                    END P2PARY=Y
     C*
     C* CHECK FOR RE-INJURY
     C*
     C     P2RINJ        IFEQ      'Y'
     C                   ADD       1             RPRINJ
     C                   ADD       P2REQD        RRIREQ
     C                   ADD       P2RECD        RRIREC
     C                   ADD       XXTACD        RRIACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR 2ND OPINION
     C*
     C     P22OPN        IFEQ      'Y'
     C                   ADD       1             RP2OPN
     C                   ADD       P2REQD        R2OREQ
     C                   ADD       P2RECD        R2OREC
     C                   ADD       XXTACD        R2OACD
     C                   END                                                    END P2RINJ=Y
     C*
     C* CHECK FOR IME
     C*
     C     P2IME         IFEQ      'Y'
     C                   ADD       1             RPIME
     C                   ADD       P2REQD        RIMREQ
     C                   ADD       P2RECD        RIMREC
     C                   ADD       XXTACD        RIMACD
     C                   END                                                    END P2RINJ=Y
     C*
     C                   END                                                    END P2LIAB=P
     C                   END                                                    END P2LIAB=D
     C                   END                                                    END P2LIAB=A
     C*
     C                   END                                                    END P2EXT =0
     C*
     C     PCKY          READE     PPC02                                  40
     C                   END                                                    END *IN40
     C                   Z-ADD     CMCASE        HLCASE
     C                   END                                                    END EQ CMCASE
     C*
     C                   END                                                    END CMEMP#
     C                   END                                                    END *IN60
     C*
     C* GET THE PRE-CERT RECORD
     C*
     C                   READ      PPC01                                  50
     C*
     C                   END                                                    END DOW 50
     C*
     C* PRINT HEADINGS AND DETAIL FOR 2ND PART OF REPORT
     C*
     C                   EXCEPT    TOT
     C*
     C* ACCUMULATE TOTAL PRE/RE-CERT ACTIVITY FOR REPORT
     C*
     C                   EXSR      PTOTAL
     C                   EXSR      RTOTAL
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
         xxPrt = '0';
         Chain orKey POR01;
         If %found;
            Except Route;
         Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C* PTOTAL - SUBROUTINE TO CALCULATE PRE-CERT TOTALS
     C*****************************************************************
     C     PTOTAL        BEGSR
     C*
     C     TPTACD        IFGT      0
     C     TPRECD        SUB       TPTACD        TPTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        TPTSAV
     C                   END
     C     TPTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        TPTSAV
     C                   END                                                    END TPTSAV
     C*
     C     PAELEC        ADD       PDELEC        TPELEC
     C     TPELEC        ADD       PPELEC        TPELEC
     C*
     C     PLTACD        IFGT      0
     C     PLRECD        SUB       PLTACD        PLTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PLTSAV
     C                   END
     C     PLTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PLTSAV
     C                   END                                                    END PLTSAV
     C*
     C     PAURGT        ADD       PDURGT        TPURGT
     C     TPURGT        ADD       PPURGT        TPURGT
     C*
     C     PUTACD        IFGT      0
     C     PURECD        SUB       PUTACD        PUTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PUTSAV
     C                   END
     C     PUTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PUTSAV
     C                   END                                                    END PUTSAV
     C*
     C     PAEMER        ADD       PDEMER        TPEMER
     C     TPEMER        ADD       PPEMER        TPEMER
     C*
     C     PETACD        IFGT      0
     C     PERECD        SUB       PETACD        PETSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PETSAV
     C                   END
     C     PETSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PETSAV
     C                   END                                                    END PETSAV
     C*
     C     PAMEDI        ADD       PDMEDI        TPMEDI
     C     TPMEDI        ADD       PPMEDI        TPMEDI
     C*
     C     PMTACD        IFGT      0
     C     PMRECD        SUB       PMTACD        PMTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PMTSAV
     C                   END
     C     PMTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PMTSAV
     C                   END                                                    END PMTSAV
     C*
     C     PASURG        ADD       PDSURG        TPSURG
     C     TPSURG        ADD       PPSURG        TPSURG
     C*
     C     PSTACD        IFGT      0
     C     PSRECD        SUB       PSTACD        PSTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PSTSAV
     C                   END
     C     PSTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PSTSAV
     C                   END                                                    END PSTSAV
     C*
     C     PAPSYC        ADD       PDPSYC        TPPSYC
     C     TPPSYC        ADD       PPPSYC        TPPSYC
     C*
     C     PYTACD        IFGT      0
     C     PYRECD        SUB       PYTACD        PYTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PYTSAV
     C                   END
     C     PYTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PYTSAV
     C                   END                                                    END PYTSAV
     C*
     C     PAPHYS        ADD       PDPHYS        TPPHYS
     C     TPPHYS        ADD       PPPHYS        TPPHYS
     C*
     C     PPTACD        IFGT      0
     C     PPRECD        SUB       PPTACD        PPTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PPTSAV
     C                   END
     C     PPTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PPTSAV
     C                   END                                                    END PPTSAV
     C*
     C     PACHIR        ADD       PDCHIR        TPCHIR
     C     TPCHIR        ADD       PPCHIR        TPCHIR
     C*
     C     PHTACD        IFGT      0
     C     PHRECD        SUB       PHTACD        PHTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PHTSAV
     C                   END
     C     PHTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PHTSAV
     C                   END                                                    END PHTSAV
     C*
     C     PAPARF        ADD       PDPARF        TPPARF
     C     TPPARF        ADD       PPPARF        TPPARF
     C*
     C     PARACD        IFGT      0
     C     PARREC        SUB       PARACD        PARSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PARSAV
     C                   END
     C     PARSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PARSAV
     C                   END                                                    END PARSAV
     C*
     C     PARINJ        ADD       PDRINJ        TPRINJ
     C     TPRINJ        ADD       PPRINJ        TPRINJ
     C*
     C     PRIACD        IFGT      0
     C     PRIREC        SUB       PRIACD        PRISAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PRISAV
     C                   END
     C     PRISAV        IFLT      0
     C                   Z-ADD     *ZEROS        PRISAV
     C                   END                                                    END PRISAV
     C*
     C     PAMGMT        ADD       PDMGMT        TPMGMT
     C     TPMGMT        ADD       PPMGMT        TPMGMT
     C*
     C     PMGACD        IFGT      0
     C     PMGREC        SUB       PMGACD        PMGSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PMGSAV
     C                   END
     C     PMGSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PMGSAV
     C                   END                                                    END PMGSAV
     C*
     C     PA2OPN        ADD       PD2OPN        TP2OPN
     C     TP2OPN        ADD       PP2OPN        TP2OPN
     C*
     C     P2OACD        IFGT      0
     C     P2OREC        SUB       P2OACD        P2OSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        P2OSAV
     C                   END
     C     P2OSAV        IFLT      0
     C                   Z-ADD     *ZEROS        P2OSAV
     C                   END                                                    END P2OSAV
     C*
     C     PAIME         ADD       PDIME         TPIME
     C     TPIME         ADD       PPIME         TPIME
     C*
     C     PIMACD        IFGT      0
     C     PIMREC        SUB       PIMACD        PIMSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        PIMSAV
     C                   END
     C     PIMSAV        IFLT      0
     C                   Z-ADD     *ZEROS        PIMSAV
     C                   END                                                    END PIMSAV
     C*
     C                   EXCEPT    PTOT
     C*
     C                   Z-ADD     *ZEROS        XPCERT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        PUREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PURECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        PEREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PERECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PASURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PACHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRIREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRIREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PA2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        PD2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        PP2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PAIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        PDIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        PPIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        PALIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        PDLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TP2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        TPTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TPTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PLTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PUTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PUTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PETACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PETSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PSTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PYTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PPTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PHTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PARSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRIACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PRISAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PMGSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        P2OSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        PIMSAV            5 0          5.0N
     C                   ENDSR
     C*****************************************************************
     C* RTOTAL - SUBROUTINE TO CALCULATE RE-CERT TOTALS
     C*****************************************************************
     C     RTOTAL        BEGSR
     C*
     C     TRTACD        IFGT      0
     C     TRRECD        SUB       TRTACD        TRTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        TRTSAV
     C                   END
     C     TRTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        TRTSAV
     C                   END                                                    END TRTSAV
     C*
     C     RAELEC        ADD       RDELEC        TRELEC
     C     TRELEC        ADD       RPELEC        TRELEC
     C*
     C     RLTACD        IFGT      0
     C     RLRECD        SUB       RLTACD        RLTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RLTSAV
     C                   END
     C     RLTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RLTSAV
     C                   END                                                    END RLTSAV
     C*
     C     RAURGT        ADD       RDURGT        TRURGT
     C     TRURGT        ADD       RPURGT        TRURGT
     C*
     C     RUTACD        IFGT      0
     C     RURECD        SUB       RUTACD        RUTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RUTSAV
     C                   END
     C     RUTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RUTSAV
     C                   END                                                    END RUTSAV
     C*
     C     RAEMER        ADD       RDEMER        TREMER
     C     TREMER        ADD       RPEMER        TREMER
     C*
     C     RETACD        IFGT      0
     C     RERECD        SUB       RETACD        RETSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RETSAV
     C                   END
     C     RETSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RETSAV
     C                   END                                                    END RETSAV
     C*
     C     RAMEDI        ADD       RDMEDI        TRMEDI
     C     TRMEDI        ADD       RPMEDI        TRMEDI
     C*
     C     RMTACD        IFGT      0
     C     RMRECD        SUB       RMTACD        RMTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RMTSAV
     C                   END
     C     RMTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RMTSAV
     C                   END                                                    END RMTSAV
     C*
     C     RASURG        ADD       RDSURG        TRSURG
     C     TRSURG        ADD       RPSURG        TRSURG
     C*
     C     RSTACD        IFGT      0
     C     RSRECD        SUB       RSTACD        RSTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RSTSAV
     C                   END
     C     RSTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RSTSAV
     C                   END                                                    END RSTSAV
     C*
     C     RAPSYC        ADD       RDPSYC        TRPSYC
     C     TRPSYC        ADD       RPPSYC        TRPSYC
     C*
     C     RYTACD        IFGT      0
     C     RYRECD        SUB       RYTACD        RYTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RYTSAV
     C                   END
     C     RYTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RYTSAV
     C                   END                                                    END RYTSAV
     C*
     C     RAPHYS        ADD       RDPHYS        TRPHYS
     C     TRPHYS        ADD       RPPHYS        TRPHYS
     C*
     C     RPTACD        IFGT      0
     C     RPRECD        SUB       RPTACD        RPTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RPTSAV
     C                   END
     C     RPTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RPTSAV
     C                   END                                                    END RPTSAV
     C*
     C     RACHIR        ADD       RDCHIR        TRCHIR
     C     TRCHIR        ADD       RPCHIR        TRCHIR
     C*
     C     RHTACD        IFGT      0
     C     RHRECD        SUB       RHTACD        RHTSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RHTSAV
     C                   END
     C     RHTSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RHTSAV
     C                   END                                                    END RHTSAV
     C*
     C     RAPARF        ADD       RDPARF        TRPARF
     C     TRPARF        ADD       RPPARF        TRPARF
     C*
     C     RARACD        IFGT      0
     C     RARREC        SUB       RARACD        RARSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RARSAV
     C                   END
     C     RARSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RARSAV
     C                   END                                                    END RARSAV
     C*
     C     RARINJ        ADD       RDRINJ        TRRINJ
     C     TRRINJ        ADD       RPRINJ        TRRINJ
     C*
     C     RRIACD        IFGT      0
     C     RRIREC        SUB       RRIACD        RRISAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RRISAV
     C                   END
     C     RRISAV        IFLT      0
     C                   Z-ADD     *ZEROS        RRISAV
     C                   END                                                    END RRISAV
     C*
     C     RAMGMT        ADD       RDMGMT        TRMGMT
     C     TRMGMT        ADD       RPMGMT        TRMGMT
     C*
     C     RMGACD        IFGT      0
     C     RMGREC        SUB       RMGACD        RMGSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RMGSAV
     C                   END
     C     RMGSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RMGSAV
     C                   END                                                    END RMGSAV
     C*
     C     RA2OPN        ADD       RD2OPN        TR2OPN
     C     TR2OPN        ADD       RP2OPN        TR2OPN
     C*
     C     R2OACD        IFGT      0
     C     R2OREC        SUB       R2OACD        R2OSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        R2OSAV
     C                   END
     C     R2OSAV        IFLT      0
     C                   Z-ADD     *ZEROS        R2OSAV
     C                   END                                                    END R2OSAV
     C*
     C     RAIME         ADD       RDIME         TRIME
     C     TRIME         ADD       RPIME         TRIME
     C*
     C     RIMACD        IFGT      0
     C     RIMREC        SUB       RIMACD        RIMSAV
     C                   ELSE
     C                   Z-ADD     *ZEROS        RIMSAV
     C                   END
     C     RIMSAV        IFLT      0
     C                   Z-ADD     *ZEROS        RIMSAV
     C                   END                                                    END RIMSAV
     C*
     C                   EXCEPT    RTOT
     C*
     C                   Z-ADD     0             CSCNT
     C*
     C                   Z-ADD     *ZEROS        XRCERT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        RUREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RURECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPEMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        REREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RERECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RASURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RACHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRIREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRIREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RA2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        RD2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        RP2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RAIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        RDIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        RPIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMREQ            5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMREC            5 0          5.0N
     C                   Z-ADD     *ZEROS        RALIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        RDLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPLIAB            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRREQD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRRECD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRELEC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRURGT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TREMER            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRMEDI            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRSURG            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRPSYC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRPHYS            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRCHIR            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRPARF            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRRINJ            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRMGMT            5 0          5.0N
     C                   Z-ADD     *ZEROS        TR2OPN            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRIME             5 0          5.0N
     C                   Z-ADD     *ZEROS        TRTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TRTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RLTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RUTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RUTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RETACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RETSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RSTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RYTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RPTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RHTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RARSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRIACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RRISAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RMGSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        R2OSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        RIMSAV            5 0          5.0N
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       XXCO#               31
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER     :'
     O                       XXSUB#              33
     O          E            ROUTE          2
     O                                           26 'GROUP          :'
     O                       XXFND               31
     O          E            ROUTE          2
     O                                           26 'EMPLOYER       :'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********      N25      XXEMP#    33
     O              N25      XXEMP#        4     37
     O*** END   CHG *** 03/07/00 **************************************
     O               25                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'FOR ACTIVITY   :'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXFRDT    36 '  /  /  '
     O**********                         39 'TO'
     O**********               XXTODT    48 '  /  /  '
     O                       XXFRDT              38 '  /  /    '
     O                                           41 'TO'
     O                       XXTODT              52 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               DSDATE    36 '  /  /  '
     O                       DSDATE              38 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               63 'INC. 2000            '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               DSDATEY  195
     O                       DSDATE             195 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
     O          E            HEAD           1
     O                       Q1PGM               10
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               ORTITL    96
     O                       ORTITL             117
     O*** END   CHG *** 03/07/00 **************************************
     O                                          189 'PAGE:'
     O                       PAGE          Z    195
     O          E            HEAD           2
     O                                           75 'FOR ACTIVITY   :'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXFRDT    84 '  /  /  '
     O**********                         87 'TO'
     O**********               XXTODT    96 '  /  /  '
     O                       XXFRDT              86 '  /  /    '
     O                                           89 'TO'
     O                       XXTODT             100 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
     O          E            HEAD           1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            HEAD           1
     O                                           11 'GROUP.....:'
     O                       XXFND               15
     O                                           19 '-'
     O                       XXFNNM              80
     O          E            HEAD           2
     O                                           11 'EMPLOYER..:'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXEMP#    17
     O**********                         19 '-'
     O**********               XXEMNM    60
     O                       XXEMP#        4     21
     O                                           23 '-'
     O                       XXEMNM              64
     O*** END   CHG *** 03/07/00 **************************************
     O          E            HEAD           0
     O                                           24 'ACCOUNTS WITH PRE/RE-CER'
     O                                           43 'TIFICATION ACTIVITY'
     O          E            HEAD           2
     O                                           24 '________________________'
     O                                           43 '___________________'
     O          E            HEAD           2
     O                                           13 'CLAIMANT NAME'
     O                                           50 'YR-CASE'
     O                                           67 'SOC. SEC. #'
     O                                           77 'ACC DT'
     O                                           91 'PRE-CERT#'
     O                                          109 '# RE-CERT'
     O          E            DET            1
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXCNAM    40
     O                       XXCNAM              39
     O*** END   CHG *** 03/07/00 **************************************
     O                       XXYR          Z     45
     O                                           46 '-'
     O                       PCCASE        Z     54
     O                       SSPRE               59
     O                                           60 '-'
     O                       SSMID               62
     O                                           63 '-'
     O                       SSSUF               67
     C*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXACDT    78 '  /  /  '
     O                       XXACDT              79 '  /  /    '
     C*** END   CHG *** 03/07/00 **************************************
     O                       PCCERT              97
     O                       XXCSRC        Z    107
     O          E            TOT         1
     O                                           12 'TOTAL CASES:'
     O                       CSCNT         Z     19
     O          E            PTOT           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               DSDATEY  195
     O                       DSDATE             195 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
     O          E            PTOT           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          189 'PAGE:'
     O                       PAGE          Z    195
     O          E            PTOT           2
     O                                           75 'FOR ACTIVITY   :'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXFRDT    84 '  /  /  '
     O**********                         87 'TO'
     O**********               XXTODT    96 '  /  /  '
     O                       XXFRDT              86 '  /  /    '
     O                                           89 'TO'
     O                       XXTODT             100 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
     O          E            PTOT           1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            PTOT           1
     O                                           11 'GROUP.....:'
     O                       XXFND               15
     O                                           19 '-'
     O                       XXFNNM              80
     O          E            PTOT           2
     O                                           11 'EMPLOYER..:'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXEMP#    17
     O**********                         19 '-'
     O**********               XXEMNM    60
     O                       XXEMP#        4     21
     O                                           23 '-'
     O                       XXEMNM              64
     O*** END   CHG *** 03/07/00 **************************************
     O          E            PTOT           0
     O                                           24 'PRE-CERTIFICATION ACTIVI'
     O                                           26 'TY'
     O          E            PTOT           2
     O                                           24 '________________________'
     O                                           26 '__'
     O          E            PTOT           1
     O                                           47 '-----LIABILITY-----'
     O                                           74 '------------- # DAYS ---'
     O                                           83 '---------'
     O          E            PTOT           1
     O                                           27 'ACTIVITY'
     O                                           33 'APPR'
     O                                           40 'DENY'
     O                                           47 'PEND'
     O                                           57 'REQUEST'
     O                                           66 'RECOMM'
     O                                           75 'ACTUAL'
     O                                           83 'SAVED'
     O          E            PTOT           2
     O                                           14 'PRE-CERT TOTAL'
     O                       XPCERT        Z     25
     O                       PALIAB        Z     33
     O                       PDLIAB        Z     40
     O                       PPLIAB        Z     47
     O                       TPREQD        Z     56
     O                       TPRECD        Z     66
     O                       TPTACD        Z     75
     O                       TPTSAV        Z     83
     O          E            PTOT           1
     O                                           16 'ADMISSION STATUS'
     O          E            PTOT           1
     O                                           10 'ELECTIVE'
     O                       TPELEC        Z     25
     O                       PAELEC        Z     33
     O                       PDELEC        Z     40
     O                       PPELEC        Z     47
     O                       PLREQD        Z     56
     O                       PLRECD        Z     66
     O                       PLTACD        Z     75
     O                       PLTSAV        Z     83
     O          E            PTOT           1
     O                                           08 'URGENT'
     O                       TPURGT        Z     25
     O                       PAURGT        Z     33
     O                       PDURGT        Z     40
     O                       PPURGT        Z     47
     O                       PUREQD        Z     56
     O                       PURECD        Z     66
     O                       PUTACD        Z     75
     O                       PUTSAV        Z     83
     O          E            PTOT           2
     O                                           11 'EMERGENCY'
     O                       TPEMER        Z     25
     O                       PAEMER        Z     33
     O                       PDEMER        Z     40
     O                       PPEMER        Z     47
     O                       PEREQD        Z     56
     O                       PERECD        Z     66
     O                       PETACD        Z     75
     O                       PETSAV        Z     83
     O          E            PTOT           1
     O                                           17 'TYPE OF ADMISSION'
     O          E            PTOT           1
     O                                           09 'MEDICAL'
     O                       TPMEDI        Z     25
     O                       PAMEDI        Z     33
     O                       PDMEDI        Z     40
     O                       PPMEDI        Z     47
     O                       PMREQD        Z     56
     O                       PMRECD        Z     66
     O                       PMTACD        Z     75
     O                       PMTSAV        Z     83
     O          E            PTOT           1
     O                                           10 'SURGICAL'
     O                       TPSURG        Z     25
     O                       PASURG        Z     33
     O                       PDSURG        Z     40
     O                       PPSURG        Z     47
     O                       PSREQD        Z     56
     O                       PSRECD        Z     66
     O                       PSTACD        Z     75
     O                       PSTSAV        Z     83
     O          E            PTOT           1
     O                                           13 'PSYCHIATRIC'
     O                       TPPSYC        Z     25
     O                       PAPSYC        Z     33
     O                       PDPSYC        Z     40
     O                       PPPSYC        Z     47
     O                       PYREQD        Z     56
     O                       PYRECD        Z     66
     O                       PYTACD        Z     75
     O                       PYTSAV        Z     83
     O          E            PTOT           1
     O                                           18 'PHYSICAL THERAPY'
     O                       TPPHYS        Z     25
     O                       PAPHYS        Z     33
     O                       PDPHYS        Z     40
     O                       PPPHYS        Z     47
     O                       PPREQD        Z     56
     O                       PPRECD        Z     66
     O                       PPTACD        Z     75
     O                       PPTSAV        Z     83
     O          E            PTOT           2
     O                                           14 'CHIROPRACTIC'
     O                       TPCHIR        Z     25
     O                       PACHIR        Z     33
     O                       PDCHIR        Z     40
     O                       PPCHIR        Z     47
     O                       PHREQD        Z     56
     O                       PHRECD        Z     66
     O                       PHTACD        Z     75
     O                       PHTSAV        Z     83
     O          E            PTOT           2
     O                                           12 'P/A REFERRAL'
     O                       TPPARF        Z     25
     O                       PAPARF        Z     33
     O                       PDPARF        Z     40
     O                       PPPARF        Z     47
     O                       PARREQ        Z     56
     O                       PARREC        Z     66
     O                       PARACD        Z     75
     O                       PARSAV        Z     83
     O          E            PTOT           2
     O                                           10 'RE-INJURY?'
     O                       TPRINJ        Z     25
     O                       PARINJ        Z     33
     O                       PDRINJ        Z     40
     O                       PPRINJ        Z     47
     O                       PRIREQ        Z     56
     O                       PRIREC        Z     66
     O                       PRIACD        Z     75
     O                       PRISAV        Z     83
     O          E            PTOT           2
     O                                           11 'HOME HEALTH'
     O                       TPMGMT        Z     25
     O                       PAMGMT        Z     33
     O                       PDMGMT        Z     40
     O                       PPMGMT        Z     47
     O                       PMGREQ        Z     56
     O                       PMGREC        Z     66
     O                       PMGACD        Z     75
     O                       PMGSAV        Z     83
     O          E            PTOT           2
     O                                           17 '2nd OPINION REQD?'
     O                       TP2OPN        Z     25
     O                       PA2OPN        Z     33
     O                       PD2OPN        Z     40
     O                       PP2OPN        Z     47
     O                       P2OREQ        Z     56
     O                       P2OREC        Z     66
     O                       P2OACD        Z     75
     O                       P2OSAV        Z     83
     O          E            PTOT           2
     O                                           09 'IME REQD?'
     O                       TPIME         Z     25
     O                       PAIME         Z     33
     O                       PDIME         Z     40
     O                       PPIME         Z     47
     O                       PIMREQ        Z     56
     O                       PIMREC        Z     66
     O                       PIMACD        Z     75
     O                       PIMSAV        Z     83
     O*
     O*
     O          E            RTOT           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               DSDATEY  195
     O                       DSDATE             195 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
     O          E            RTOT           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          189 'PAGE:'
     O                       PAGE          Z    195
     O          E            RTOT           2
     O                                           75 'FOR ACTIVITY   :'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXFRDT    84 '  /  /  '
     O**********                         87 'TO'
     O**********               XXTODT    96 '  /  /  '
     O                       XXFRDT              86 '  /  /    '
     O                                           89 'TO'
     O                       XXTODT             100 '  /  /    '
     O*** END   CHG *** 03/07/00 **************************************
     O          E            RTOT           1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            RTOT           1
     O                                           11 'GROUP.....:'
     O                       XXFND               15
     O                                           19 '-'
     O                       XXFNNM              80
     O          E            RTOT           2
     O                                           11 'EMPLOYER..:'
     O*** BEGIN CHG *** 03/07/00 **************************************
     O**********               XXEMP#    17
     O**********                         19 '-'
     O**********               XXEMNM    60
     O                       XXEMP#        4     20
     O                                           22 '-'
     O                       XXEMNM              63
     O*** END   CHG *** 03/07/00 **************************************
     O          E            RTOT           0
     O                                           23 'RE-CERTIFICATION ACTIVI'
     O                                           25 'TY'
     O          E            RTOT           2
     O                                           23 '_______________________'
     O                                           25 '__'
     O          E            RTOT           1
     O                                           47 '-----LIABILITY-----'
     O                                           74 '------------- # DAYS ---'
     O                                           83 '---------'
     O          E            RTOT           1
     O                                           27 'ACTIVITY'
     O                                           33 'APPR'
     O                                           40 'DENY'
     O                                           47 'PEND'
     O                                           57 'REQUEST'
     O                                           66 'RECOMM'
     O                                           75 'ACTUAL'
     O                                           83 'SAVED'
     O          E            RTOT           2
     O                                           13 'RE-CERT TOTAL'
     O                       XRCERT        Z     25
     O                       RALIAB        Z     33
     O                       RDLIAB        Z     40
     O                       RPLIAB        Z     47
     O                       TRREQD        Z     56
     O                       TRRECD        Z     66
     O                       TRTACD        Z     75
     O                       TRTSAV        Z     83
     O          E            RTOT           1
     O                                           16 'ADMISSION STATUS'
     O          E            RTOT           1
     O                                           10 'ELECTIVE'
     O                       TRELEC        Z     25
     O                       RAELEC        Z     33
     O                       RDELEC        Z     40
     O                       RPELEC        Z     47
     O                       RLREQD        Z     56
     O                       RLRECD        Z     66
     O                       RLTACD        Z     75
     O                       RLTSAV        Z     83
     O          E            RTOT           1
     O                                           08 'URGENT'
     O                       TRURGT        Z     25
     O                       RAURGT        Z     33
     O                       RDURGT        Z     40
     O                       RPURGT        Z     47
     O                       RUREQD        Z     56
     O                       RURECD        Z     66
     O                       RUTACD        Z     75
     O                       RUTSAV        Z     83
     O          E            RTOT           2
     O                                           11 'EMERGENCY'
     O                       TREMER        Z     25
     O                       RAEMER        Z     33
     O                       RDEMER        Z     40
     O                       RPEMER        Z     47
     O                       REREQD        Z     56
     O                       RERECD        Z     66
     O                       RETACD        Z     75
     O                       RETSAV        Z     83
     O          E            RTOT           1
     O                                           17 'TYPE OF ADMISSION'
     O          E            RTOT           1
     O                                           09 'MEDICAL'
     O                       TRMEDI        Z     25
     O                       RAMEDI        Z     33
     O                       RDMEDI        Z     40
     O                       RPMEDI        Z     47
     O                       RMREQD        Z     56
     O                       RMRECD        Z     66
     O                       RMTACD        Z     75
     O                       RMTSAV        Z     83
     O          E            RTOT           1
     O                                           10 'SURGICAL'
     O                       TRSURG        Z     25
     O                       RASURG        Z     33
     O                       RDSURG        Z     40
     O                       RPSURG        Z     47
     O                       RSREQD        Z     56
     O                       RSRECD        Z     66
     O                       RSTACD        Z     75
     O                       RSTSAV        Z     83
     O          E            RTOT           1
     O                                           13 'PSYCHIATRIC'
     O                       TRPSYC        Z     25
     O                       RAPSYC        Z     33
     O                       RDPSYC        Z     40
     O                       RPPSYC        Z     47
     O                       RYREQD        Z     56
     O                       RYRECD        Z     66
     O                       RYTACD        Z     75
     O                       RYTSAV        Z     83
     O          E            RTOT           1
     O                                           18 'PHYSICAL THERAPY'
     O                       TRPHYS        Z     25
     O                       RAPHYS        Z     33
     O                       RDPHYS        Z     40
     O                       RPPHYS        Z     47
     O                       RPREQD        Z     56
     O                       RPRECD        Z     66
     O                       RPTACD        Z     75
     O                       RPTSAV        Z     83
     O          E            RTOT           2
     O                                           14 'CHIROPRACTIC'
     O                       TRCHIR        Z     25
     O                       RACHIR        Z     33
     O                       RDCHIR        Z     40
     O                       RPCHIR        Z     47
     O                       RHREQD        Z     56
     O                       RHRECD        Z     66
     O                       RHTACD        Z     75
     O                       RHTSAV        Z     83
     O          E            RTOT           2
     O                                           12 'P/A REFERRAL'
     O                       TRPARF        Z     25
     O                       RAPARF        Z     33
     O                       RDPARF        Z     40
     O                       RPPARF        Z     47
     O                       RARREQ        Z     56
     O                       RARREC        Z     66
     O                       RARACD        Z     75
     O                       RARSAV        Z     83
     O          E            RTOT           2
     O                                           10 'RE-INJURY?'
     O                       TRRINJ        Z     25
     O                       RARINJ        Z     33
     O                       RDRINJ        Z     40
     O                       RPRINJ        Z     47
     O                       RRIREQ        Z     56
     O                       RRIREC        Z     66
     O                       RRIACD        Z     75
     O                       RRISAV        Z     83
     O          E            RTOT           2
     O                                           11 'HOME HEALTH'
     O                       TRMGMT        Z     25
     O                       RAMGMT        Z     33
     O                       RDMGMT        Z     40
     O                       RPMGMT        Z     47
     O                       RMGREQ        Z     56
     O                       RMGREC        Z     66
     O                       RMGACD        Z     75
     O                       RMGSAV        Z     83
     O          E            RTOT           2
     O                                           17 '2nd OPINION REQD?'
     O                       TR2OPN        Z     25
     O                       RA2OPN        Z     33
     O                       RD2OPN        Z     40
     O                       RP2OPN        Z     47
     O                       R2OREQ        Z     56
     O                       R2OREC        Z     66
     O                       R2OACD        Z     75
     O                       R2OSAV        Z     83
     O          E            RTOT           2
     O                                           09 'IME REQD?'
     O                       TRIME         Z     25
     O                       RAIME         Z     33
     O                       RDIME         Z     40
     O                       RPIME         Z     47
     O                       RIMREQ        Z     56
     O                       RIMREC        Z     66
     O                       RIMACD        Z     75
     O                       RIMSAV        Z     83
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
