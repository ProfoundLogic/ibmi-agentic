     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CVLMC178 - UPDATE CMDNY FROM WCMS             C:53 02/09/99  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMCML1    IF   E           K DISK    USROPN
     FFMCML1    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMFNL1    IF   E           K DISK
     F*
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     IPCM02
     I              CMCO#                       C2CO#
     I              CMSUB#                      C2SUB#
     I              CMFND                       C2FND
     I              CMFYR                       C2FYR
     I              CMEMP#                      C2EMP#
     I              CMDIV                       C2DIV
     I              CMCASE                      C2CASE
     I              CMSS#                       C2SS#
     I              CMACDT                      C2ACDT
     I              CMACTM                      C2ACTM
     I              CMDTRP                      C2DTRP
     I              CMDTLW                      C2DTLW
     I              CMACDS                      C2ACDS
     I              CMMCON                      C2MCON
     I              CMRT01                      C2RT01
     I              CMAR01                      C2AR01
     I              CMDTRE                      C2DTRE
     I              CMSTAT                      C2STAT
     I              CMUSER                      C2USER
     I              CMPGM                       C2PGM
     I              CMTIME                      C2TIME
     I              CMDT                        C2DT
     I              CMENTD                      C2ENTD
     I              CMRCVD                      C2RCVD
     I              CMPREX                      C2PREX
     I              CMPRV1                      C2PRV1
     I              CMPRM1                      C2PRM1
     I              CMEFD1                      C2EFD1
     I              CMPRV2                      C2PRV2
     I              CMPRM2                      C2PRM2
     I              CMEFD2                      C2EFD2
     I              CMPRV3                      C2PRV3
     I              CMPRM3                      C2PRM3
     I              CMEFD3                      C2EFD3
     I              CMPRV4                      C2PRV4
     I              CMPRM4                      C2PRM4
     I              CMEFD4                      C2EFD4
     I              CMPHY1                      C2PHY1
     I              CMPHM1                      C2PHM1
     I              CMEFD5                      C2EFD5
     I              CMPHY2                      C2PHY2
     I              CMPHM2                      C2PHM2
     I              CMEFD6                      C2EFD6
     I              CMPHY3                      C2PHY3
     I              CMPHM3                      C2PHM3
     I              CMEFD7                      C2EFD7
     I              CMCTYP                      C2DTYP
     I              CMCLTP                      C2CLTP
     I              CMRSRC                      C2RSRC
     I              CMMGMT                      C2MGMT
     I              CMRPRV                      C2RPRV
     I              CMRGN                       C2RGN
     I              CMPRFL                      C2PRFL
     I              CMPRDT                      C2PRDT
     I              CMAST                       C2AST
     I              CMMMI                       C2MMI
     I              CMIUSR                      C2IUSR
     I              CMIDT                       C2IDT
     I              CM1A1                       C21A1
     I              CM1A2                       C21A2
     I              CM1A3                       C21A3
     I              CM1A4                       C21A4
     I              CM1A5                       C21A5
     I              CM1A6                       C21A6
     I              CM1A7                       C21A7
     I              CM1A8                       C21A8
     I              CM1A9                       C21A9
     I              CM1A10                      C21A10
     I              CM6N1                       C26N1
     I              CM6N2                       C26N2
     I              CM6N3                       C26N3
     I              CM6N4                       C26N4
     I              CM6N5                       C26N5
     I              CM6N6                       C26N6
     I              CM6N7                       C26N7
     I              CM6N8                       C26N8
     I              CM6N9                       C26N9
     I              CM6N10                      C26N10
     I              CM10A1                      C210A1
     I              CM10A2                      C210A2
     I              CM10A3                      C210A3
     I              CM10A4                      C210A4
     I              CM10A5                      C210A5
     I              CM40A1                      C240A1
     I              CM40A2                      C240A2
     I              CM40A3                      C240A3
     I              CM40A4                      C240A4
     I              CM40A5                      C240A5
     I              CM92N1                      C292N1
     I              CM92N2                      C292N2
     I              CM92N3                      C292N3
     I              CM92N4                      C292N4
     I              CM92N5                      C292N5
     I              CMMCA                       C2MCA
     I              CMITIM                      C2ITIM
     I              CMLTFL                      C2LTFL
     I              CMTCV1                      C2TCV1
     I              CMTCV2                      C2TCV2
     I              CMTCV3                      C2TCV3
     I              CMCMNU                      C2CMNU
     I              CMCMSU                      C2CMSU
     I              CMCLSU                      C2CLSU
     I              CMCLAD                      C2CLAD
     I              CMDIA1                      C2DIA1
     I              CMLOS1                      C2LOS1
     I              CMDIA2                      C2DIA2
     I              CMLOS2                      C2LOS2
     I              CMDIA3                      C2DIA3
     I              CMLOS3                      C2LOS3
     I              CMDIA4                      C2DIA4
     I              CMLOS4                      C2LOS4
     I              CMDIA5                      C2DIA5
     I              CMLOS5                      C2LOS5
     I              CMSETU                      C2SETU
     I              CMOCC                       C2OCC
     I              CMCNTY                      C2CNTY
     I              CMSCL#                      C2SCL#
     I              CMRNDT                      C2RNDT
     I              CMACD2                      C2ACD2
     I              CMINAC                      C2INAC
     I              CMOEPR                      C2OEPR
     I              CM#DEP                      C2#DEP
     I              CMAPDT                      C2APDT
     I              CMRLSN                      C2RLSN
     I              CMRFSN                      C2RFSN
     I              CMRMID                      C2RMID
     I              CMRTIL                      C2RTIL
     I              CMRDAT                      C2RDAT
     I              CMRTYP                      C2RTYP
     I              CMRDSC                      C2RDSC
     I              CMMCTM                      C2MCTM
     I              CMMC24                      C2MC24
     I              CMESDT                      C2ESDT
     I              CMECDT                      C2ECDT
     I              CMINDT                      C2INDT
     I              CMRINA                      C2RINA
     I              CMIFEI                      C2IFEI
     I              CMIMOD                      C2IMOD
     I              CMIBWC                      C2IBWC
     I              CMINWC                      C2INWC
     I              CMHFEI                      C2HFEI
     I              CMHMOD                      C2HMOD
     I              CMHBWC                      C2HBWC
     I              CMHNWC                      C2HNWC
     I              CMCLR#                      C2CLR#
     I              CMCLRN                      C2CLRN
     I              CMEMR#                      C2EMR#
     I              CMEMRN                      C2EMRN
     I              CMTER                       C2TER
     I              CMTSPC                      C2TSPC
     I              CMTOCC                      C2TOCC
     I              CMPLAC                      C2PLAC
     I              CMPLA1                      C2PLA1
     I              CMPLCY                      C2PLCY
     I              CMPLST                      C2PLST
     I              CMPLZP                      C2PLZP
     I              CMCLIN                      C2CLIN
     I              CMSUBR                      C2SUBR
     I              CMHIST                      C2HIST
     I              CMHIDT                      C2HIDT
     I              CMPRPO                      C2PRPO
     I              CMDEEM                      C2DEEM
     I              CMDEDT                      C2DEDT
     I              CMSTWK                      C2STWK
     I              CMSEFL                      C2SEFL
     I              CMWGAE                      C2WGAE
     I              CMWGTY                      C2WGTY
     I              CMWGFQ                      C2WGFQ
     I              CMWKPT                      C2WKPT
     I              CMHRRT                      C2HRRT
     I              CMHRWK                      C2HRWK
     I              CMWKDY                      C2WKDY
     I              CMSTTM                      C2STTM
     I              CMENTM                      C2ENTM
     I              CMWGDT                      C2WGDT
     I              CMRFTX                      C2RFTX
     I              CMCASG                      C2CASG
     I              CMRPDT                      C2RPDT
     I              CMREHB                      C2REHB
     I              CMSRC                       C2SRC
     I              CMMED                       C2MED
     I              CMLSN                       C2LSN
     I              CMFSN                       C2FSN
     I              CMMID                       C2MID
     I              CMMMIP                      C2MMIP
     I              CMCVFL                      C2CVFL
     I              CMNTWK                      C2NTWK
     I              CMDNY                       C2DNY
     I              CMPEND                      C2PEND
     I              CMCLFA                      C2CLFA
     I              CMOCDS                      C2OCDS
     I              CMODIS                      C2ODIS
     I              CMYRLX                      C2YRLX
     I              CMDYWK                      C2DYWK
     I              CMHRDY                      C2HRDY
     I              CMESTS                      C2ESTS
     I              CMOSCL                      C2OSCL
     I              CMDBDT                      C2DBDT
     I              CMSTF3                      C2STF3
     I              CMACD3                      C2ACD3

     C*
     C********** *ENTRY    PLIST
     C******************   PARM           @@MODE  1
     C*
     C     CMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C     *LIKE         DEFINE    C2SUB#        KKSUB#
     C     *LIKE         DEFINE    C2FND         KKFND
     C*
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   MOVE      *ON           *INOA
     C                   MOVE      'U'           @@MODE            1
     C*
     C     @@MODE        COMP      'U'                                    10
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##NPWC        ANDEQ     ' '
     C                   OPEN      WMCML1
     C                   ELSE
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C                   READ      PCM01                                  98
     C*
     C     *IN98         DOUEQ     *ON
     C*
     C* FUND XLATE
     C*
     C     FNKY          CHAIN     PFN01                              61
     C*
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     FNSUB#        KKSUB#
     C                   Z-ADD     FNFND         KKFND
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM02                              99
     C     *IN99         IFEQ      *OFF
     C     CMDNY         ANDNE     *BLANKS
     C     CMDNY         IFEQ      'N'
     C                   MOVE      ' '           C2DNY
     C                   ELSE
     C                   MOVEL     CMDNY         C2DNY
     C                   ENDIF
     C*
     C                   ADD       1             COUNT
     C   10              UPDATE    PCM02
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   SETOFF                                       OA
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMLSN         ##LAST           56
     C                   MOVEL(P)  CMFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        XXNAME           30
     C*
     C                   EXCEPT    DET1
     C                   ENDIF
     C*
     C                   READ      PCM01                                  98
     C                   ENDDO
     C*
     C                   EXCEPT    TOT1
     C*
     C                   SETON                                        LR
     C*
     OQSYSPRT   E            HEAD1          2 02
     O                                            3 'CO#'
     O          E            HEAD1          2
     O                                            3 'CO#'
     O                                           11 'SUB'
     O                                           18 'FND'
     O                                           24 'FYR'
     O                                           33 'CASE'
     O                                           39 'MC'
     O                                           44 'WC'
     O                                           53 'NAME'
     O          E            DET1           1
     O                       C2CO#         4      3
     O                       C2SUB#        4     11
     O                       C2FND         4     18
     O                       C2FYR         4     24
     O                       C2CASE        4     33
     O                       C2DNY               39
     O                       CMDNY               44
     O                       XXNAME              79
     O          E            TOT1        2
     O                                           22 'Total number of cases:'
     O                       COUNT         J     29
