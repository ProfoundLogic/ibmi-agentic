     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - WA037R - UPDATE ALL WCMS CLAIM FILES                          A
     H*****************************************************************
     H*                                                               *
     H*    CREATED: 10/17/03  C: 53 Proj: 126WBP                      *
     H*****************************************************************
     H*                                                               *
     H* This API is part of the the Tranfer process of moving         *
     H* an employer from one group to another.  Specifically this     *
     H* program updates all the files with the changed group and      *
     H* employer, and claims information.                             *
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/21/11 C:180 SUPPORT 78552
     H*                Transfer EDI employer-paid claim data.
     H*
     H* 01/03/08 C:15  Support: WC135/60772                           *
     H*                Chgd code to not delete WDDDP records if claim *
     H*                is transferred to another emp/div within the   *
     H*                same PEO, for clt135.                          *
     H*                                                               *
     H*  11/15/01 C:053 Project: 114AAB                               *
     H*                 Add client modified files.                    *
     H*                                                               *
     H*  02/15/00 C:112 Project: 65WDT                                *
     H*                 Added code to allow transfer of a claim from  *
     H*                 one group to another.                         *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*  60         GENERAL USAGE (CHAIN)                             *
     H*                                                               *
     H*****************************************************************
      *
      * Diary Transfer Work File
     FWADTL     UF   E           K DISK
      * Request Payment Transaction File
     FWARPL     UF   E           K DISK
     FWARPPML   UF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
      * Auxiliary Compensation Payee File
     FWDACL     UF   E           K DISK
      * Deductible Detail File
     FWDDDL     UF   E           K DISK
      * Claimant Name/Address Tracking File
     FWDNTL     UF   E           K DISK
      * Payment Detail File
     FWDPAL     UF   E           K DISK
      * View Check Detail File
     FWDRPL     UF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
      * Average Weekly Wage History File
     FWMAWL     UF   E           K DISK
      * Benefit History File
     FWMBHL     UF   E           K DISK
      * Benefit Adjustment History File
     FWMBNL     UF   E           K DISK
      * Additional Body Part File
     FWMBYL     UF   E           K DISK
      * Claim History File
     FWMCHL     UF   E           K DISK
      * Claim Master File
     FWMCML1    UF   E           K DISK
      * Diary Date File
     FWMDDL     UF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
      * EDI Log History File
     FWMLGL     UF   E           K DISK
      ***** Begin Add  ****  11/21/11 *********************************
      * EDI Employer-Paid file
     FWDA3L     UF   E           K DISK
      ***** End Add *******  11/21/11 *********************************
      *Claim Legal master File
     FWMLLL     UF   E           K DISK
      * Late Payment File
     FWMLPL     UF   E           K DISK
      * State NOI Info File
     FWMNIL     UF   E           K DISK
      * Reserve History File
     FWMRHL     UF   E           K DISK
      * Refund Request File
     FWMRRL     UF   E           K DISK
      * Lost Time Transfer History File
     FWMTHL     UF   E           K DISK
      * Treating Physician History File
     FWMTPL     UF   E           K DISK
      * Work History File
     FWMWHL     UF   E           K DISK
      * GL Interface Workfile
     FWAGLL1    UF   E           K DISK
      * Worker's Comp Claim Deletion/Detail File
     FWACML     UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
      * Worker's Comp Check Reconciliation Detail File
     FWDCRLA    UF   E           K DISK
     F*** BEGIN ADD *** 02/15/00 **************************************
     F*  Employer Master File
     FWMEML     IF   E           K DISK
     F*** END ADD ***** 02/15/00 **************************************
     F*** BEG ADD *** 11/15/01 **************************************
     FWMIHL     UF   E           K DISK
     FWMTIL     UF   E           K DISK
     FWMC1L     UF   E           K DISK
     F*** END ADD *** 11/15/01 **************************************
     F*  DWC13 Initail Summary File
     FWSFRL     UF   E           K DISK
     F*** Begin add *** 01/03/08 **************************************
     FWMFNL     IF   E           K DISK
      ***** End   Add  ****  11/21/11 *********************************
      *

     D                 DS                  INZ
     D  XXKEY                  1     15
     D  CRFYR                  1      3
     D  CRCASE                 4     10
     D  CRSEQ#                11     15
     D                 DS                  INZ
     D  XXKY                   1     15  0
     D  XXFYR                  1      3  0
     D  XXCASE                 4     10  0
     D  XXSEQ#                11     15  0
     F*** End add ***** 01/03/08 **************************************
      *
      * Diary Transfer Work File

     IPDT01
     I              DTCO#                       XXCO#
     I              DTFND                       XXFND
     I              DTFYR                       XXFYR
     I              DTCASE                      XXCASE
     I              DTEMP#                      XXEMP#
     I              DTDIV                       XXDIV
      *
      * Request Payment Transaction File
     IPRP01
     I              RPCO#                       XXCO#
     I              RPFND                       XXFND
     I              RPFYR                       XXFYR
     I              RPCASE                      XXCASE
      * Request Payment History File
     IPRP03
     I              RPCO#                       XXCO#
     I              RPFND                       XXFND
     I              RPFYR                       XXFYR
     I              RPCASE                      XXCASE
      * Auxiliary Compensation Payee File
     IPAC01
     I              ACCO#                       XXCO#
     I              ACFND                       XXFND
     I              ACFYR                       XXFYR
     I              ACCASE                      XXCASE
      * Deductible Detail File
     IPDD01
     I              DDCO#                       XXCO#
     I              DDFND                       XXFND
     I              DDFYR                       XXFYR
     I              DDCASE                      XXCASE
     I              DDEMP#                      XXEMP#
     I              DDDIV                       XXDIV
      * Claimant Name/Address Tracking File
     IPNT01
     I              NTCO#                       XXCO#
     I              NTFND                       XXFND
     I              NTFYR                       XXFYR
     I              NTCASE                      XXCASE
      * Payment Detail File
     IPPA01
     I              PACO#                       XXCO#
     I              PAFND                       XXFND
     I              PAFYR                       XXFYR
     I              PACASE                      XXCASE
      * View Check Detail File
     IPRP02
     I              RPCO#                       XXCO#
     I              RPFND                       XXFND
     I              RPFYR                       XXFYR
     I              RPCASE                      XXCASE
      * Average Weekly Wage History File
     IPAW01
     I              AWCO#                       XXCO#
     I              AWFND                       XXFND
     I              AWFYR                       XXFYR
     I              AWCASE                      XXCASE
      * Benefit History File
     IPBH01
     I              BHCO#                       XXCO#
     I              BHFND                       XXFND
     I              BHFYR                       XXFYR
     I              BHCASE                      XXCASE
      * Benefit Adjustment History File
     IPBN01
     I              BNCO#                       XXCO#
     I              BNFND                       XXFND
     I              BNFYR                       XXFYR
     I              BNCASE                      XXCASE
      * Additional Body Part File
     IPBY01
     I              BYCO#                       XXCO#
     I              BYFND                       XXFND
     I              BYFYR                       XXFYR
     I              BYCASE                      XXCASE
      * Claim History File
     IPCH01
     I              CHCO#                       XXCO#
     I              CHFND                       XXFND
     I              CHFYR                       XXFYR
     I              CHCASE                      XXCASE
      * Claim Master File
     IPCM01
     I              CMCO#                       XXCO#
     I              CMFND                       XXFND
     I              CMFYR                       XXFYR
     I              CMCASE                      XXCASE
     I              CMEMP#                      XXEMP#
     I              CMDIV                       XXDIV
      * Diary Date File
     IPDD02
     I              DDCO#                       XXCO#
     I              DDFND                       XXFND
     I              DDFYR                       XXFYR
     I              DDCASE                      XXCASE
      * EDI Log History File
     IPLG01
     I              LGCO#                       XXCO#
     I              LGFND                       XXFND
     I              LGFYR                       XXFYR
     I              LGCASE                      XXCASE
      * Claim Legal master File
     IPLL01
     I              LLCO#                       XXCO#
     I              LLFND                       XXFND
     I              LLFYR                       XXFYR
     I              LLCASE                      XXCASE
      * Late Payment File
     IPLP01
     I              LPCO#                       XXCO#
     I              LPFND                       XXFND
     I              LPFYR                       XXFYR
     I              LPCASE                      XXCASE
      * State NOI Info File
     IPNI01
     I              NICO#                       XXCO#
     I              NIFND                       XXFND
     I              NIFYR                       XXFYR
     I              NICASE                      XXCASE
      * Reserve History File
     IPRH01
     I              RHCO#                       XXCO#
     I              RHFND                       XXFND
     I              RHFYR                       XXFYR
     I              RHCASE                      XXCASE
      * Refund Request File
     IPRR01
     I              RRCO#                       XXCO#
     I              RRFND                       XXFND
     I              RRFYR                       XXFYR
     I              RRCASE                      XXCASE
      * Lost Time Transfer History File
     IPTH01
     I              THCO#                       XXCO#
     I              THFND                       XXFND
     I              THFYR                       XXFYR
     I              THCASE                      XXCASE
      * Treating Physician History File
     IPTP01
     I              TPCO#                       XXCO#
     I              TPFND                       XXFND
     I              TPFYR                       XXFYR
     I              TPCASE                      XXCASE
      * Work History File
     IPWH01
     I              WHCO#                       XXCO#
     I              WHFND                       XXFND
     I              WHFYR                       XXFYR
     I              WHCASE                      XXCASE
      * GL Interface Workfile
     IPGL01
     I              GLCO#                       XXCO#
     I              GLFND                       XXFND
     I              GLFYR                       XXFYR
     I              GLCASE                      XXCASE
     I              GLEMP#                      XXEMP#
     I              GLDIV                       XXDIV
      * Worker's Comp Claim Deletion/Detail File
     IPCM02
     I              CMCO#                       XXCO#
     I              CMFND                       XXFND
     I              CMFYR                       XXFYR
     I              CMCASE                      XXCASE
     I              CMEMP#                      XXEMP#
     I              CMDIV                       XXDIV
      * Worker's Comp Check Reconciliation Detail File
     IPCR01
     I              CRCO#                       XXCO#
     I              CRFND                       XXFND
     I              CRKY                        XXKEY
     I*** BEG ADD *** 11/15/01 **************************************
     IPIH01
     I              IHCO#                       XXCO#
     I              IHFND                       XXFND
     I              IHFYR                       XXFYR
     I              IHCASE                      XXCASE
     IPTI01
     I              TICO#                       XXCO#
     I              TIFND                       XXFND
     I              TIFYR                       XXFYR
     I              TICASE                      XXCASE
     IPC101
     I              C1CO#                       XXCO#
     I              C1FND                       XXFND
     I              C1FYR                       XXFYR
     I              C1CASE                      XXCASE
     I*** END ADD *** 11/15/01 **************************************
      * DWC13 Initial Summary Entry File
     IPFR01
     I              FRCO#                       XXCO#
     I              FRFND                       XXFND
     I              FRFYR                       XXFYR
     I              FRCASE                      XXCASE
      ***** Begin Add  ****  11/21/11 *********************************
      *
      * EDI Employer-paid file
      *
     IPA301
     I              A3CO#                       XXCO#
     I              A3FND                       XXFND
     I              A3FYR                       XXFYR
     I              A3CASE                      XXCASE
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FGRP            3
     C                   PARM                    @@FEMP            9
     C                   PARM                    @@FDIV            5
     C                   PARM                    @@FFYR            3
     C                   PARM                    @@FCAS            7
     C                   PARM                    @@TGRP            3
     C                   PARM                    @@TEMP            9
     C                   PARM                    @@TDIV            5
     C                   PARM                    @@TFYR            3
     C                   PARM                    @@TCAS            7
      *** BEGIN ADD *** 02/15/00 **************************************
     C                   PARM                    @@TRNX            1
      *** END ADD ***** 02/15/00 **************************************
      *
      *
      *****************************************************************
      *  KEYS
      *****************************************************************
      *
      * Key used to retrieve 'From' data from files
      *
     C     FRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFFND
     C                   KFLD                    XXFFYR
     C                   KFLD                    XXFCAS
      *
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFFND
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C*  Key list for the Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** END ADD ***** 02/15/00 **************************************
      *****************************************************************
      *  LIKE DEFINITIONS
      *****************************************************************
      *
     C     *LIKE         DEFINE    XXFND         XXFFND
     C     *LIKE         DEFINE    XXFYR         XXFFYR
     C     *LIKE         DEFINE    XXCASE        XXFCAS
      *
     C*** Begin add *** 01/03/08 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     CRKEY         CHAIN     PFN01                              60
     C  N60              MOVE      *BLANKS       FNPEOF
     C*** End add ***** 01/03/08 **************************************
      *
      *****************************************************************
      *  MAIN
      *****************************************************************
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FGRP        XXFFND
     C                   MOVE      @@FFYR        XXFFYR
     C                   MOVE      @@FCAS        XXFCAS
      *
     C     FRKY          CHAIN     PDT01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   EXSR      LOADEX
     C                   UPDATE    PDT01
     C     FRKY          READE     PDT01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PRP01
     C     FRKY          READE     PRP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRP03                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PRP03
     C     FRKY          READE     PRP03                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAC01
     C     FRKY          READE     PAC01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDD01                              60
     C     *IN60         DOWEQ     *OFF
      *** BEGIN ADD *** 02/15/000 *************************************
      *
      * Delete the Deductible Detail record if transfer is of type
      * 'C' indicating this is a Claim transfer.  Process normally
      * when transfer type is an Employer transfer type 'E'
      *
     C     @@TRNX        IFEQ      'E'
      *** END ADD ***** 02/15/000 *************************************
     C                   EXSR      LOADTO
     C                   EXSR      LOADEX
     C                   UPDATE    PDD01
      *** BEGIN ADD *** 02/15/000 *************************************
     C                   ELSE
     C     @@TRNX        IFEQ      'C'
     C*** Begin add *** 01/03/08 **************************************
     C     CLTID         IFEQ      '135'
     C     @@FGRP        ANDEQ     @@TGRP
     C     FNPEOF        ANDNE     *BLANKS
     C                   LEAVE
     C                   ENDIF
     C*** End add ***** 01/03/08 **************************************
     C                   DELETE    PDD01
     C                   ENDIF
     C                   ENDIF
      *** END ADD ***** 02/15/000 *************************************
     C     FRKY          READE     PDD01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PNT01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PNT01
     C     FRKY          READE     PNT01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PPA01
     C     FRKY          READE     PPA01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRP02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PRP02
     C     FRKY          READE     PRP02                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAW01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAW01
     C     FRKY          READE     PAW01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PBH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PBH01
     C     FRKY          READE     PBH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PBN01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PBN01
     C     FRKY          READE     PBN01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PBY01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PBY01
     C     FRKY          READE     PBY01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PCH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCH01
     C     FRKY          READE     PCH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   EXSR      LOADEX
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C*  For a single claim transfer, if the employer premises flag
     C*  is 'Y', load the 'Place of accident' fields.
     C*
     C     @@TRNX        IFEQ      'C'
     C     CMEADR        ANDEQ     'Y'
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C     EMPAD1        IFNE      *BLANK
     C                   MOVEL     EMNAME        CMACNM
     C                   MOVEL     EMPAD1        CMACDR
     C                   MOVEL     EMPCTY        CMACTY
     C                   MOVEL     EMPST         CMAST
     C                   MOVEL     EMPZIP        CMACZ1
     C                   MOVE      EMPZIP        CMACZ2
     C                   ELSE
     C                   MOVEL     EMNAME        CMACNM
     C                   MOVEL     EMMAD1        CMACDR
     C                   MOVEL     EMMCTY        CMACTY
     C                   MOVEL     EMMST         CMAST
     C                   MOVEL     EMMZIP        CMACZ1
     C                   MOVE      EMMZIP        CMACZ2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 02/15/00 **************************************
     C*
     C                   UPDATE    PCM01
     C     FRKY          READE     PCM01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDD02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDD02
     C     FRKY          READE     PDD02                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PLG01
     C     FRKY          READE     PLG01                                  60
     C                   ENDDO
      *
      *** BEGIN ADD *** 11/21/11 ************************************
      *
     C     FRKY          CHAIN     PA301                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PA301
     C     FRKY          READE     PA301                                  60
     C                   ENDDO
      *** END ADD ***** 11/21/11 ************************************
      *
     C     FRKY          CHAIN     PLL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PLL01
     C     FRKY          READE     PLL01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PLP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PLP01
     C     FRKY          READE     PLP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PNI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PNI01
     C     FRKY          READE     PNI01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PRH01
     C     FRKY          READE     PRH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PRR01
     C     FRKY          READE     PRR01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PTH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTH01
     C     FRKY          READE     PTH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PTP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTP01
     C     FRKY          READE     PTP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PWH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PWH01
     C     FRKY          READE     PWH01                                  60
     C                   ENDDO
      *
     C*** BEG ADD *** 11/15/01 **************************************
      *
     C     FRKY          CHAIN     PC101                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PC101
     C     FRKY          READE     PC101                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PIH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PIH01
     C     FRKY          READE     PIH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PTI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTI01
     C     FRKY          READE     PTI01                                  60
     C                   ENDDO
      *
     C*** END ADD *** 11/15/01 **************************************
      *
     C     FRKY          CHAIN     PGL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   EXSR      LOADEX
     C                   UPDATE    PGL01
     C     FRKY          READE     PGL01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PCM02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   EXSR      LOADEX
     C                   UPDATE    PCM02
     C     FRKY          READE     PCM02                                  60
     C                   ENDDO
      *
      *
     C     FRKY          CHAIN     PFR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PFR01
     C     FRKY          READE     PFR01                                  60
     C                   ENDDO
      *
      *
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     *OFF
      *** test file fields defined as alphanumeric for presents of only
      *** numeric contents
     C                   TESTN                   CRFYR                71
     C                   TESTN                   CRCASE               72
     C     *IN71         IFEQ      *ON
     C     *IN72         ANDEQ     *ON
     C                   MOVE      CRFYR         XXFYR
     C                   MOVE      CRCASE        XXCASE
     C     XXFYR         IFEQ      XXFFYR
     C     XXCASE        ANDEQ     XXFCAS
     C                   EXSR      LOADTO
     C                   UPDATE    PCR01
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANKS       XXKEY
     C                   ENDIF
     C     CRKEY         READE     PCR01                                  60
     C                   ENDDO
      *
     C                   RETURN
      *
      *****************************************************************
      *  LOADTO
      *****************************************************************
      *
     C     LOADTO        BEGSR
      *
     C                   MOVE      @@TGRP        XXFND
     C                   MOVE      @@TFYR        XXFYR
     C                   MOVE      @@TCAS        XXCASE
      *
     C                   ENDSR
      *****************************************************************
      *  LOADEX - load extra fields of EMP# and DIV for files with them
      *****************************************************************
      *
     C     LOADEX        BEGSR
      *
     C                   MOVE      @@TEMP        XXEMP#
     C                   MOVE      @@TDIV        XXDIV
      *
     C                   ENDSR
