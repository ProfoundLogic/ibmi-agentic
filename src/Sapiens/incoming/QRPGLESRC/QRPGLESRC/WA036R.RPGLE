     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - WA036R - UPDATE ALL WCMS EMPLOYER FILES                       A
     H*****************************************************************
     H*                                                               *
     H*    CREATED: 10/17/03  C: 53 Proj: 126WBP                      *
     H*****************************************************************
     H*                                                               *
     H* This API is part of the the Tranfer process of moving         *
     H* an employer from one group to another.  Specifically this     *
     H* program updates all the files with the changed group and      *
     H* employer information.                                         *
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 00/00/00 C:00 Req/Proj:                                       *
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
      * Billing Work File
     FWABLL1    UF   E           K DISK
      * Deductible Net Recovery Pending File
     FWADQL     UF   E           K DISK
      * Employer Discount Detail File
     FWADSL1    UF   E           K DISK
      * Office Auxiliary
     FWAOFL     UF   E           K DISK
      * Employer Payroll Self-Reporting File
     FWASRL5    UF   E           K DISK
      * Cancellation Detail File
     FWDCXL1    UF   E           K DISK
      * Employer Discount Detail File
     FWDDSL1    UF   E           K DISK
     F                                     RENAME(PDS01:PDS02)
      * A/R Batch Entry File
     FWDEAL5    UF   E           K DISK
      * Employer Detail History File
     FWDEHL     UF   E           K DISK
      * Employer Detail File
     FWDELL     UF   E           K DISK
      * Experience Modifier tracking File
     FWDEXL2    UF   E           K DISK
      * Employer Increased Limits File
     FWDILL1    UF   E           K DISK
      * Employer Premium File
     FWDMPL1    UF   E           K DISK
      * Cancellation History File
     FWDXHL2    UF   E           K DISK
      * Agent Commission File
     FWMACL10   UF   E           K DISK
      *Agent History File
     FWMAHL     UF   E           K DISK
      * Additional Entity/Location File
     FWMALL     UF   E           K DISK
      * Audit Tracking Log File
     FWMAUL2    UF   E           K DISK
      * Audit Status Detail Listing File
     FWMAVL3    UF   E           K DISK
      *Employer Third Adress File
     FWMA3L     UF   E           K DISK
      * Contractors Class File
     FWMCCL1    UF   E           K DISK
      * Certificate Holders File
     FWMCEL1    UF   E           K DISK
      * Department Master File
     FWMDPL     UF   E           K DISK
      * Employer A/R File
     FWMEAL     UF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
      * Employer Master File
     FWMEML     UF   E           K DISK
      * Endorsement File
     FWMENL1    UF   E           K DISK
      * Employer Payroll File
     FWMEPL     UF   E           K DISK
      * Employer Endorsement Forms File
     FWMMUL     UF   E           K DISK
      * Owners/Officers Master File
     FWMOOL     UF   E           K DISK
      * NCCI Reporting Submission History File
     FWMSHL13   UF   E           K DISK
      * Provisional Premium Detail File
     FWZPPL     UF   E           K DISK
      * Employer Accounts Receivable
     FWAEAL     UF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
      * Employer Payroll File
     FWAEPL     UF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
      * Financial Earned Premium
     FWAFPL1    UF   E           K DISK
      * Agent Quote System File
     FWAQTL7    UF   E           K DISK
      * Agency Billing Detail File
     FWDABL2    UF   E           K DISK
      * Alternate Employers File
     FWDAEL3    UF   E           K DISK
      * Contractor Billing Detail File
     FWDCBL     UF   E           K DISK
      * Employer EARS Factor File
     FWDERL3    UF   E           K DISK
      * Membership Fee A/R File
     FWDFDL     UF   E           K DISK
      * Add Ent/Loc Detail File
     FWDLDL     UF   E           K DISK
      * Employer Status History File
     FWHESL     UF   E           K DISK
      * Named Insureds File
     FWMANL     UF   E           K DISK
      * Direct Debit Employer Maintenance File
     FWMDBL     UF   E           K DISK
      * Worker's Comp Endorsement File
     FWMENL11   UF   E           K DISK
      * Loss Control Tracking File
     FWMLCL2    UF   E           K DISK
      * Member Packets File
     FWMMBL6    UF   E           K DISK
      * Reinsurance Policy File
     FWMRIL1    UF   E           K DISK
      * Carrier/State reporting File
     FWMRPL     UF   E           K DISK
      * Exception Staff Code File
     FWMSEL     UF   E           K DISK
      *
      * Billing Work File

     IPBL01
     I              BLCO#                       XXCO#
     I              BLFND                       XXFND
     I              BLEMP#                      XXEMP#
     I              BLDIV                       XXDIV
      *
      * Deductible Net Recovery Pending File
     IPDQ01
     I              DQCO#                       XXCO#
     I              DQFND                       XXFND
     I              DQEMP#                      XXEMP#
     I              DQDIV                       XXDIV
      *
      * Employer Discount Detail File
     IPDS01
     I              DSCO#                       XXCO#
     I              DSFND                       XXFND
     I              DSEMP#                      XXEMP#
     I              DSDIV                       XXDIV
      *
      * Office Auxiliary
     IPOF01
     I              OFCO#                       XXCO#
     I              OFFND                       XXFND
     I              OFEMP#                      XXEMP#
     I              OFDIV                       XXDIV
      *
      * Employer Payroll Self-Reporting File
     IPSR01
     I              SRCO#                       XXCO#
     I              SRFND                       XXFND
     I              SREMP#                      XXEMP#
     I              SRDIV                       XXDIV
      *
      * Cancellation Detail File
     IPCX01
     I              CXCO#                       XXCO#
     I              CXFND                       XXFND
     I              CXEMP#                      XXEMP#
     I              CXDIV                       XXDIV
      *
      * Employer Discount Detail File
     IPDS02
     I              DSCO#                       XXCO#
     I              DSFND                       XXFND
     I              DSEMP#                      XXEMP#
     I              DSDIV                       XXDIV
      *
      * A/R Batch Entry File
     IPEA01
     I              EACO#                       XXCO#
     I              EAFND                       XXFND
     I              EAEMP#                      XXEMP#
     I              EADIV                       XXDIV
      *
      * Employer Detail History File
     IPEH01
     I              EHCO#                       XXCO#
     I              EHFND                       XXFND
     I              EHEMP#                      XXEMP#
     I              EHDIV                       XXDIV
      *
      * Employer Detail File
     IPEL01
     I              ELCO#                       XXCO#
     I              ELFND                       XXFND
     I              ELEMP#                      XXEMP#
     I              ELDIV                       XXDIV
      *
      * Experience Modifier tracking File
     IPEX01
     I              EXCO#                       XXCO#
     I              EXFND                       XXFND
     I              EXEMP#                      XXEMP#
     I              EXDIV                       XXDIV
      *
      * Employer Increased Limits File
     IPIL01
     I              ILCO#                       XXCO#
     I              ILFND                       XXFND
     I              ILEMP#                      XXEMP#
     I              ILDIV                       XXDIV
      *
      * Employer Premium File
     IPMP01
     I              MPCO#                       XXCO#
     I              MPFND                       XXFND
     I              MPEMP#                      XXEMP#
     I              MPDIV                       XXDIV
      *
      * Cancellation History File
     IPXH01
     I              XHCO#                       XXCO#
     I              XHFND                       XXFND
     I              XHEMP#                      XXEMP#
     I              XHDIV                       XXDIV
      *
      * Agent Commission File
     IPAC01
     I              ACCO#                       XXCO#
     I              ACFND                       XXFND
     I              ACEMP#                      XXEMP#
     I              ACDIV                       XXDIV
      *
      *Agent History File
     IPAH01
     I              AHCO#                       XXCO#
     I              AHFND                       XXFND
     I              AHEMP#                      XXEMP#
     I              AHDIV                       XXDIV
      *
      * Additional Entity/Location File
     IPAL01
     I              ALCO#                       XXCO#
     I              ALFND                       XXFND
     I              ALEMP#                      XXEMP#
     I              ALDIV                       XXDIV
      *
      * Audit Tracking Log File
     IPAU01
     I              AUCO#                       XXCO#
     I              AUFND                       XXFND
     I              AUEMP#                      XXEMP#
     I              AUDIV                       XXDIV
      *
      * Audit Status Detail Listing File
     IPAV01
     I              AVCO#                       XXCO#
     I              AVFND                       XXFND
     I              AVEMP#                      XXEMP#
     I              AVDIV                       XXDIV
      *
      *Employer Third Adress File
     IPA301
     I              A3CO#                       XXCO#
     I              A3FND                       XXFND
     I              A3EMP#                      XXEMP#
     I              A3DIV                       XXDIV
      *
      * Contractors Class File
     IPCC01
     I              CCCO#                       XXCO#
     I              CCFND                       XXFND
     I              CCEMP#                      XXEMP#
     I              CCDIV                       XXDIV
      *
      * Certificate Holders File
     IPCE01
     I              CECO#                       XXCO#
     I              CEFND                       XXFND
     I              CEEMP#                      XXEMP#
     I              CEDIV                       XXDIV
      *
      * Department Master File
     IPDP01
     I              DPCO#                       XXCO#
     I              DPFND                       XXFND
     I              DPEMP#                      XXEMP#
     I              DPDIV                       XXDIV
      *
      * Employer A/R File
     IPEA02
     I              EACO#                       XXCO#
     I              EAFND                       XXFND
     I              EAEMP#                      XXEMP#
     I              EADIV                       XXDIV
      *
      * Employer Master File
     IPEM01
     I              EMCO#                       XXCO#
     I              EMFND                       XXFND
     I              EMEMP#                      XXEMP#
     I              EMDIV                       XXDIV
      *
      * Endorsement File
     IPEN01
     I              ENCO#                       XXCO#
     I              ENFND                       XXFND
     I              ENEMP#                      XXEMP#
     I              ENDIV                       XXDIV
      *
      * Employer Payroll File
     IPEP01
     I              EPCO#                       XXCO#
     I              EPFND                       XXFND
     I              EPEMP#                      XXEMP#
     I              EPDIV                       XXDIV
      *
      * Employer Endorsement Forms File
     IPMU01
     I              MUCO#                       XXCO#
     I              MUFND                       XXFND
     I              MUEMP#                      XXEMP#
     I              MUDIV                       XXDIV
      *
      * Owners/Officers Master File
     IPOO01
     I              OOCO#                       XXCO#
     I              OOFND                       XXFND
     I              OOEMP#                      XXEMP#
     I              OODIV                       XXDIV
      *
      * NCCI Reporting Submission History File
     IPSH01
     I              SHCO#                       XXCO#
     I              SHFND                       XXFND
     I              SHEMP#                      XXEMP#
     I              SHDIV                       XXDIV
      *
      * Provisional Premium Detail File
     IPPP01
     I              PPCO#                       XXCO#
     I              PPFND                       XXFND
     I              PPEMP#                      XXEMP#
     I              PPDIV                       XXDIV
      *
      * Employer Accounts Receivable
     IPEA03
     I              EACO#                       XXCO#
     I              EAFND                       XXFND
     I              EAEMP#                      XXEMP#
     I              EADIV                       XXDIV
      *
      * Employer Payroll File
     IPEP02
     I              EPCO#                       XXCO#
     I              EPFND                       XXFND
     I              EPEMP#                      XXEMP#
     I              EPDIV                       XXDIV
      *
      * Employer Payroll File
     IPFP01
     I              FPCO#                       XXCO#
     I              FPFND                       XXFND
     I              FPEMP#                      XXEMP#
     I              FPDIV                       XXDIV
      *
      * Provisional Premium Detail File
     IPQT01
     I              QTCO#                       XXCO#
     I              QTFND                       XXFND
     I              QTEMP#                      XXEMP#
     I              QTDIV                       XXDIV
      *
      * Agency Billing Detail File
     IPAB01
     I              ABCO#                       XXCO#
     I              ABFND                       XXFND
     I              ABEMP#                      XXEMP#
     I              ABDIV                       XXDIV
      * Alternate Employers File
     IPAE01
     I              AECO#                       XXCO#
     I              AEFND                       XXFND
     I              AEEMP#                      XXEMP#
     I              AEDIV                       XXDIV
      * Contractor Billing Detail File
     IPCB01
     I              CBCO#                       XXCO#
     I              CBFND                       XXFND
     I              CBEMP#                      XXEMP#
     I              CBDIV                       XXDIV
      * Employer EARS Factor File
     IPER01
     I              ERCO#                       XXCO#
     I              ERFND                       XXFND
     I              EREMP#                      XXEMP#
     I              ERDIV                       XXDIV
      * Membership Fee A/R File
     IPFD01
     I              FDCO#                       XXCO#
     I              FDFND                       XXFND
     I              FDEMP#                      XXEMP#
     I              FDDIV                       XXDIV
      * Add Ent/Loc Detail File
     IPLD01
     I              LDCO#                       XXCO#
     I              LDFND                       XXFND
     I              LDEMP#                      XXEMP#
     I              LDDIV                       XXDIV
      * Employer Status History
     IPES01
     I              ESCO#                       XXCO#
     I              ESFND                       XXFND
     I              ESEMP#                      XXEMP#
     I              ESDIV                       XXDIV
      * Named Insureds File
     IPAN01
     I              ANCO#                       XXCO#
     I              ANFND                       XXFND
     I              ANEMP#                      XXEMP#
     I              ANDIV                       XXDIV
      * Direct Debit Employer Maintenance File
     IPDB01
     I              DBCO#                       XXCO#
     I              DBFND                       XXFND
     I              DBEMP#                      XXEMP#
     I              DBDIV                       XXDIV
      * Worker's Comp Endorsement File
     IPEN011
     I              ENCO#                       XXCO#
     I              ENFND                       XXFND
     I              ENEMP#                      XXEMP#
     I              ENDIV                       XXDIV
      * Loss Control Tracking File
     IPLC01
     I              LCCO#                       XXCO#
     I              LCFND                       XXFND
     I              LCEMP#                      XXEMP#
     I              LCDIV                       XXDIV
      * Member Packets File
     IPMB01
     I              MBCO#                       XXCO#
     I              MBFND                       XXFND
     I              MBEMP#                      XXEMP#
     I              MBDIV                       XXDIV
      * Reinsurance Policy File
     IPRI01
     I              RICO#                       XXCO#
     I              RIFND                       XXFND
     I              RIEMP#                      XXEMP#
     I              RIDIV                       XXDIV
      * Carrier/State reporting File
     IPRP01
     I              RPCO#                       XXCO#
     I              RPFND                       XXFND
     I              RPEMP#                      XXEMP#
     I              RPDIV                       XXDIV
      * Exception Staff Code File
     IPSE01
     I              SECO#                       XXCO#
     I              SEFND                       XXFND
     I              SEEMP#                      XXEMP#
     I              SEDIV                       XXDIV
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FGRP            3
     C                   PARM                    @@FEMP            9
     C                   PARM                    @@FDIV            5
     C                   PARM                    @@TGRP            3
     C                   PARM                    @@TEMP            9
     C                   PARM                    @@TDIV            5
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
     C                   KFLD                    XXFEMP
     C                   KFLD                    XXFDIV
      *
      *****************************************************************
      *  LIKE DEFINITIONS
      *****************************************************************
      *
     C     *LIKE         DEFINE    XXFND         XXFFND
     C     *LIKE         DEFINE    XXEMP#        XXFEMP
     C     *LIKE         DEFINE    XXDIV         XXFDIV
      *****************************************************************
      *  MAIN
      *****************************************************************
      *
      * initialize key variables
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FGRP        XXFFND
     C                   MOVE      @@FEMP        XXFEMP
     C                   MOVE      @@FDIV        XXFDIV
      *
     C     FRKY          CHAIN     PBL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PBL01
     C     FRKY          READE     PBL01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDQ01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDQ01
     C     FRKY          READE     PDQ01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDS01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDS01
     C     FRKY          READE     PDS01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     POF01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    POF01
     C     FRKY          READE     POF01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PSR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PSR01
     C     FRKY          READE     PSR01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PCX01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCX01
     C     FRKY          READE     PCX01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDS02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDS02
     C     FRKY          READE     PDS02                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEA01
     C     FRKY          READE     PEA01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEH01
     C     FRKY          READE     PEH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEL01
     C     FRKY          READE     PEL01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEX01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEX01
     C     FRKY          READE     PEX01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PIL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PIL01
     C     FRKY          READE     PIL01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PMP01
     C     FRKY          READE     PMP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PXH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PXH01
     C     FRKY          READE     PXH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAC01
     C     FRKY          READE     PAC01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAH01
     C     FRKY          READE     PAH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAL01
     C     FRKY          READE     PAL01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAU01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAU01
     C     FRKY          READE     PAU01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAV01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAV01
     C     FRKY          READE     PAV01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PA301                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PA301
     C     FRKY          READE     PA301                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PCC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCC01
     C     FRKY          READE     PCC01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PCE01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCE01
     C     FRKY          READE     PCE01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDP01
     C     FRKY          READE     PDP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEA02
     C     FRKY          READE     PEA02                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEM01
     C     FRKY          READE     PEM01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEN01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEN01
     C     FRKY          READE     PEN01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEP01
     C     FRKY          READE     PEP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PMU01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PMU01
     C     FRKY          READE     PMU01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     POO01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    POO01
     C     FRKY          READE     POO01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PSH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PSH01
     C     FRKY          READE     PSH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PPP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PPP01
     C     FRKY          READE     PPP01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PEA03                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEA03
     C     FRKY          READE     PEA03                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PEP02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEP02
     C     FRKY          READE     PEP02                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PFP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PFP01
     C     FRKY          READE     PFP01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PQT01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PQT01
     C     FRKY          READE     PQT01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PAB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAB01
     C     FRKY          READE     PAB01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PAE01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAE01
     C     FRKY          READE     PAE01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PCB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCB01
     C     FRKY          READE     PCB01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PER01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PER01
     C     FRKY          READE     PER01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PFD01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PFD01
     C     FRKY          READE     PFD01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PLD01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PLD01
     C     FRKY          READE     PLD01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PES01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PES01
     C     FRKY          READE     PES01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PAN01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAN01
     C     FRKY          READE     PAN01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PDB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDB01
     C     FRKY          READE     PDB01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PEN011                             60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEN011
     C     FRKY          READE     PEN011                                 60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PLC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PLC01
     C     FRKY          READE     PLC01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PMB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PMB01
     C     FRKY          READE     PMB01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PRI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PRI01
     C     FRKY          READE     PRI01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PRP01
     C     FRKY          READE     PRP01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PSE01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PSE01
     C     FRKY          READE     PSE01                                  60
     C                   ENDDO
      *
      *  program has reached maximum of 50 files it can process with
      *  compiler limitations.  Call WA0036R01 to process remainder
      *  of affected files
      *
     C                   CALL      'WA036R1'
     C                   PARM                    @@CO#
     C                   PARM                    @@FGRP
     C                   PARM                    @@FEMP
     C                   PARM                    @@FDIV
     C                   PARM                    @@TGRP
     C                   PARM                    @@TEMP
     C                   PARM                    @@TDIV
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
     C                   MOVE      @@TEMP        XXEMP#
     C                   MOVE      @@TDIV        XXDIV
      *
     C                   ENDSR
