     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* API - FA800R - UPDATE ALL WCMC EMPLOYER FILES
     H*****************************************************************
     H*                                                               *
     H* This API is part of the the Tranfer process of moving         *
     H* an employer from one group to another.  Specifically this     *
     H* program updates all the files with the changed group, employer*
     H* and claim information for WCMC.  Additionally, the program    *
     H* writes a record to a history file.                            *
     H*                                                               *
     H*    CREATED: 05/26/99  C:112 Proj: 65WDC                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/20/08 C:33  Project: WC1770                                *
     H*                Ensure MC values are kept in sync with system  *
     H*                 values.                                       *
     H*                                                               *
     H* 12/05/01 C:15  Support: WC065/29279                           *
     H*                Changed pgm due to flaw in code which prevents *
     H*                any files from being updated.                  *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*  60         GENERAL USAGE (CHAIN)                             *
     H*                                                               *
     H*****************************************************************
      *
     F*  Logical file definition for WCMS Employer Transfer File
     FFAETL     O  A E             DISK
      *
      * Diagnosis Code Info File
     FFADAL     UF   E           K DISK
     F***  Begin Add  ***  05/20/09  **************************************
     F                                     INFDS(DAFDS)
     F***  End   Add  ***  05/20/09  **************************************
      *
     F*  Diagnosis Code/Detail Charge Temp File
     FFADCL     UF   E           K DISK
      *
      * Duplicate Claims Workfile
     FFADUL     UF   E           K DISK
      *
     F*  EOMB Header Amount Transaction File
     FFAEAL1    UF   E           K DISK
      *
      * Dental Bill Header Info File
     FFAEDL     UF   E           K DISK
      *
     F* EOMB Header EOB/Notes Transaction File
     FFAEEL     UF   E           K DISK
      *
      * EOMB Phys/HCFA Header File
     FFAEHL     UF   E           K DISK
      *
     F* EOMB Tansaction File
     FFAEOL     UF   E           K DISK
      *
      * Pharmacy Bill Header Info File
     FFAEPL     UF   E           K DISK
      *
     F* Recon/Refund EOMB Transaction File
     FFAERL     UF   E           K DISK
      *
      * EOMB Status Change Transaction File
     FFAESL     UF   E           K DISK
      *
     F* EOMB Hosp/UB Header File
     FFAEUL     UF   E           K DISK
      *
      * EOMB Auxiliary File
     FFAE1L     UF   E           K DISK
      *
     F* HBA Transaction File
     FFAHBL     UF   E           K DISK
      *
      * Request Letter Transaction File
     FFALTL     UF   E           K DISK
      *
     F* Claim Med Service Transaction File
     FFAMSL     UF   E           K DISK
      *
      * Claim Phys/Prov/Spec Transaction File
     FFAPPL1    UF   E           K DISK
      *
     F* All: BWC Treatment Plan Notes
     FFATDL     UF   E           K DISK
      *
      * All: BWC Treatment Plan ICD9 Code
     FFATIL     UF   E           K DISK
      *
     F* All: BWC Treatment Plan File
     FFATRL     UF   E           K DISK
      *
      * Treatment Report Transaction File
     FFATXL     UF   E           K DISK
      *
     F* Work History File
     FFAWHL     UF   E           K DISK
      *
      * Detail Charge File
     F*** BEGIN CHG *** 07/19/02 **************************************
     F**********FDCAL   UF  E           K        DISK
     FFDCALL    UF   E           K DISK
     F*** END   CHG *** 07/19/02 **************************************
      *
     F* EOMB Dental Detail Transaction File
     FFDDDL     UF   E           K DISK
      *
      * EOMB Detail EOB/Notes Transaction File
     FFDDEL     UF   E           K DISK
      *
     F* EOMB Phys/HCFA Detail Transaction File
     FFDDHL     UF   E           K DISK
      *
      * EOMB Pharmacy Detail Transaction File
     FFDDPL     UF   E           K DISK
      *
     F* EOMB Hospital/UB Detail Transaction Fileer Transfer File
     FFDDUL     UF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
      *
      * EOMB Delete Transaction File
     FFDEOL     UF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
      *
     F* EOMB Delete Auxiliary File
     FFDE1L     UF   E           K DISK
     F                                     RENAME(PE101:PE102)
      *
      * Detail Charge Auxiliary File
     FFDHCL     UF   E           K DISK
      *
      * Prov Bills Specific Errors File
     FFDMGL     UF   E           K DISK
      *
     F* Claim Status History File
     FFMCHL1    UF   E           K DISK
      *
      * Claim Master File
     FFMCML1    UF   E           K DISK
      *
     F* EDI Log History File
     FFMLGL     UF   E           K DISK
      *
      * Medical Management Billing Transaction File
     FNAMBL     UF   E           K DISK
      *
      * Pre-Certification Billing Transaction File
     FNAPBL     UF   E           K DISK
      *
     F* Ser-up Bill Transaction File
     FNASBL     UF   E           K DISK
      *
      * Network Transaction Record File
     FNATRL     UF   E           K DISK
     F                                     RENAME(PTR01:PTR02)
      *
      * Time/Materials Detail Charge File
     FNDTML     UF   E           K DISK
      *
     F* Accounts Receivable File
     FNMARL2    UF   E           K DISK
      *
      * Pre-Certification Procedure Code File
     FUDPOL     UF   E           K DISK
      *
      * Claim Pre-Certification Procedure Code File
     FUMCHL1    UF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
      *
      * Pre-Certification master File
     FUMPCL3    UF   E           K DISK
     F*** BEGIN ADD *** 07/19/02 **************************************
     FFDDAL     UF   E           K DISK
     F                                     RENAME(PDA01:PDA02)
     FFDOSL     UF   E           K DISK
     FNMIML2    UF   E           K DISK
      *

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D***  Begin Add  ***  05/20/09  **************************************
     D DAFDS           DS
     D  DARRN                397    400B 0
     D***  End   Add  ***  05/20/09  **************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     F*** END   ADD *** 07/19/02 **************************************
      *
      * Medical Management Billing Transaction File
      *
      *
      * Diagnosis Code Info File

     IPDA01
     I              DACO#                       XXCO#
     I              DASUB#                      XXSUB#
     I              DAFND                       XXFND
     I              DAFYR                       XXFYR
     I              DACASE                      XXCASE
      *
     F*  Diagnosis Code/Detail Charge Temp File
     IPDC01
     I              DCCO#                       XXCO#
     I              DCSUB#                      XXSUB#
     I              DCFND                       XXFND
     I              DCFYR                       XXFYR
     I              DCCASE                      XXCASE
      *
      * Duplicate Claims Workfile
     IPDU01
     I              DUCO#                       XXCO#
     I              DUSUB#                      XXSUB#
     I              DUFND                       XXFND
     I              DUFYR                       XXFYR
     I              DUCASE                      XXCASE
      *
     F*  EOMB Header Amount Transaction File
     IPEA01
     I              EACO#                       XXCO#
     I              EASUB#                      XXSUB#
     I              EAFND                       XXFND
     I              EAFYR                       XXFYR
     I              EACASE                      XXCASE
      *
      * Dental Bill Header Info File
     IPED01
     I              EDCO#                       XXCO#
     I              EDSUB#                      XXSUB#
     I              EDFND                       XXFND
     I              EDFYR                       XXFYR
     I              EDCASE                      XXCASE
      *
     F* EOMB Header EOB/Notes Transaction File
     IPEE01
     I              EECO#                       XXCO#
     I              EESUB#                      XXSUB#
     I              EEFND                       XXFND
     I              EEFYR                       XXFYR
     I              EECASE                      XXCASE
      *
      * EOMB Phys/HCFA Header File
     IPEH01
     I              EHCO#                       XXCO#
     I              EHSUB#                      XXSUB#
     I              EHFND                       XXFND
     I              EHFYR                       XXFYR
     I              EHCASE                      XXCASE
      *
     F* EOMB Tansaction File
     IPEO01
     I              EOCO#                       XXCO#
     I              EOSUB#                      XXSUB#
     I              EOFND                       XXFND
     I              EOFYR                       XXFYR
     I              EOCASE                      XXCASE
      *
      * Pharmacy Bill Header Info File
     IPEP01
     I              EPCO#                       XXCO#
     I              EPSUB#                      XXSUB#
     I              EPFND                       XXFND
     I              EPFYR                       XXFYR
     I              EPCASE                      XXCASE
      *
     F* Recon/Refund EOMB Transaction File
     IPER01
     I              ERCO#                       XXCO#
     I              ERSUB#                      XXSUB#
     I              ERFND                       XXFND
     I              ERFYR                       XXFYR
     I              ERCASE                      XXCASE
      *
      * EOMB Status Change Transaction File
     IPES01
     I              ESCO#                       XXCO#
     I              ESSUB#                      XXSUB#
     I              ESFND                       XXFND
     I              ESFYR                       XXFYR
     I              ESCASE                      XXCASE
      *
     F* EOMB Hosp/UB Header File
     IPEU01
     I              EUCO#                       XXCO#
     I              EUSUB#                      XXSUB#
     I              EUFND                       XXFND
     I              EUFYR                       XXFYR
     I              EUCASE                      XXCASE
      *
      * EOMB Auxiliary File
     IPE101
     I              E1CO#                       XXCO#
     I              E1SUB#                      XXSUB#
     I              E1FND                       XXFND
     I              E1FYR                       XXFYR
     I              E1CASE                      XXCASE
      *
     F* HBA Transaction File
     IPHB01
     I              HBCO#                       XXCO#
     I              HBSUB#                      XXSUB#
     I              HBFND                       XXFND
     I              HBFYR                       XXFYR
     I              HBCASE                      XXCASE
      *
      * Request Letter Transaction File
     IPLT01
     I              LTCO#                       XXCO#
     I              LTSUB#                      XXSUB#
     I              LTFND                       XXFND
     I              LTFYR                       XXFYR
     I              LTCASE                      XXCASE
      *
     F* Claim Med Service Transaction File
     IPMS01
     I              MSCO#                       XXCO#
     I              MSSUB#                      XXSUB#
     I              MSFND                       XXFND
     I              MSFYR                       XXFYR
     I              MSCASE                      XXCASE
      *
      * Claim Phys/Prov/Spec Transaction File
     IPPP01
     I              PPCO#                       XXCO#
     I              PPSUB#                      XXSUB#
     I              PPFND                       XXFND
     I              PPFYR                       XXFYR
     I              PPCASE                      XXCASE
      *
     F* All: BWC Treatment Plan Notes
     IPTD01
     I              TDCO#                       XXCO#
     I              TDSUB#                      XXSUB#
     I              TDFND                       XXFND
     I              TDFYR                       XXFYR
     I              TDCASE                      XXCASE
      *
      * All: BWC Treatment Plan ICD9 Code
     IPTI01
     I              TICO#                       XXCO#
     I              TISUB#                      XXSUB#
     I              TIFND                       XXFND
     I              TIFYR                       XXFYR
     I              TICASE                      XXCASE
      *
     F* All: BWC Treatment Plan File
     IPTR01
     I              TRCO#                       XXCO#
     I              TRSUB#                      XXSUB#
     I              TRFND                       XXFND
     I              TRFYR                       XXFYR
     I              TRCASE                      XXCASE
      *
      * Treatment Report Transaction File
     IPTX01
     I              TXCO#                       XXCO#
     I              TXSUB#                      XXSUB#
     I              TXFND                       XXFND
     I              TXFYR                       XXFYR
     I              TXCASE                      XXCASE
      *
     F* Work History File
     IPWH01
     I              WHCO#                       XXCO#
     I              WHSUB#                      XXSUB#
     I              WHFND                       XXFND
     I              WHFYR                       XXFYR
     I              WHCASE                      XXCASE
      *
      * Detail Charge File
     IPCA01
     I              CACO#                       XXCO#
     I              CASUB#                      XXSUB#
     I              CAFND                       XXFND
     I              CAFYR                       XXFYR
     I              CACASE                      XXCASE
     I*** BEGIN ADD *** 07/19/02 **************************************
     IPDA02
     I              DACO#                       XXCO#
     I              DASUB#                      XXSUB#
     I              DAFND                       XXFND
     I              DAFYR                       XXFYR
     I              DACASE                      XXCASE
     IPOS01
     I              OSCO#                       XXCO#
     I              OSSUB#                      XXSUB#
     I              OSFND                       XXFND
     I              OSFYR                       XXFYR
     I              OSCASE                      XXCASE
     IPIM01
     I              IMCO#                       XXCO#
     I              IMSUB#                      XXSUB#
     I              IMFND                       XXFND
     I              IMFYR                       XXFYR
     I              IMCASE                      XXCASE
     I*** END   ADD *** 07/19/02 **************************************
      *
     F* EOMB Dental Detail Transaction File
     IPDD01
     I              DDCO#                       XXCO#
     I              DDSUB#                      XXSUB#
     I              DDFND                       XXFND
     I              DDFYR                       XXFYR
     I              DDCASE                      XXCASE
      *
      * EOMB Detail EOB/Notes Transaction File
     IPDE01
     I              DECO#                       XXCO#
     I              DESUB#                      XXSUB#
     I              DEFND                       XXFND
     I              DEFYR                       XXFYR
     I              DECASE                      XXCASE
      *
     F* EOMB Phys/HCFA Detail Transaction File
     IPDH01
     I              DHCO#                       XXCO#
     I              DHSUB#                      XXSUB#
     I              DHFND                       XXFND
     I              DHFYR                       XXFYR
     I              DHCASE                      XXCASE
      *
      * EOMB Pharmacy Detail Transaction File
     IPDP01
     I              DPCO#                       XXCO#
     I              DPSUB#                      XXSUB#
     I              DPFND                       XXFND
     I              DPFYR                       XXFYR
     I              DPCASE                      XXCASE
      *
     F* EOMB Hospital/UB Detail Transaction Fileer Transfer File
     IPDU02
     I              DUCO#                       XXCO#
     I              DUSUB#                      XXSUB#
     I              DUFND                       XXFND
     I              DUFYR                       XXFYR
     I              DUCASE                      XXCASE
      *
      * EOMB Delete Transaction File
     IPEO02
     I              EOCO#                       XXCO#
     I              EOSUB#                      XXSUB#
     I              EOFND                       XXFND
     I              EOFYR                       XXFYR
     I              EOCASE                      XXCASE
      *
     F* EOMB Delete Auxiliary File
     IPE102
     I              E1CO#                       XXCO#
     I              E1SUB#                      XXSUB#
     I              E1FND                       XXFND
     I              E1FYR                       XXFYR
     I              E1CASE                      XXCASE
      *
      * Detail Charge Auxiliary File
     IPHC01
     I              HCCO#                       XXCO#
     I              HCSUB#                      XXSUB#
     I              HCFND                       XXFND
     I              HCFYR                       XXFYR
     I              HCCASE                      XXCASE
      *
      * Prov Bills Specific Errors File
     IPMG01
     I              MGCO#                       XXCO#
     I              MGSUB#                      XXSUB#
     I              MGFND                       XXFND
     I              MGFYR                       XXFYR
     I              MGCASE                      XXCASE
      *
     F* Claim Status History File
     IPCH01
     I              CHCO#                       XXCO#
     I              CHSUB#                      XXSUB#
     I              CHFND                       XXFND
     I              CHFYR                       XXFYR
     I              CHCASE                      XXCASE
      *
      * Claim Master File
     IPCM01
     I              CMCO#                       XXCO#
     I              CMSUB#                      XXSUB#
     I              CMFND                       XXFND
     I              CMEMP#                      XXEMP#
     I              CMDIV                       XXDIV
     I              CMFYR                       XXFYR
     I              CMCASE                      XXCASE
      *
     F* EDI Log History File
     IPLG01
     I              LGCO#                       XXCO#
     I              LGSUB#                      XXSUB#
     I              LGFND                       XXFND
     I              LGFYR                       XXFYR
     I              LGCASE                      XXCASE
      *
      * Medical Management Billing Transaction File
     IPMB01
     I              MBCO#                       XXCO#
     I              MBSUB#                      XXSUB#
     I              MBFND                       XXFND
     I              MBFYR                       XXFYR
     I              MBCASE                      XXCASE
      *
      * Pre-Certification Billing Transaction File
     IPPB01
     I              PBCO#                       XXCO#
     I              PBSUB#                      XXSUB#
     I              PBFND                       XXFND
     I              PBFYR                       XXFYR
     I              PBCASE                      XXCASE
      *
     F* Ser-up Bill Transaction File
     IPSB01
     I              SBCO#                       XXCO#
     I              SBSUB#                      XXSUB#
     I              SBFND                       XXFND
     I              SBFYR                       XXFYR
     I              SBCASE                      XXCASE
      *
      * Network Transaction Record File
     IPTR02
     I              TRCO#                       XXCO#
     I              TRSUB#                      XXSUB#
     I              TRFND                       XXFND
     I              TRFYR                       XXFYR
     I              TRCASE                      XXCASE
      *
      * Time/Materials Detail Charge File
     IPTM01
     I              TMCO#                       XXCO#
     I              TMSUB#                      XXSUB#
     I              TMFND                       XXFND
     I              TMFYR                       XXFYR
     I              TMCASE                      XXCASE
      *
     F* Accounts Receivable File
     IPAR01
     I              ARCO#                       XXCO#
     I              ARSUB#                      XXSUB#
     I              ARFND                       XXFND
     I              ARFYR                       XXFYR
     I              ARCASE                      XXCASE
      *
      * Pre-Certification Procedure Code File
     IPPO01
     I              POCO#                       XXCO#
     I              POSUB#                      XXSUB#
     I              POFND                       XXFND
     I              POFYR                       XXFYR
     I              POCASE                      XXCASE
      *
      * Claim Pre-Certification Procedure Code File
     IPCH02
     I              CHCO#                       XXCO#
     I              CHSUB#                      XXSUB#
     I              CHFND                       XXFND
     I              CHFYR                       XXFYR
     I              CHCASE                      XXCASE
      *
      * Pre-Certification master File
     IPPC01
     I              PCCO#                       XXCO#
     I              PCSUB#                      XXSUB#
     I              PCFND                       XXFND
     I              PCFYR                       XXFYR
     I              PCCASE                      XXCASE
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
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  KFLD           XXSUB#
     C*******************  KFLD           XXFND
     C*******************  KFLD           XXFYR
     C*******************  KFLD           XXCASE
     C*** End dlt ***** 12/05/01 **************************************
     C*** Begin add *** 12/05/01 **************************************
     C                   KFLD                    XXFSUB            5 0
     C                   KFLD                    XXFFND            3 0
     C                   KFLD                    XXFFYR            3 0
     C                   KFLD                    XXFCAS            7 0
     C*** End add ***** 12/05/01 **************************************
      *
      *****************************************************************
      *  KEYS
      *****************************************************************
      *
      * PLIST
      *****************************************************************
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
      *****************************************************************
      *  MAIN
      *****************************************************************
      *
      * call program to translate WCMS CO#/FND to WCMC CO#/SUB#/FND
      *
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@FGRP        ##WCF#
     C                   EXSR      XLATE
     C*
     C*** Begin add *** 12/05/01 **************************************
     C     ##RTCD        IFEQ      'F'
     C                   GOTO      ENDPGM
     C                   ENDIF
     C*** End add ***** 12/05/01 **************************************
     C*** Begin dlt *** 12/05/01 **************************************
     C*********  ##RTCD    IFNE 'F'
     C*** End dlt ***** 12/05/01 **************************************
     C                   MOVE      ##SUB#        ETOSUB
     C                   MOVE      ##FND         ETOFND
     C*
     C                   MOVE      @@TGRP        ##WCF#
     C                   EXSR      XLATE
     C*
     C*** Begin add *** 12/05/01 **************************************
     C     ##RTCD        IFEQ      'F'
     C                   GOTO      ENDPGM
     C                   ENDIF
     C*** End add ***** 12/05/01 **************************************
     C*** Begin dlt *** 12/05/01 **************************************
     C*********  ##RTCD    IFNE 'F'
     C*** End dlt ***** 12/05/01 **************************************
     C                   MOVE      ##SUB#        ETSUB#
     C                   MOVE      ##FND         ETFND
     C*
     C* Write record to WCMS Employer Transfer File
     C* used to maintain a history
     C*
     C                   MOVE      @@CO#         ETCO#
     C                   MOVE      @@TEMP        ETEMP#
     C                   MOVE      @@TDIV        ETDIV
     C                   MOVE      @@TFYR        ETFYR
     C                   MOVE      @@TCAS        ETCASE
     C                   MOVE      @@FEMP        ETOEMP
     C                   MOVE      @@FDIV        ETODIV
     C                   MOVE      @@FFYR        ETOFYR
     C                   MOVE      @@FCAS        ETOCAS
     C                   MOVE      Q1USER        ETUSER
     C                   MOVE      Q1PGM         ETPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ETDT
     C                   MOVE      DSTIME        ETTIME
     C                   MOVE      Q1USER        ETIUSR
     C                   MOVE      Q1PGM         ETIPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ETIDT
     C                   MOVE      DSTIME        ETITIM
     C*
     C                   WRITE     PET01
     C*
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  ENDIF
     C*******************  ENDIF
     C*** End dlt ***** 12/05/01 **************************************
      *
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE @@CO#     ##CO#
     C*******************  MOVE @@FGRP    ##WCF#
     C*******************  EXSR XLATE
     C*** End dlt ***** 12/05/01 **************************************
     C*
     C                   MOVE      @@CO#         XXCO#
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE ##SUB#    XXSUB#
     C*******************  MOVE ##FND     XXFND
     C*** End dlt ***** 12/05/01 **************************************
     C                   MOVE      @@FEMP        XXEMP#
     C                   MOVE      @@FDIV        XXDIV
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE @@FFYR    XXFYR
     C*******************  MOVE @@FCAS    XXCASE
     C*** End dlt ***** 12/05/01 **************************************
     C*** Begin add *** 12/05/01 **************************************
     C                   MOVE      ETOSUB        XXFSUB
     C                   MOVE      ETOFND        XXFFND
     C                   MOVE      @@FFYR        XXFFYR
     C                   MOVE      @@FCAS        XXFCAS
     C*** End add ***** 12/05/01 **************************************
      *
     C     FRKY          CHAIN     PDA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDA01
     C***  Begin Add  ***  05/20/09  **************************************
     C                   EXSR      UPDSYS
     C***  End   Add  ***  05/20/09  **************************************
     C     FRKY          READE     PDA01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PDC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDC01
     C     FRKY          READE     PDC01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PDU01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDU01
     C     FRKY          READE     PDU01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEA01
     C     FRKY          READE     PEA01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PED01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PED01
     C     FRKY          READE     PED01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PEE01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEE01
     C     FRKY          READE     PEE01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEH01
     C     FRKY          READE     PEH01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PEO01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEO01
     C     FRKY          READE     PEO01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEP01
     C     FRKY          READE     PEP01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PER01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PER01
     C     FRKY          READE     PER01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PES01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PES01
     C     FRKY          READE     PES01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PEU01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEU01
     C     FRKY          READE     PEU01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PE101                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PE101
     C     FRKY          READE     PE101                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PHB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PHB01
     C     FRKY          READE     PHB01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PLT01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PLT01
     C     FRKY          READE     PLT01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PMS01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PMS01
     C     FRKY          READE     PMS01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PPP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PPP01
     C     FRKY          READE     PPP01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PTD01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTD01
     C     FRKY          READE     PTD01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PTI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTI01
     C     FRKY          READE     PTI01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PTR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTR01
     C     FRKY          READE     PTR01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PTX01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTX01
     C     FRKY          READE     PTX01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PWH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PWH01
     C     FRKY          READE     PWH01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PCA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCA01
     C     FRKY          READE     PCA01                                  60
     C                   ENDDO
     C*** BEGIN ADD *** 07/19/02 **************************************
      *
     C     FRKY          CHAIN     PDA02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDA02
     C     FRKY          READE     PDA02                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     POS01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    POS01
     C     FRKY          READE     POS01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PIM01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PIM01
     C     FRKY          READE     PIM01                                  60
     C                   ENDDO
     C*** END   ADD *** 07/19/02 **************************************
     C*
      *
     C     FRKY          CHAIN     PDD01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDD01
     C     FRKY          READE     PDD01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PDE01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDE01
     C     FRKY          READE     PDE01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PDH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDH01
     C     FRKY          READE     PDH01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PDP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDP01
     C     FRKY          READE     PDP01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PDU02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PDU02
     C     FRKY          READE     PDU02                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PEO02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PEO02
     C     FRKY          READE     PEO02                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PE102                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PE102
     C     FRKY          READE     PE102                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PHC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PHC01
     C     FRKY          READE     PHC01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PMG01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PMG01
     C     FRKY          READE     PMG01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PCH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCH01
     C     FRKY          READE     PCH01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCM01
     C     FRKY          READE     PCM01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PLG01
     C     FRKY          READE     PLG01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PMB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PMB01
     C     FRKY          READE     PMB01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PPB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PPB01
     C     FRKY          READE     PPB01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PSB01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PSB01
     C     FRKY          READE     PSB01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PTR02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTR02
     C     FRKY          READE     PTR02                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PTM01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PTM01
     C     FRKY          READE     PTM01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PAR01
     C     FRKY          READE     PAR01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PPO01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PPO01
     C     FRKY          READE     PPO01                                  60
     C                   ENDDO
     C*
      *
     C     FRKY          CHAIN     PCH02                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PCH02
     C     FRKY          READE     PCH02                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PPC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PPC01
     C     FRKY          READE     PPC01                                  60
     C                   ENDDO
      *
     C*** Begin add *** 12/05/01 **************************************
     C     ENDPGM        TAG
     C*** End add ***** 12/05/01 **************************************
     C                   MOVE      '1'           *INLR
      *
      *****************************************************************
      *  LOADTO
      *****************************************************************
      *
     C     LOADTO        BEGSR
      *
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE @@CO#     ##CO#
     C*******************  MOVE @@TGRP    ##WCF#
     C*******************  EXSR XLATE
     C*** End dlt ***** 12/05/01 **************************************
     C*
     C                   MOVE      @@CO#         XXCO#
     C*** Begin dlt *** 12/05/01 **************************************
     C*******************  MOVE ##SUB#    XXSUB#
     C*******************  MOVE ##FND     XXFND
     C*** End dlt ***** 12/05/01 **************************************
     C*** Begin add *** 12/05/01 **************************************
     C                   MOVE      ETSUB#        XXSUB#
     C                   MOVE      ETFND         XXFND
     C*** End add ***** 12/05/01 **************************************
     C                   MOVE      @@TEMP        XXEMP#
     C                   MOVE      @@TDIV        XXDIV
     C                   MOVE      @@TFYR        XXFYR
     C                   MOVE      @@TCAS        XXCASE
      *
     C                   ENDSR
     C*
     C***  Begin Add  ***  05/20/09  **************************************
     C*====================================================================
     C* Update the system values
     C*====================================================================
     C     UPDSYS        BEGSR
     C*
     C                   CALL      'FA275R'
     C                   PARM      DARRN         ##RRN             9 0
     C                   PARM      'U'           ##MODE            1
     C                   PARM      'MC'          ##FROM            2
     C                   PARM      'SY'          ##TO              2
     C*
     C                   ENDSR
     C***  End   Add  ***  05/20/09  **************************************
     C*
      *****************************************************************
      *  XLATE
      *****************************************************************
      *
     C     XLATE         BEGSR
     C*
     C                   CALL      'FA700R'                             99
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##WCF#            3
     C                   PARM      *ZEROS        ##SUB#            5
     C                   PARM      *ZEROS        ##FND             3
     C*
     C                   ENDSR
