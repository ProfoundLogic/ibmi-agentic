     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('WTBNDDIR')
     H*
     H/TITLE W8054R - CLAIMS ELIGIBILITY FILE EXTRACT - EXTRACT PGM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     CARRIES OUT THE CLAIMS ELIGIBILITY FILE EXTRACT           *
     H*     PROCESS                                                   *
     H*                                                               *
     H*  CALLED FROM: W8154C - CLAIMS ELIGIBILITY FILE EXTRACT        *
     H*                        OVERRIDE PROGRAM                       *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED: 08/16/07       PROGRAMMER: 62      PROJECT: MC300   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/04/24 C:053 Support: 133/117784                            *
     H*                Allow alpha SS# for LUBA                       *
     H*                                                               *
     H* 06/11/21 C:079 Support: 152/95165                             *
     H*                Add time to run date logic                     *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420                                 *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 07/10/19 C:079 Project: 152MAP                                *
     H*                Add call to company API                        *
     H*                                                               *
     H* 05/20/19 C:079 Project: 102MHV                                *
     H*                Add Date of Death and Settlement Date          *
     H*                                                               *
     H* 08/01/18 C:079 Support: 99 72530                              *
     H*                Change %Found to %Eof on Reade on WMCHP        *
     H*                                                               *
     H* 07/25/18 C:180 Project: 146MIC                                *
     H*                Add claim reopened status processing.          *
     H*                                                               *
     H* 10/09/17 C:079 Project: 146MGT                                *
     H*                Add Body Area and Policy Number                *
     H*                                                               *
     H* 06/30/17 C:079 Project: MC325                                 *
     H*                Change logic for #days since closed            *
     H*                                                               *
     H* 05/01/17 C:079 Project: MC325                                 *
     H*                Add logic for SA016R                           *
     H*                Add Death logic                                *
     H*                Remove IEelig                                  *
     H*                                                               *
     H* 03/10/17 C:079 Project: 90MHY                                 *
     H*                Add data area CEDATA2 for PBM                  *
     H*                Change document ID to generic name             *
     H*                                                               *
     H* 02/14/17 C:212 Project: 90MHY                                 *
     H*                PBM Eligibility related changes                *
     H*                                                               *
     H* 11/14/16 C:079 Project: 90MIB                                 *
     H*                Fix 180 day logic                              *
     H*                                                               *
     H* 11/10/16 C:309 Project: 90MIB                                 *
     H*                Add logic to Exclude Claims where Exclude      *
     H*                by Group                                       *
     H*                                                               *
     H* 06/20/16 C:309 Project: 136MHB                                *
     H*                Load new field "Jurisdiction Claim Number"     *
     H*                                                               *
     H* 03/18/16 C:079 Project: 45MKI                                 *
     H*                Make CEJRST available for all clients          *
     H*                                                               *
     H* 02/23/16 C:079 Project: 45MKI                                 *
     H*                Add logic to use configuration file            *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 05/07/14 C:280 Support: 85387                                 *
     H*                3) add code to open and close file base on mcif*
     H*                                                               *
     H* 05/07/14 C:296 Support: 85387                                 *
     H*                1) Making all MC files user Open/Close.        *
     H*                2) All W group files should be Open.           *
     H*                                                               *
     H* 04/11/14 C:280 Project: MC375                                 *
     H*                -Add code to populate icd code 9 or 10 depending upon
     H*                 icd flag                                      *
     H*                                                               *
     H* 09/23/13 C:079 Support: 83547 MC114                           *
     H*                -Change act group to *caller                   *
     H*                                                               *
     H*  04/04/13   C:277   PROJECT: 133MNC                           *
     H*                     Include active and inactive providers     *
     H*                                                               *
     H* 09/03/12 C:189 Support: 80533 WC133                           *
     H*                -The fix for 79371 caused incorrect Claim# to be
     H*                 printed on the error report.                  *
     H*                                                               *
     H* 08/01/12 C:280 Project: MC363                                 *
     H*                -Add second address line to file               *
     H*                                                               *
     H* 03/06/12 C:251 Support: 79371 MC133                           *
     H*                -Report correct claim number in summary report *
     H*                                                               *
     H* 01/09/12 C:251 Project: 114MCX                                *
     H*                -Set claim closed date as per the CMRXEL flag  *
     H*                                                               *
     H* 12/16/11 C:062 Project: WC114 78674                           *
     H*                Don't populate CECVFL or CEDNY if CSI.         *
     H*                                                               *
     H* 10/20/11 C:062 Project: WC133 78303                           *
     H*                -Populate the area code.                       *
     H*                                                               *
     H* 10/11/11 C:062 Project: 78219                                 *
     H*                Populate CECVFL and CEDNY.                     *nt.
     H*                                                               *
     H* 05/03/11 C:251 Project: 133MKP                                *
     H*                -Modify SUBSET Criteria when called for 133 client.
     H*                                                               *
     H* 02/01/11 C:220 Project: 114MYA                                *
     H*                -Modify Rx Eligibility extract file            *
     H*                                                               *
     H* 11/17/10 C:227 Project: 133MJR                                *
     H*                -Exclude record only claims (CMLTFL = 'R')     *
     H*                                                               *
     H* 10/20/10 C:227 Project: 133MJR                                *
     H*                -Incl denied and controverted claims.          *
     H*                -Exclude claims if state <> 'AR' or 'MS'       *
     H*                                                               *
     H* 07/30/10 C:79  SUPPORT: 73731                                 *
     H*                Eliminate time from claim history key list     *
     H*                                                               *
     H* 07/06/10 C:62  SUPPORT: 73458                                 *
     H*                Exclude denied, controverted and death claims. *
     H*                                                               *
     H* 12/16/09 C:79  135WNW                                         *
     H*                For client 135 omit Claimants where the NCCI   *
     H*                Carrier # = 68666                              *
     H*                                                               *
     H* 03/31/08 C:47  114MQN                                         *
     H*                CHECK WMEMP:EMDCRX FLAG TO DETERMINE WHETHER   *
     H*                AN EMPLOYER SHOULD BE EXCLUDED OR NOT          *
     H*                FROM PROCESSING BASED ON THE VALUE OF THIS     *
     H*                DCRX-EXPORT-EXCLUDE FLAG.                      *
     H*
     H*  03/12/08 C:62 PROJECT 114MPE                                 *
     H*                EXCLUDE CLAIMS IF THE EMPLOYER IS EXCLUDED     *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*  SMCOL    PCO01: CO                                   Company *
     H*  SMORL    POR01: CO/PGM                              Override *
     H*  SMUPL    PUP01: USER                               User Parm *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FWMCMLG    IF   E           K DISK    USROPN
     FWMCHL     IF   E           K DISK    USROPN
     FWMFNL     IF   E           K DISK    USROPN
     FWMCDL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK    USROPN
     FFADAL2    IF   E           K DISK    USROPN
     F*
     FFMCML1    IF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM02)
     FFMCHL2    IF   E           K DISK    USROPN
     F                                     RENAME(PCH01:PCH02)
     FFMCDL     IF   E           K DISK    USROPN
     F                                     RENAME(PCD01:PCD02)
     FFMEML     IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:PEM02)
     FFAPPL1    IF   E           K DISK    USROPN
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMCXL     IF   E           K DISK
     FWMAHL     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWDF2L     IF   E           K DISK    USROPN
     FSMIEP     IF   E             DISK
     Fwmrpl4    if   e           k disk
     FWACEP4    O    E             DISK
     FWMLLL     IF   E           K DISK    USROPN
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d rtvSettings     pr                  Extpgm('SA016R')
     d  @@Cchg                        1
     d  @@Cifs                        1
     d  @@Csts                        1
     d  @@Cdny                        1
     d  @@Ccvfl                       1
     d  @@Cgrp                        1
     d  @@Cemp                        1
     d  @@Eifs                        1
     d  @@Eagt                        1
     d  @@Eemp                        1
     d  @@Fifs                        1
     d  @@Ftyp                        1
     d  @@Udup                        1
     d  @@Udrc                        1
     d  @@Urel                        1
     d  @@Upcl                        1
     d  @@Updn                        1
     d  @@Udpn                        1
     d  @@ClDays                      5  0
     d  @@IncZro                      1
     d  @@Crcd                        1
     d  @@CrDth                       1
     d  @@RtCd                        1
     d  @@Rsts                        1
     d  @@Cchg         s              1
     d  @@Cifs         s              1
     d  @@Csts         s              1
     d  @@Cdny         s              1
     d  @@Ccvfl        s              1
     d  @@Cgrp         s              1
     d  @@Cemp         s              1
     d  @@Eifs         s              1
     d  @@Eagt         s              1
     d  @@Eemp         s              1
     d  @@Fifs         s              1
     d  @@Ftyp         s              1
     d  @@Udup         s              1
     d  @@Udrc         s              1
     d  @@Urel         s              1
     d  @@Upcl         s              1
     d  @@Updn         s              1
     d  @@Udpn         s              1
     d  @@ClDays       s              5  0
     d  @@IncZro       s              1
     d  @@Crcd         s              1
     d  @@CrDth        s              1
     d  @@RtCd         s              1
     d  @@Rsts         s              1
     DGetOnlyCo#       pr             3  0
     d  OutCo#         s                   Like(CMco#) Inz(0)
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  CLM#                   1     15
     D  FND                    1      3  0
     D  FYR                    4      6  0
     D  CASE                   7     13  0
     D*
     D  xxSysDt        s              7  0
     D  xxCurDt        s              8  0
     D  xxCurDt1       s               d
     D  intCloseDt     s              8  0
     D  intCloseDt1    s               d
     D  DtBef180dys    s              8  0
     D  intRunDat      s              7  0
     D  strRunDat      s             10A
     D  strCurDate     s             10A
     d*** Beg Add *** 06/11/21 ****************************************
     D  strCurTime     s              6A
     D  intRunTime     s              6S 0
     d*** End Add *** 06/11/21 ****************************************
      *
     d*** Beg Chg *** 06/11/21 ****************************************
     d*****  ceData         s             10    dtaara
     d  ceData         s             17    dtaara
     d*** End Chg *** 06/11/21 ****************************************
     D  ceData2        s             10    dtaara
      *
     D  ceDataDs       DS
     D  ceDAMM                        2
     D  ceDAS1                        1
     D  ceDADD                        2
     D  ceDAS2                        1
     D  ceDACY                        4
     d*** Beg Add *** 06/11/21 ****************************************
     D  ceDAd2                        1
     D  ceTime                        6
     d*** End Add *** 06/11/21 ****************************************

     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D XTITL           C                   CONST('Claims Eligibili-
     D                                     ty Summary Report')
     D DIGITS          C                   CONST('0123456789')
     D PPCDOC          C                   CONST('CH0000076WCAS99210')
     D*
     D*****************************************************************
     D* Standalone fields
     D*****************************************************************
     D isSMiep         s              1    inz(*blanks)
     D xxVendor        s             10    inz

     IPCM02
     I              CMOSCL                      CMOXCL

     C*   KEY LISTS
     C*****************************************************************
     C*
     C*   Agent Assignment
     C*
     C     AAKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHAGT#
     C*
     C*   Agent Master
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   Agent History
     C*
     C     AHKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C                   KFLD                    KKEFDT
     C*
     C     AHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*   Company
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Case History
     C*
     C     CHKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     CHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKEFDT
     C*
     C     CHKY3         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKEFDT
     C*
     C     CHKY4         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Claim Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C*   Fund
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C*
     C*   Diagnosis Code
     C*
     C     DAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*   Claimant demographics
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   Claim Physician/Provider Specialist
     C*
     C     PPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Employer
     C*
     C     EMKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     EMKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  Output Routing
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C*   User Parameter
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*
     C*   Group Auxiliary
     C*
     C     F2KY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CHEFDT        KKEFDT
     C     *LIKE         DEFINE    CHTIME        KKTIME
     C     *LIKE         DEFINE    CHEFDT        X1FRDT
     C     *LIKE         DEFINE    CHEFDT        X1TODT
     C     *LIKE         DEFINE    CHEFDT        XXCODT
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *like         define    CEpol#        xxPol#
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DCID           12
     C                   PARM                    @@DCI2           50
     C                   PARM                    @@Vendor         10
     C                   PARM                    @@PMOA            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     DTYYM1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C*   Initialize
     C*****************************************************************
     C*
     C*  PGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*  Print Routing Page
     C*
     C                   EXSR      SRRTE
     C*
     C                   OPEN      WMFNL
     C*  CALL TO SA001R TO FIND OUT WHETHER CLIENT HAS WCMS
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILL
     C*
     C* IF WCMS THEN READ FILE WMCMP
     C     @@WCIF        IFEQ      'Y'
     C*  Read 1st record
     C*
     C                   OPEN      WMCMLG
     C                   OPEN      WMCHL
     C                   OPEN      WMAHL
     C                   OPEN      WMAAL1
     C                   OPEN      WMAGL
     C                   OPEN      WDF2L
     C                   Open      wmlll
     C                   READ      PCM01                                  99
     C                   EXCEPT    HEAD
     C     *IN99         DOWEQ     '0'
     C*
     C*  If subset criteria met
     C*
     C                   EXSR      SUBSET
     C*
     C     MEETSS        IFEQ      *ON
     C                   EXSR      ERRCK
     C*
     C     *IN90         IFEQ      *ON
     C     COUNTE        IFEQ      *ZEROS
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C                   ADD       1             COUNTE
     C                   EXSR      BREAK2
     C                   EXSR      SRDET
     C                   ELSE
     C                   ADD       1             COUNTW
     C                   OPEN      WMCDL
     C                   OPEN      WMEML
     C     @@MCIF        IFEQ      'Y'
     C                   OPEN      FADAL2
     C                   OPEN      FAPPL1
     C                   ENDIF
     C                   EXSR      WRITER
     C     @@MCIF        IFEQ      'Y'
     C                   CLOSE     FADAL2
     C                   CLOSE     FAPPL1
     C                   ENDIF
     C                   CLOSE     WMCDL
     C                   CLOSE     WMEML
     C                   ENDIF
     C                   ENDIF                                                  MEETSS
     C*
     C*  Read next record
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO                                                  WRK RECORD
     C                   CLOSE     WMCMLG
     C                   CLOSE     WMCHL
     C                   Close     wmlll
     C*
     C* IF NON WCMS THEN READ FILE FMCMP
     C                   ELSE
     C*
     C*
     C     @@MCIF        IFEQ      'Y'
     C*
     C*
     C*
     C*  Read 1st record
     C*
     C                   OPEN      FMCML1
     C                   READ      PCM02                                  99
     C                   EXCEPT    HEAD
     C     *IN99         DOWEQ     '0'
     C*
     C*  If subset criteria met
     C*
     C                   EXSR      SUBSET
     C*
     C     MEETSS        IFEQ      *ON
     C                   EXSR      ERRCK
     C*
     C     *IN90         IFEQ      *ON
     C     COUNTE        IFEQ      *ZEROS
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C                   ADD       1             COUNTE
     C                   EXSR      BREAK2
     C                   EXSR      SRDET
     C                   ELSE
     C                   ADD       1             COUNTW
     C                   OPEN      FADAL2
     C                   OPEN      FMCHL2
     C                   OPEN      FMCDL
     C                   OPEN      FMEML
     C                   OPEN      FAPPL1
     C                   EXSR      WRINWC
     C                   CLOSE     FADAL2
     C                   CLOSE     FMCHL2
     C                   CLOSE     FMCDL
     C                   CLOSE     FMEML
     C                   CLOSE     FAPPL1
     C                   ENDIF
     C                   ENDIF                                                  MEETSS
     C*
     C*  Read next record
     C*
     C                   READ      PCM02                                  99
     C                   ENDDO                                                  WRK RECORD
     C*
     C                   CLOSE     FMCML1
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C                   CLOSE     WMFNL
     C*  Last Record processing
     C*
     C                   EXSR      BREAK1
     C                   EXCEPT    TOTAL
     C*
     C                   EXSR      CRTDOC
      /Free
       If xxVendor = 'OPTUM';
          @@dcid = 'PBM.txt';
       Endif;
      /End-free
     C*
     C*  End of Processing
     C*
      /Free
       kkPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
      /Free
          strCurDate = %Char(%Date():*usa0);
        //*** Beg Add *** 06/11/2021 **************************************************
          strCurTime = %Editc(%Dec(%Time():*HMS):'X');
        //*** End Add *** 06/11/2021 **************************************************
        //*** Beg Chg *** 06/11/2021 **************************************************
        //***  ceDataDs = %Subst(StrCurDate:1:2) + '/' + %Subst(StrCurDate:3:2) +
        //***   '/' + %Subst(StrCurDate:5:4) ;
          ceDataDs = %Subst(StrCurDate:1:2) + '/' + %Subst(StrCurDate:3:2) +
           '/' + %Subst(StrCurDate:5:4) + '-' + %Trim(strCurTime);
        //*** End Chg *** 06/11/2021 **************************************************

          If ceDatads <> *Blanks;
             Select;
             When xxVendor <> 'OPTUM';
                ceData = ceDatads;
                out *lock ceData ;
                Unlock ceData;
             When xxVendor = 'OPTUM';
               ceData2 = ceDatads;
               out *lock ceData2 ;
               Unlock ceData2;
             Endsl;
          EndIf;
      /End-Free
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* BREAK1 - CHECK IF AT END OF PAGE AND BREAK IF SO
     C*****************************************************************
     CSR   BREAK1        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  BREAK
     C*****************************************************************
     C* BREAK2 - CHECK IF AT END OF PAGE AND BREAK IF SO
     C*****************************************************************
     CSR   BREAK2        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  BREAK
     C*****************************************************************
     C* CRTDOC - CREATE THE DOC NAME
     C*****************************************************************
     CSR   CRTDOC        BEGSR
     C*
     C     HIT           IFEQ      'Y'
     C*
     C                   TIME                    DSDTTM
     C*
     C     DSTIME        IFLE      120000
     C                   CALL      'SRPAST8'
     C     DSDATE        PARM      DSDATE        ##DATE            8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM      *BLANKS       ##ERR             1
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT7            7
     C                   MOVE      XXDAT7        XXDAT6            6
     C*
     C     'CE'          CAT(P)    XXDAT6:0      @@DCID
     C                   CAT       '.TXT':0      @@DCID
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '135'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDAT6
     C                   MOVE      DSTIME        XXTIME            6
     C     PPCDOC        CAT(P)    XXDAT6:0      @@DCI2
     C                   CAT       XXTIME:0      @@DCI2
     C                   CAT       '.CLM':0      @@DCI2
     C                   OTHER
     C                   CLEAR                   @@DCI2
     C                   ENDSL
     C*
     C                   ELSE
     C                   CLEAR                   @@DCID
     C                   CLEAR                   @@DCI2
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* ERRCK - Check for errors
     C*****************************************************************
     CSR   ERRCK         BEGSR
     C*
     C                   SETOFF                                       90
     C*
     C* *IN90 = *ON, Write error detail line and not write WACEP record
     C*
     C* Does SS# have any non-numeric characters
     C*
     C*** Beg Add *** 10/04/24 ***************************************
     C     XXCLT#        IFNE      '133'
     C*** End Add *** 10/04/24 ***************************************
     C     DIGITS        CHECK     CMSS#         N                        90
     C*** Beg Add *** 10/04/24 ***************************************
     C                   ENDIF                                                  CMLTFL='R'
     C*** End Add *** 10/04/24 ***************************************
     C*
     C                   ENDSR                                                  ERRCK
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        COUNTE            9 0
     C                   Z-ADD     *ZEROS        COUNTW            9 0
     C                   TIME                    DSDTTM
     C                   MOVE      'N'           HIT               1
      /Free
                         OutCo# =  GetOnlyCo#();
      /End-Free
     C*
     C                   If        OutCo# = 0
     C                   MOVE      @@CO#         XXCO#
     C                   Else
     C                   eval      xxCo# = OutCo#
     C                   Endif
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C*
     C     Q1PRMS        IFGT      3
     C                   Eval      XXVendor = @@Vendor
     C                   ENDIF
     C                   MOVE      ' '           XXPMOA            1
     C     Q1PRMS        IFGT      4
     C                   MOVE      @@PMOA        XXPMOA
     C                   ENDIF
      /free

            rtvSettings(@@Cchg:@@Cifs:@@Csts:@@Cdny:@@Ccvfl:@@Cgrp:
                         @@Cemp:@@Eifs:@@Eagt:@@Eemp:@@Fifs:@@Ftyp:
                         @@Udup:@@Udrc:@@Urel:@@Upcl:@@Updn:@@Udpn:
                         @@ClDays:@@IncZro:@@Crcd:@@CrDth:@@RtCd:@@Rsts);
                         if @@RtCd = '1';
      /end-free
     C                   eval      isSMiep = 'Y'
     C                   endif
      /Free
         Select;
         When xxVendor <> 'OPTUM';
            in *lock *dtaara;
            ceDatads = ceData;
         When xxVendor = 'OPTUM';
            in *lock *dtaara;
            ceDatads = ceData2;
         Endsl;
      /End-Free
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxSysDt
      *
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* SRDET - Write Detail Record
     C*****************************************************************
     CSR   SRDET         BEGSR
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C*  Detail record print
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C                   ENDSR                                                  SRDET
     C*****************************************************************
     C* SRRTE - Print routing page
     C*****************************************************************
     CSR   SRRTE         BEGSR
     C*
     C*   GET COMPANY, USER, INFO.
     C*
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   Output Routing - detail report 0
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
     C*
     C*  User Name if no Distribution specified in routing
     C*
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
     C*
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  XTITL         ORTITL
     C*
     C                   ENDIF
     C*
     C*  Center Title in Field
     C*
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
     C*
     C* Center Company name in display field
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C                   ENDSR                                                  SRRTE
     C*****************************************************************
     C* SUBSET - Check if member of records processed subset
     C*****************************************************************
     CSR   SUBSET        BEGSR
     C*
     C                   MOVE      *ON           MEETSS            1
     C*
     C*  MEETSS = *ON, if to be processed
     C*
     C*
     C*
     C*
     C* IS THIS A TEST GROUP
     C*
     C                   CLEAR                   PFN01
     C     FNKY          CHAIN     PFN01                              95
     C     *IN95         IFEQ      *OFF
     C     FNTEST        IFEQ      'Y'
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C     @@WCIF        IFEQ      'Y'
     C     MEETSS        ANDEQ     *ON
     C     XXCLT#        ANDEQ     '135'
     C*
     C     F2KY          CHAIN     PF201                              95
     C*
     C     *IN95         IFEQ      *OFF
     C     F2CAR#        ANDEQ     68666
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     @@WCIF        IFEQ      'Y'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C*
     C                   CLEAR                   PAG01
     C     AHKY1         SETGT     PAH01
     C     AHKY2         READPE    PAH01                                  99
     C     *IN99         IFEQ      *OFF
     C     AAKY          CHAIN     PAA01                              99
     C     *IN99         IFEQ      *OFF
     C     AGKY          CHAIN     PAG01                              99
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXCLT#        IFNE      '133'
     C     AGEEXP        IFEQ      'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF                                                  XXCLT#<>'133'
     C                   ENDIF
     C*
     C*
     C     @@WCIF        IFEQ      'Y'
     C                   OPEN      WMEML
     C     EMKY1         CHAIN     PEM01                              67
     C     *IN67         IFEQ      *OFF
     C     EMDCRX        IFEQ      'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C                   CLOSE     WMEML
     C                   ENDIF
     C     @@WCIF        IFEQ      'Y'
     C*
     C* Exclude Denied Claims Not client 133
     C*
     C     XXCLT#        IFNE      '133'
     C     CMDNY         IFEQ      'Y'
     C     isSMiep       andne     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   if        isSMiep = 'Y' and CMdny = 'Y'
     C                             and @@cDny = 'Y'
     C                   eval      meetSS = *off
     C                   Endif
     C*
     C* Exclude Controverted Claims
     C*
     C     CMCVFL        IFEQ      'Y'
     C     isSMiep       andne     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   if        isSMiep = 'Y' and CMcvFl= 'Y'
     C                             and @@cCvfl = 'Y'
     C                   eval      meetSS = *off
     C                   Endif
     C                   ENDIF
     C* Exclude Death Claims
     C                   if        isSMiep = 'Y' and CMdod <> 0
     C                             and @@crDth = 'Y'
     C                   eval      meetss = *off
     C                   endif
     C                   if        isSMiep <> 'Y' and CMdod <> 0
     C                   eval      meetss = *off
     C                   endif
     C                   if        isSMiep = 'Y' and CMltFl = 'R'
     C                             and @@crcd = 'Y'
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   ENDIF                                                  XXCLT#<>'133'
     C*
     C*
     C                   ENDIF
     C* Client 133
     C     XXCLT#        IFEQ      '133'
     C*
     C     CMLTFL        IFEQ      'R'
     C     isSMiep       andne     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  CMLTFL='R'
     C                   if        isSMiep = 'Y' and CMltFl = 'R'
     C                             and @@crcd = 'Y'
     C                   eval      meetss = *off
     C                   endif
     C*
     C     CMdod         IFNE      0
     C     XXPMOA        ANDEQ     'P'
     C     isSMiep       andne     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  CMDTH='Y'
     C                   if        isSMiep = 'Y' and CMdod <> 0
     C                             and @@crDth = 'Y' and xxPmoa = 'P'
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   ENDIF                                                  XXCLT#='133'
     C     XXCLT#        IFEQ      '114'
     C     CMLTFL        ANDEQ     'R'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  CLT-114
     C* Exclude Claims by group
      /Free
         if FNclelig= 'Y';
            meetSS = *Off;
         Endif;

         If isSMiep = 'Y' and @@cchg = 'Y';
            If ceDataDs <> *Blanks;
               strRunDat = ceDACY + ceDAMM + ceDADD;
               Monitor;
                  intRunDat = %Dec(strRunDat:8:0) - 19000000;
               On-Error;
                  intRunDat = *Zeros;
               EndMon;
            //*** Beg Add *** 06/11/21 *************************************
               Monitor;
                  IntRunTime = %Dec(CEtime:6:0);
               On-Error;
                  IntRunTime = 0;
               EndMon;
            //*** End Add *** 06/11/21 *************************************
            Else;
               intRunDat = *Zeros;
            //*** Beg Add *** 06/11/21 *************************************
               IntRunTime = 0;
            //*** End Add *** 06/11/21 *************************************
            EndIf;

            If CMdt < intRunDat;
               meetSS = *Off;
            EndIf;
            //*** Beg Add *** 06/11/21 *************************************
            If CMdt = intRunDat and (CMtime < IntRunTime and IntRunTime <> 0);
               meetSS = *Off;
            EndIf;
            //*** End Add *** 06/11/21 *************************************
         EndIf;

        // Include closed claims only if closed within the last 180 days
        // Get the most recent claim Status
         If @@WCIF = 'Y';
         Setgt chky2 pCH01;
         Readpe chky1 pCH01;
         If Not %Eof (WMchl);
         If CHocst = 'C';
            If @@cldays > 0 and @@csts = 'O' and isSMiep = 'Y';
               If CHefdt <> *Zeros;
                  intCloseDt = 19000000 + CHefdt;
                  xxCurDt = 19000000 + xxSysDt;
                  intCloseDt1= %Date(xxCurDt) - %Days(@@cldays);
                  DtBef180dys= %Dec(%Char(intCloseDt1:*iso0):8:0);
               EndIf;
               If intCloseDt < DtBef180dys;
                  meetSS = *Off;
               EndIf;
            EndIf;
         EndIf;
         Endif;
         If CMstat = 'C' and isSMiep = 'Y'
           and @@cSts = 'O' and  @@cldays = 0;
           meetSS = *off;
         endif;
         If CMstat = 'O' and isSMiep = 'Y'
           and @@cSts = 'C' and  @@cldays = 0;
           meetSS = *off;
         endif;
         Endif;
      /End-Free
      /free

        If xxVendor = 'OPTUM';
           meetSS = *on;

           // dont't include unless eligibility flag is checked
           If CMrxel <> 'Y';
              meetSS = *off;
           EndIf;

           // don't include if the claim has not been changed since the
           // last run date
           If CMdt < intRunDat;
              meetSS = *off;
           EndIf;

           // don't include controverted or denied claims
           If CMcvfl = 'Y';
              meetSS = *off;
           Endif;
           If CMdny = 'Y';
              meetSS = *off;
           Endif;

        EndIf;

      /end-free
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE WACEP4
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     CXFL01        ANDNE     'Y'
     C*
     C                   CLEAR                   PCE01
     C*
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   MOVEL(P)  CLM#          CECLM#
     C*
     C*** CALL FA700R TO RETRIEVE SUB#
     C                   MOVEL     CMCO#         XCO#              3
     C                   MOVEL     CMFND         XWCF#             3
     C                   CALL      'FA700R'
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    XCO#
     C                   PARM                    XWCF#
     C                   PARM      ' '           @@SUB#            5
     C                   PARM      ' '           @@FND             3
     C*
     C*
     C     @@MCIF        IFEQ      'Y'
     C*
     C*
     C*
     C                   MOVE      @@SUB#        XXSUB#
     C     DAKY          CHAIN     PDA01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     DAICD9        CEDIAG
     C                   ENDIF
     C*
     C*
     C                   endif
     C*
     C*
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL(P)  SRDTO         CECLDT
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       CECLDT
     C                   ENDIF
     C     XXCLT#        IFEQ      '114'
     C     CHOCST        ANDEQ     'O'
     C     CMRXEL        ANDEQ     'Y'
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYM1
     C                   MOVEL(P)  SRDTO         CECLDT
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEACDT
     C*
     C     CDKY          CHAIN     PCD01                              62
     C     *IN62         IFEQ      '0'
     C                   MOVE(P)   CDLSN         CELSN
     C                   MOVE(P)   CDFSN         CEFSN
     C                   MOVEL     CDADR1        CEADD
     C                   MOVEL     CDADR2        CEADR2
     C                   MOVEL     CDCTY         CECTY
     C                   MOVE      CDST          CEST
     C                   MOVE      CDZIP         CEZIP
     C                   MOVEL     CDAREA        CEPHN
     C                   MOVE      CDPHN#        CEPHN
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEDOB
     C*
     C                   MOVE      CDSEX         CESEX
     C                   ENDIF
     C                   MOVE      CMSS#         CESS#
     C                   MOVE      CMLTFL        CELTFL
     C                   MOVEL     CMACDS        CEINJ
     C                   MOVE      CMSTAT        CESTS
     C*
      /Free
        If isSMIEP = 'Y' and @@Rsts = 'Y';
           Setgt chky1 pCH01;
           Readpe chky1 pCH01;
           If not %eof(WMchl) and CHocst = 'O';
              Readpe chky1 pCH01;
              If not %eof(WMchl) and CHocst = 'C';
                 CEsts = 'R';
              Endif;
           Endif;
        Endif;
      /End-Free
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEETDT
     C*
     C*
     C     @@MCIF        IFEQ      'Y'
     C*
     C*
     C*
     C     PPKY          CHAIN     PPP01                              71
     C     *IN71         IFEQ      '0'
     C                   MOVE      PPFEIN        CEPDEA
     C                   ENDIF
     C*
     C*
     C                   endif
     C*
     C*
     C*
     C                   MOVE      *BLANKS       CERDE1
     C                   MOVE      *BLANKS       CERDE2
     C                   MOVE      *BLANKS       CERDE3
     C     EMKY1         CHAIN     PEM01                              67
     C     *IN67         IFEQ      '0'
     C                   MOVEL     EMNAME        CEEMPN
     C                   MOVEL     EMMAD1        CEADDR
     C                   MOVEL     EMMCTY        CEECTY
     C                   MOVE      EMMST         CEEST
     C                   MOVE      EMMZIP        CEEZIP
     C                   ENDIF
     C*
     C                   MOVE      CMCLEX        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CEADJ
     C                   MOVEL     UPPHNA        CEAPHN
     C                   MOVE      UPPHN#        CEAPHN
     C                   MOVEL     UPFAXA        CEAFAX
     C                   MOVE      UPFAX#        CEAFAX
     C                   MOVEL     UPMAIL        CEAEML
     C                   MOVE      UPEXT#        CEAEXT
     C                   ENDIF
     C*
     C                   MOVE      CMSUPR        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CECMGR
     C                   MOVEL     UPPHNA        CECPHN
     C                   MOVE      UPPHN#        CECPHN
     C                   MOVEL     UPFAXA        CECFAX
     C                   MOVE      UPFAX#        CECFAX
     C                   MOVEL     UPMAIL        CECEML
     C                   MOVE      UPEXT#        CECEXT
     C                   ENDIF
     C                   MOVE      'Y'           HIT
     C*
     C* Include closed claims where the SDF adjuster field on the claims
     C*file isn't empty & send the status in the export field as O-open.
     C*
     C     XXCLT#        IFEQ      '114'
     C     CMSDFA        ANDNE     *BLANK
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVEL     'O'           CESTS
     C                   ENDIF                                                  CHG STATUS
     C*
     C                   ENDIF                                                  CLT-114
     C                   MOVEL     CMJRST        CEJRST
     C     XXCLT#        IFNE      '114'
     C     CMCVFL        IFNE      *BLANKS
     C                   MOVEL     CMCVFL        CECVFL
     C                   ELSE
     C                   MOVEL     'N'           CECVFL
     C                   ENDIF
     C*
     C     CMDNY         IFNE      *BLANKS
     C                   MOVEL     CMDNY         CEDNY
     C                   ELSE
     C                   MOVEL     'N'           CEDNY
     C                   ENDIF
     C                   ENDIF
     C*
     C*
      /free

        If daicdind = 'Y';
          ceicd10 = daicd1001;
          cediag  = *blanks;
        Endif;

      /end-free
     C*
     C*
      /Free

        CEjCN = CMaCL# ;
        CEArea = CMpart;
        Chain (CMco#:CMfnd:CMfyr:CMemp#:CMdiv) wmrpl4;
        If %found (wmrpl4) and RPcap# <> *blanks;
           EvalR CEpol# = %Trim(RPcap#);
        Else;
           xxPol# = %EditC(CMfnd:'X') + %EditC(CMemp#:'X') +
              %EditC(CMfyr:'X');
           EvalR CEpol# = %Trim(xxPol#);
        Endif;

      /End-free
      /Free
        // Date of Death
        CEdoD = CMdoD;

        // Settlement Date
        Chain (CMco#:CMfnd:CMfYr:CMcase) pll01;
        If %Found(wmlll);
           CEstDt = LLstDt;
        Endif;

      /End-free
     C*
     C                   WRITE     PCE01
     C                   ENDIF
     C*
     C                   ENDSR                                                  WRITER
     C*****************************************************************
     C* WRINWC - WRITE WACEP4 FOR NON WCMS CLIENT
     C*****************************************************************
     CSR   WRINWC        BEGSR
     C*
     C                   CLEAR                   PCE01
     C*
     C                   Z-ADD     CMFND         FND
     C                   Z-ADD     CMFYR         FYR
     C                   Z-ADD     CMCASE        CASE
     C                   MOVEL(P)  CLM#          CECLM#
     C                   MOVEL(P)  CMSUB#        CESUB#
     C*
     C                   MOVEL     CMSUB#        XXSUB#
     C*** MOVE DIAGNOSIS FIELD FROM FADAP
     C*
     C     DAKY          CHAIN     PDA01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     DAICD9        CEDIAG
     C                   ENDIF
     C*
     C     CHKY3         SETGT     PCH02
     C     CHKY4         READPE    PCH02                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CECLDT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEACDT
     C*
     C     CDKY          CHAIN     PCD02                              62
     C     *IN62         IFEQ      '0'
     C                   MOVE(P)   CDLSN         CELSN
     C                   MOVE(P)   CDFSN         CEFSN
     C                   MOVEL     CDADR1        CEADD
     C                   MOVEL     CDADR2        CEADR2
     C                   MOVEL     CDCTY         CECTY
     C                   MOVE      CDST          CEST
     C                   MOVE      CDZIP         CEZIP
     C                   MOVEL     CDAREA        CEPHN
     C                   MOVE      CDPHN#        CEPHN
     C                   MOVE      CDDTOB        CEDOB
     C                   MOVE      CDSEX         CESEX
     C                   ENDIF
     C*
     C                   MOVE      CMSS#         CESS#
     C                   MOVE      CMLTFL        CELTFL
     C                   MOVEL     CMACDS        CEINJ
     C                   MOVE      CMSTAT        CESTS
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CEETDT
     C*
     C*
     C     @@MCIF        IFEQ      'Y'
     C*
     C*
     C*
     C     PPKY          CHAIN     PPP01                              71
     C     *IN71         IFEQ      '0'
     C                   MOVE      PPFEIN        CEPDEA
     C                   ENDIF
     C*
     C*
     C                   endif
     C*
     C*
     C*
     C                   MOVE      *BLANKS       CERDE1
     C                   MOVE      *BLANKS       CERDE2
     C                   MOVE      *BLANKS       CERDE3
     C*
     C     EMKY2         CHAIN     PEM02                              67
     C     *IN67         IFEQ      '0'
     C                   MOVEL     EMNAME        CEEMPN
     C                   MOVEL     EMMAD1        CEADDR
     C                   MOVEL     EMMCTY        CEECTY
     C                   MOVE      EMMST         CEEST
     C                   MOVE      EMMZIP        CEEZIP
     C                   ENDIF
     C*
     C                   MOVE      CMCLAD        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CEADJ
     C                   MOVEL     UPPHNA        CEAPHN
     C                   MOVE      UPPHN#        CEAPHN
     C                   MOVEL     UPFAXA        CEAFAX
     C                   MOVE      UPFAX#        CEAFAX
     C                   MOVEL     UPMAIL        CEAEML
     C                   MOVE      UPEXT#        CEAEXT
     C                   ENDIF
     C*
     C                   MOVE      CMCLSU        XXSTAF
     C     UPKY1         CHAIN     PUP02                              68
     C     *IN68         IFEQ      '0'
     C                   MOVEL     UPUSNM        CECMGR
     C                   MOVEL     UPPHNA        CECPHN
     C                   MOVE      UPPHN#        CECPHN
     C                   MOVEL     UPFAXA        CECFAX
     C                   MOVE      UPFAX#        CECFAX
     C                   MOVEL     UPMAIL        CECEML
     C                   MOVE      UPEXT#        CECEXT
     C                   ENDIF
     C*
     C                   MOVE      'Y'           HIT
      /Free
                         CEdoD = CMdeDt;
      /End-Free
     C*
     C                   WRITE     PCE01
     C*
     C                   ENDSR                                                  WRINWS
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
     O          E            ROUTE          1
     O                       ORDIS2              56
     O          E            ROUTE          1
     O                       ORDIS3              56
     O          E            ROUTE          1
     O                       ORDIS4              56
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE          2
     O                                           29 'COMPANY  . . . . . :'
     O                       XXCO#         Z   +001
     O*
     O          E            ROUTE          1 60
     O                                           25 '(C) COPYRIGHT PROGRESSI'
     O                                         +000 'VE DATA SOLUTIONS. INC.,'
     O                       *YEAR             +001
     O*
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              80
     O                       DSDATE        Y    132
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            HEAD1       1  1
     O                                           32 'Accident'
     O*
     O          E            HEAD1          2
     O                                            3 'Grp'
     O                                           17 'Claim number'
     O                                           30 'Date'
     O                                           39 'SS#'
     O*
     O          E            DETAIL         1
     O                       CMFND         4      3
     O                       CLM#              +002
     O                       XXACDT            +002 '  /  /    '
     O                       CMSS#             +002
     O*
     O          E            TOTAL       1  1
     O                                           20 'Number of records no'
     O                                           32 't extracted:'
     O                       COUNTE            +002 '       0 '
     O*
     O          E            TOTAL          1
     O                                           20 'Number of records ex'
     O                                           32 'tracted    :'
     O                       COUNTW            +002 '       0 '
     O*
