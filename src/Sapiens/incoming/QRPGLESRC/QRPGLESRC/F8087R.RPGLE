     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*                                                               *
     H*  F8087R - Upload HES Bills - Transfer to SR Files             *
     H*****************************************************************
     H*                                                               *
     H*  Called from:                                                 *
     H*  Calls to   :                                                 *
     H*                                                               *
     H*****************************************************************
     H*  Created on 11/03/2009 by C:227 for Project 135MMX            *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H* 04/07/10  C:227 Support:72263                                 *
     H*                 Change condition to populate payment value for*
     H*                 drugs for HES in WARPP                        *
     H*                                                               *
     H* 03/23/10  C:227 Support:72091                                 *
     H*                 Change condition for Possible duplicates and  *
     H*                 get Provider address from FMPDP               *
     H*                                                               *
     H* 02/17/10  C:227 PROJECT:135MMX                                *
     H*                 CHANGE CONDITION TO WRITE IN WARPP            *
     H*                                                               *
     H* 03/27/09  C:062 PROJECT:67519                                 *
     H*                 CORRECT RECON CLAIM ACCESS.                   *
     H*                                                               *
     H* 08/09/07  C:191 PROJECT:WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*                                                               *
     H*  06/12/07 C:188 135MJK                                        *
     H*                   Concentra process network check             *
     H*                   Additional changes to fee update calcs.     *
     H*                                                               *
     H*  03/30/07 C:188 135MJK                                        *
     H*                   CONCENTRA PROCESS NETWORK CHECK             *
     H*                                                               *
     H* 05/10/07 C:079 135MFX - Reactivate the EONWNM                *
     H*                                                               *
     H* 11/16/06 C:181 135MFX - (B135MFX) ** COMMENTED OUT **        *
     H*                   ONLY REACTIVATE THE EONWNM CODE
     H*                   DO NOT REACTIVATE THE EONAMT CODE
     H*                - Prevent Network Fees Being Issued For      *
     H*                  Concentra. Prevent Network Setup For       *
     H*                  Each Group By Removing Code In CHKNET      *
     H*                  SR That Does This, Thus Preventing W0081R  *
     H*                  From Issuing Check Requests.               *
     H*                                                               *
     H*  10/26/06 C:053 53674                                         *
     H*                 ACTIVATE DELETED CODE, ACCUMULATE N1TFEE      *
     H*                 RMVD 135MFX CODE
     H*                                                               *
     H*  08/03/06 C:062 135MHC                                        *
     H*                 SUPPRESS CHECK CREATION WHEN THE BILL HAS     *
     H*                 DENIED.                                       *
     H*                                                               *
     H*  07/03/06 C:062 135MHC                                        *
     H*                 SUPPRESS CHECK CREATION                       *
     H*                                                               *
     H*  03/31/06 C:163 135MFX                                        *
     H*                 - PRINT SUB NTWK ON EOB                       *
     H*                                                               *
     H*  10/13/05 C:79 SUPPORT 49388                                  *
     H*                - SET EAEPFL='E' WHEN BILL IS PENDED SO IT WONT*
     H*                  PRINT IN THE DAILY BATCH                     *
     H*                                                               *
     H*  10/07/05 C:79 SUPPORT 49315                                  *
     H*                - DO NOT WRITE A CHECK FOR POSSIBLE DUPS       *
     H*                - DO NOT WRITE A GIM CHECK FOR TOTAL DUPS      *
     H*                - CORRECT LOGIC FOR DENIED/CLOSED CHECK REQ.   *
     H*                                                               *
     H*  08/08/05 C:79 PROJECT 135MET                                 *
     H*                - DO NOT WRITE A CONCENTRA CHECK OR STORE AMTS *
     H*                - REMOVE CALL TO APPORTIONMENT PGM             *
     H*                - DONT USE N1IPBF TO MAKE PAYMENTS.  PAY IF    *
     H*                  CLAIM IS NOT DENIED                          *
     H*                - ADD CODE TO PROCESS CLAIM NUMBERS W/DASHES   *
     H*                - GO AHEAD AND LOAD THE POSSIBLE DUPLICATES    *
     H*                                                               *
     H*  12/11/03 C:62 PROJECT 129MEQ                                 *
     H*                - CHECK FOR DUPLICATE PAYMENTS                 *
     H*                                                               *
     H*  10/21/03 C:62 PROJECT 129MDB                                 *
     H*                - WRITE NOTES.                                 *
     H*                                                               *
     H*  09/14/03 C:62 PROJECT 129MBO                                 *
     H*                - NEED TO UPDATE/WRITE FDDHP/FDDUP             *
     H*                                                               *
     H*  08/25/03 C:62 PROJECT 129MBO 38734                           *
     H*                - IF CONCENTRA DOES NOT SEND A RECON REASON    *
     H*                  THEN DEFAULT A VALUE                         *
     H*                - FIX THE PAID AMOUNT                          *
     H*                - MOVE THE GETCLM SUBROUTINE                   *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FFAH1P     UF   E             DISK
     FFAH1L1    UF   E           K DISK
     F                                     RENAME(PH101:PH102)
     FFAH1L2    UF   E           K DISK
     F                                     RENAME(PH101:PH103)
     FFTHEL1    UF A E           K DISK
     FFAH9P     UF   E             DISK
     F*
     F* Header files
     F*
     FFAEOL4    UF A E           K DISK
     F                                     INFDS(DBFDS)
     F*******FAEOL74 IF  E           K        DISK
     F*******     PEO01                             KRENAMEPEO02
     F*******FAEOL68 IF  E           K        DISK
     F*******            PEO01                             KRENAMEPEO03
     FFAEAL1    IF A E           K DISK
     FFAEAL37   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFAESL2    IF A E           K DISK
     F*
     F* Detail files
     F*
     FFDCAP     O    E             DISK
     FFDDAL2    IF A E           K DISK
     FFDDPL     IF A E           K DISK
     FFAEPL     IF A E           K DISK
     F* Check Request file
     F*
     FWARPLN    IF A E           K DISK
     F*
     F* Miscellaneous files
     F*
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDCAL19   UF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFMC1L     UF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEGIN CHG *** 03/31/06 **************************************
     F*** FMF1L   UF  E           K        DISK
     FFMF1L     IF   E           K DISK
     F*** END   CHG *** 03/31/06 **************************************
     FFMGZL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FSTSTL     IF   E           K DISK
     FWARPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCIL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWTPSL     IF   E           K DISK
     F*
      /COPY SCOPYWW01D
     D BID             S              1    DIM(25)
     D BI              S              1    DIM(25)
     D DII             S              1    DIM(6)
     D DIO             S              1    DIM(6)
     D*********           CSC     1 112 12   PSC     3
     D********* NOT             S              1    DIM(280)                             NOTE
     D CLM             S              1    DIM(20)
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D REAS1           C                   CONST('HES -
     D                                     bill upload')
     D CODEDS          C                   CONST('MEDICINE DISPENSED -
     D                                     BY PHYSICIA')
     D ZEROS           C                   CONST('000000000')
     D ERRDS1          C                   CONST('Invalid Claim number')
     D ERRDS2          C                   CONST('SS# does not match')
     D ERRDS3          C                   CONST('Clmnt last name mis-
     D                                     match')
     D ERRDS4          C                   CONST('Claimant DOI mis-
     D                                     match')
     D ERRDS5          C                   CONST('Invaild carrier')
     D ERRDS6          C                   CONST('Submit Dt can-
     D                                     not be blank')
     D ERRDS7          C                   CONST('Proces Dt can-
     D                                     not be blank')
     D ERRDS8          C                   CONST('Fill Dt cannot -
     D                                     be blank')
     D ERRDS9          C                   CONST('Rx# cannot be -
     D                                     blank')
     D ERRD10          C                   CONST('NDC# must -
     D                                     not be blank')
     D ERRD11          C                   CONST('Quantity can-
     D                                     not be blank')
     D ERRD12          C                   CONST('Provider is not -
     D                                     valid')
     D ERRD13          C                   CONST('Invalid Carrier')
     D ERRD14          C                   CONST('Payment error')
     D ERRD15          C                   CONST('Duplicate Claim Ref#')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     D ALPHA           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D                 DS
     D  NWP                    1     50
     D                                     DIM(10)
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  CLAIM#                 1     20
     D  XXWCF#                 1      3
     D  XXFYR                  4      6
     D  XXCASE                 7     13
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3    INZ('000')
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11    INZ('000')
     D*
     D                 DS
     D  DATA                   1     75
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D                 DS
     D  NOTE                   1    140
     D  XXNOT1                 1     70
     D  XXNOT2                71    140

     IPCM02
     I              CMOSCL                      CWOSCL
     I*
     I***PY SCOPYWW01I

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@DCID           12
     C                   PARM                    @@RRN             9 0          FAN9P RRN
     C                   PARM                    @@RRN2            9 0          FAH1P RRN
     C                   PARM                    @@PRVC            1            PROV CHANGE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
     C*
     C     CAKY          KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    KKDSEQ
     C*
     C     CPKY          KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    H1EOB#
     C*
     C     CAPKY1        KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     EOBKY1        KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    H1EOB#
     C*
     C     FMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     CIKY          KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKPTYP
     C                   KFLD                    KKSYS
     C*
     C     CAKY2         KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C*
     C     CAKY3         KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    H1EOB#
     C                   KFLD                    KKLIN#
     C*
     C     DAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    EORCN
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C*
     C     CMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CMKY3         KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C*
     C     EAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSINV
     C*
     C     EAKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     EOBKY         KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    H1EOB#
     C*
     C     EOKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1SINV
     C*
     C     EOKY2         KLIST
     C                   KFLD                    KKIDOC
     C*
     C     ESKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     F1KY          KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    H1SUB#
     C                   KFLD                    H1FND
     C*
     C     GZKY          KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    KKZIPA
     C*
     C     PDKY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     PDKY2         KLIST
     C                   KFLD                    KKFEIN
     C*
     C     HEKY          KLIST
     C                   KFLD                    H1CR#
     C                   KFLD                    ERRN#
     C*
     C     RPKY1         KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     C*
     C     RPKY2         KLIST
     C                   KFLD                    H1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    H1FYR
     C                   KFLD                    H1CASE
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     H1KY1         KLIST
     C                   KFLD                    @@DCID
     C*
     C     H1KY2         KLIST
     C                   KFLD                    H1EOB#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    RPSEQ#        XXSEQ#
     C     *LIKE         DEFINE    PCKEY         KKKEY
     C     *LIKE         DEFINE    EOAST         XXAST
     C     *LIKE         DEFINE    GZZIPA        KKZIPA
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    RPEOBS        KKEOBS
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    EASINV        KKSINV
     C     *LIKE         DEFINE    EOSINV        K1SINV
     C     *LIKE         DEFINE    EOSINV        XXSINV
     C     *LIKE         DEFINE    EOIDOC        KKIDOC
     C     *LIKE         DEFINE    CIPTYP        KKPTYP
     C     *LIKE         DEFINE    CISYS         KKSYS
     C     *LIKE         DEFINE    EOCHRG        OHCHRG
     C     *LIKE         DEFINE    EOMRA         OHMRA
     C     *LIKE         DEFINE    EOOTHR        OHOTHR
     C     *LIKE         DEFINE    EONDIS        OHNDIS
     C     *LIKE         DEFINE    EOPAID        OHPAID
     C     *LIKE         DEFINE    EOINPE        OHINPE
     C     *LIKE         DEFINE    EOINPN        OHINPN
     C     *LIKE         DEFINE    EOOUTE        OHOUTE
     C     *LIKE         DEFINE    EOOUTN        OHOUTN
     C     *LIKE         DEFINE    EOASCE        OHASCE
     C     *LIKE         DEFINE    EOASCN        OHASCN
     C     *LIKE         DEFINE    EOCLNE        OHCLNE
     C     *LIKE         DEFINE    EOCLNN        OHCLNN
     C     *LIKE         DEFINE    EOLABE        OHLABE
     C     *LIKE         DEFINE    EOLABN        OHLABN
     C     *LIKE         DEFINE    EOGRPE        OHGRPE
     C     *LIKE         DEFINE    EOGRPN        OHGRPN
     C     *LIKE         DEFINE    EOINDE        OHINDE
     C     *LIKE         DEFINE    EOINDN        OHINDN
     C     *LIKE         DEFINE    CACHRG        ODCHRG
     C     *LIKE         DEFINE    CAMRA         ODMRA
     C     *LIKE         DEFINE    CAOTHR        ODOTHR
     C     *LIKE         DEFINE    CANDIS        ODNDIS
     C     *LIKE         DEFINE    CAPAID        ODPAID
     C     *LIKE         DEFINE    EOPAID        XXAMT
     C     *LIKE         DEFINE    EOFRDT        XXFRDT
     C     *LIKE         DEFINE    EOTODT        XXTODT
     C     *LIKE         DEFINE    EANAMT        HLDAMT
     C     *LIKE         DEFINE    EANAMT        KKPAID
     C     *LIKE         DEFINE    HEERDS        ERRDSC
     C     *LIKE         DEFINE    HESTAT        ERRSTS
     C     *LIKE         DEFINE    H1HLSN        KKH1LN
     C     *LIKE         DEFINE    CDLSN         KKCDLN
     C*
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* ERRCK - ERROR CHECK SUBROUTINE
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C                   SETOFF                                       90
     C*
     C     H1SBDT        IFEQ      *BLANKS
     C                   Z-ADD     6             ERRN#             2 0
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C     H1HPDT        IFEQ      *BLANKS
     C                   Z-ADD     7             ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C     H1FLDT        IFEQ      *BLANKS
     C                   Z-ADD     8             ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C     H1RX#         IFEQ      *BLANKS
     C                   Z-ADD     9             ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C     H1NDC         IFEQ      *BLANKS
     C                   Z-ADD     10            ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C     H1QTY         IFEQ      *BLANKS
     C                   Z-ADD     11            ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C     H1VNID        IFNE      'PPC'
     C     H1VNID        ANDNE     'EIA'
     C                   Z-ADD     13            ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C*
     C*Vendor Network ID
     C     H1VNID        IFEQ      'PPC'
     C     H1VNID        OREQ      'EIA'
     C                   EXSR      GETCLM
     C     FMKY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     'EIA  '       LOOKUP    NWP                                    85
     C     *IN85         IFEQ      *ON
     C*
     C     H1VNID        IFNE      'EIA'
     C                   Z-ADD     13            ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF                                                  IF VEND<>EIA
     C*
     C                   ELSE
     C*
     C     'PPC  '       LOOKUP    NWP                                    85
     C     *IN85         IFEQ      *ON
     C     H1VNID        IFNE      'PPC'
     C                   Z-ADD     13            ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF                                                  IF VEND<>PPC
     C                   ENDIF                                                  IF IN85 = ON
     C*
     C                   ENDIF                                                  IF IN85 = ON
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 10/25/05 **************************************
     C******************
     C*
B0A  C     *IN93         IFEQ      *OFF
     C*
     C* Check to see if original bill has already been uploaded
     C*
     C                   MOVEL     H1CR#         KKSINV
     C     EAKY1         CHAIN     PEA02                              99
B02  C     *IN99         IFEQ      *ON
     C*
     C* Check WCMC Claim Master to see if claim number exists
     C*
     C                   EXSR      GETCLM
     C     CMKY2         CHAIN     PCM01                              99
B03  C     *IN99         IFEQ      *ON
     C                   Z-ADD     1             ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL     H1HADT        XXACDT            8 0
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXHADT            7 0
     C     CMACDT        IFNE      XXHADT
     C                   Z-ADD     4             ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
E03  C                   ENDIF
     C*
     C* FEIN/MOD does not exist.
     C*
B05  C     H1FEIN        IFEQ      *BLANKS
     C     H1FEIN        OREQ      ZEROS
     C                   MOVE      H1FEIN        KKFEIN
S05  C                   ELSE
     C                   MOVE      H1FEIN        KKFEIN
E05  C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        KKMOD
     C*
     C     PDKY1         CHAIN     PPD01                              99
B06  C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     PDSTAT        ANDEQ     'I'
     C                   Z-ADD     12            ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
E06  C                   ENDIF
     C*
     C* Check Social security number
     C*
     C                   MOVEL(P)  H1HSS#        KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     2             ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ELSE
     C     LO:ALPHA      XLATE     H1HLSN        KKH1LN
     C     LO:ALPHA      XLATE     CDLSN         KKCDLN
     C     KKH1LN        IFNE      KKCDLN
     C                   Z-ADD     3             ERRN#
     C                   EXSR      ERRHES
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C*
     C* Check for Duplicate Payments
     C*
     C*** BEG CHG *** 03/23/10 ****************************************
     C*******              MOVELH1PAID    SIGN    1
     C*******              MOVE H1PAID    AMOUNT  92
     C                   MOVEL     H1DUE         SIGN              1
     C                   MOVE      H1DUE         AMOUNT            9 2
     C*** BEG CHG *** 03/23/10 ****************************************
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        XXAMT
     C                   ELSE
     C                   Z-ADD     AMOUNT        XXAMT
     C                   ENDIF
     C     XXAMT         IFNE      *ZEROS
     C*
     C                   MOVEL     H1FLDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVEL     H1FLDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXTODT
     C*
     C* Check for duplicate request/payment
     C*
     C                   CALL      'WA052R'
     C                   PARM      ##CO#         $#CO#             3 0          Company #
     C                   PARM      KKWCF#        $#FND             3 0          Fund
     C                   PARM      KKFYR         $#FYR             3 0          Fund Year
     C                   PARM      KKCASE        $#CASE            7 0          Case
     C                   PARM      XXFRDT        $#FROM            7 0          From Date
     C                   PARM      XXTODT        $#TO              7 0          To Date
     C                   PARM      XXAMT         $#AMT             9 2          Amount
     C                   PARM      'ME'          $#RECD            2            Reserve Code
     C                   PARM      PDFEIN        $#FEIN            9 0          FEIN #
     C                   PARM      'M'           $#RETP            1            Reserve Type
     C                   PARM      *BLANKS       $#RTCD            2
     C*
     C     $#RTCD        IFNE      *BLANKS
     C*** BEG ADD *** 03/23/10 ****************************************
     C     $#RTCD        IFEQ      'DR'
     C     $#RTCD        OREQ      'DP'
     C*** END ADD *** 03/23/10 ****************************************
     C                   SETON                                        9094
     C                   Z-ADD     14            ERRN#
     C                   ADD       1             XXDUP
     C                   EXSR      ERRHES
     C*** BEG ADD *** 03/23/10 ****************************************
     C                   ENDIF
     C*** END ADD *** 03/23/10 ****************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*
S02  C                   ELSE
     C                   SETON                                        909193
     C                   Z-ADD     15            ERRN#
     C                   EXSR      ERRHES
E02  C                   ENDIF
     C*
E0A  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRHES - WRITE TO ERROR LOG FILE FTHEP
     C*================================================================
     C     ERRHES        BEGSR
     C                   ADD       1             XXERCD
     C                   SELECT
     C     ERRN#         WHENEQ    1
     C                   MOVEL(P)  ERRDS1        ERRDSC
     C     ERRN#         WHENEQ    2
     C                   MOVEL(P)  ERRDS2        ERRDSC
     C     ERRN#         WHENEQ    3
     C                   MOVEL(P)  ERRDS3        ERRDSC
     C     ERRN#         WHENEQ    4
     C                   MOVEL(P)  ERRDS4        ERRDSC
     C     ERRN#         WHENEQ    5
     C                   MOVEL(P)  ERRDS5        ERRDSC
     C     ERRN#         WHENEQ    6
     C                   MOVEL(P)  ERRDS6        ERRDSC
     C     ERRN#         WHENEQ    7
     C                   MOVEL(P)  ERRDS7        ERRDSC
     C     ERRN#         WHENEQ    8
     C                   MOVEL(P)  ERRDS8        ERRDSC
     C     ERRN#         WHENEQ    9
     C                   MOVEL(P)  ERRDS9        ERRDSC
     C     ERRN#         WHENEQ    10
     C                   MOVEL(P)  ERRD10        ERRDSC
     C     ERRN#         WHENEQ    11
     C                   MOVEL(P)  ERRD11        ERRDSC
     C     ERRN#         WHENEQ    12
     C                   MOVEL(P)  ERRD12        ERRDSC
     C     ERRN#         WHENEQ    13
     C                   MOVEL(P)  ERRD13        ERRDSC
     C     ERRN#         WHENEQ    14
     C                   MOVEL(P)  ERRD14        ERRDSC
     C     ERRN#         WHENEQ    15
     C                   MOVEL(P)  ERRD15        ERRDSC
     C                   OTHER
     C                   ENDSL
     C     HEKY          CHAIN     PHE01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     ERRDSC        HEERDS
     C                   MOVEL     ERRSTS        HESTAT
     C                   MOVEL(P)  Q1USER        HEUSER
     C                   MOVEL(P)  Q1PGM         HEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HEDT
     C                   Z-ADD     DSTIME        HETIME
     C                   UPDATE    PHE01
     C                   ELSE
     C                   CLEAR                   PHE01
     C                   MOVEL(P)  H1CR#         HECR#
     C                   MOVEL(P)  ERRN#         HEERR
     C                   MOVEL     ERRDSC        HEERDS
     C                   MOVEL(P)  'A'           HESTAT
     C                   MOVEL(P)  Q1USER        HEUSER
     C                   MOVEL(P)  Q1USER        HEIUSR
     C                   MOVEL(P)  Q1PGM         HEPGM
     C                   MOVEL(P)  Q1PGM         HEIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HEDT
     C                   MOVEL     SRDTO         HEIDT
     C                   Z-ADD     DSTIME        HETIME
     C                   Z-ADD     DSTIME        HEITIM
     C                   WRITE     PHE01
     C                   ENDIF
     C*
     C                   SETOFF                                       9394
     C*
     C                   ENDSR
     C*================================================================
     C* GETAST - GET THE ADJUDICATION STATE
     C*================================================================
     C     GETAST        BEGSR
     C*
     C                   MOVEL(P)  'UC'          XXAST
     C                   MOVEL(P)  H1AST         KKST
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  H1AST         XXAST
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* GETCLM - GET THE CLAIM NUMBER
     C*================================================================
     C     GETCLM        BEGSR
     C*
     C                   MOVEL     H1CLM#        CLAIM#
     C                   MOVE      XXWCF#        KKWCF#
     C                   MOVE      XXFYR         KKFYR
     C                   MOVE      XXCASE        KKCASE
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      KKWCF#        #1WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1SUB#        KKSUB#
     C                   MOVE      #1FND         KKFND
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  99
     C                   SETOFF                                       16
     C                   Z-ADD     0             XXPRCD            9 0
     C                   Z-ADD     0             XXERCD            9 0
     C                   Z-ADD     0             XXDUP             9 0
     C*
     C     Q1PRMS        IFGT      2
     C*
     C     @@RRN2        CHAIN(N)  PH101                              98
     C  N98CMKY1         CHAIN     PCM01                              98
     C                   MOVEL     H1FEIN        KKFEIN
     C  N98PDKY2         CHAIN     PPD01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C*
     C     @@PRVC        IFEQ      'Y'
     C                   SETON                                        16
     C                   ENDIF
     C                   EXSR      WRITER
     C                   EXSR      UPDAT1
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     H1KY1         CHAIN     PH102                              98
     C     *IN98         IFEQ      *OFF
     C     *IN98         DOWEQ     *OFF
     C*
     C                   EXSR      ERRCK
     C                   EXSR      UPDT1A
     C     *IN90         IFEQ      *OFF
     C                   EXSR      WRITER
     C                   EXSR      UPDT1B
     C                   ENDIF
     C*
     C     H1KY1         READE     PH102                                  98
     C     *IN98         IFEQ      *ON
     C                   EXSR      UPDAT9
     C                   ENDIF
     C                   ENDDO
     C                   ELSE
     C                   EXSR      UPDAT9
     C                   ENDIF
     C                   ENDIF
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT1A - Update HES Bill Header Record (FAH1P) - 1ST UPDATE
     C*================================================================
     C     UPDT1A        BEGSR
     C*
     C                   SETOFF                                       92
     C     *IN90         IFEQ      *ON
     C********** *IN93     IFEQ *OFF
     C**********           MOVEL'R'       H1PRFL    P
     C**********           ELSE
     C                   MOVEL(P)  'E'           H1PRFL
     C**********           ENDIF
     C                   MOVE      *BLANKS       H1DLUS
     C                   MOVE      *BLANKS       H1DLPG
     C                   Z-ADD     0             H1DLTM
     C                   Z-ADD     0             H1DLDT
     C                   MOVE      *BLANKS       H1DLRE
     C                   MOVE      *BLANKS       H1DLUS
     C                   Z-ADD     0             H1PRDT
     C                   Z-ADD     0             H1PRTM
     C                   Z-ADD     0             H1ULDT
     C                   Z-ADD     0             H1RCDT
     C                   Z-ADD     0             H1RRDT
     C*
     C                   ELSE
     C*
     C     H1EOB#        IFEQ      *ZEROS
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1EOB#
     C                   UPDATE    PC101
     C                   ENDIF
     C                   Z-ADD     C1EOB#        H1EOB#
     C                   ENDIF
     C*
     C                   Z-ADD     CMCO#         H1CO#
     C                   Z-ADD     CMSUB#        H1SUB#
     C                   Z-ADD     CMFND         H1FND
     C                   Z-ADD     CMFYR         H1FYR
     C                   Z-ADD     CMCASE        H1CASE
     C                   MOVEL     PDFEIN        H1FEIN
     C                   MOVEL(P)  CMSS#         H1SS#
     C                   Z-ADD     CMACDT        H1ACDT
     C*
     C                   ENDIF
     C*
     C* Initialize the fields
     C*
     C                   UPDATE    PH102
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT1B - Update HES Bill Header Record (FAH1P) - 2ND UPDATE
     C*================================================================
     C     UPDT1B        BEGSR
     C*
     C     H1KY2         CHAIN     PH103                              99
     C     *IN99         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1PRDT
     C*
     C                   MOVEL     *BLANKS       H1DLUS
     C                   MOVEL     *BLANKS       H1DLPG
     C                   Z-ADD     *ZEROS        H1DLTM
     C                   Z-ADD     *ZEROS        H1DLDT
     C                   MOVEL     *BLANKS       H1DLRE
     C                   MOVEL(P)  'Y'           H1PRFL
     C*
     C                   UPDATE    PH103
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT1 - Update HES Bill Header Record (FAH1P) - SINGLE MODE
     C*================================================================
     C     UPDAT1        BEGSR
     C*
     C     @@RRN2        CHAIN     PH101                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  'Y'           H1PRFL
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1PRDT
     C*
     C                   UPDATE    PH101
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT9 - Update CONCENTRA Bill Transmission File (FAH9P).
     C*================================================================
     C     UPDAT9        BEGSR
     C*
     C     H1KY1         CHAIN(N)  PH102                              99
     C     *IN99         IFEQ      *ON
     C     @@RRN         CHAIN     PH901                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  Q1USER        H9USER
     C                   MOVEL(P)  Q1PGM         H9PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H9PRDT
     C                   MOVEL     SRDTO         H9DT
     C                   Z-ADD     DSTIME        H9PRTM
     C*
     C                   ADD       XXPRCD        H9TRCT
     C                   Z-ADD     XXERCD        H9ERR
     C                   Z-ADD     XXDUP         H9DUP
     C*
     C                   UPDATE    PH901
     C                   ENDIF
     C*
     C* Copy document to CONCENTRA folder
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   CALL      'F8087C'
     C                   PARM                    #1CO#
     C                   PARM      @@DCID        ##DCID           12
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDCAP - Update FDCAP record
     C*================================================================
     C     UPDCAP        BEGSR
     C*
     C********             MOVE H2LIN#    KKLIN#
     C*
     C     CAKY3         CHAIN     PCA03                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  H1NDC         CACODE
     C*
     C                   MOVEL(P)  EOUSER        CAUSER
     C                   MOVEL(P)  EOPGM         CAPGM
     C                   Z-ADD     EODT          CADT
     C                   Z-ADD     EOTIME        CATIME
     C*
     C                   MOVE      H1BILA        CACHRG
     C                   MOVE      H1PRIC        CAMRAA
     C*
     C*
     C     CAOTHR        IFEQ      *ZEROS
     C                   MOVEL     *BLANKS       CAOT1C
     C                   ELSE
     C                   MOVEL     'CON'         CAOT1C
     C                   ENDIF
     C*
     C                   MOVE      H1DUE         CAPAID
     C                   Z-ADD     EOMRA         CAMRA
     C*
     C                   EXSR      WRTDAP                                       FDDAP
     C*
     C                   UPDATE    PCA03
     C*
     C                   ELSE
     C                   EXSR      WRTCAP
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   ADD       1             XXPRCD
     C*
     C* Get the adjudication state
     C*
     C                   EXSR      GETAST
     C*
     C* Write the header records
     C*
     C                   EXSR      WRTEOP                                       FAEOP
     C                   EXSR      WRTEAP                                       FAEAP
     C                   EXSR      WRTESP                                       FAESP
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      WRTEPP                                       FAEPP
     C                   ENDSL
     C*
     C* Write the detail records
     C*
     C                   EXSR      WRTDTL
     C     EAPLNK        IFEQ      '2'
     C     EAPLNK        OREQ      '3'
     C*
     C* Write the check request unless the claim is denied
     C*
     C********   CMDNY     IFNE 'Y'
     C********   CMSTAT    ANDNE'C'
     C     *IN94         IFNE      *ON
     C*
     C*********  H1DUP     IFNE 'Y'
     C*** BEG CHG *** 02/17/10 ****************************************
     C*********            MOVE H1PAID    KKPAID
     C                   Z-ADD     0             KKPAID
     C                   MOVE      H1DUE         KKPAID
     C*** END CHG *** 02/17/10 ****************************************
     C     H1TTYP        IFEQ      'P'
     C     KKPAID        ANDGT     0
     C                   EXSR      WRTRP1                                       WARPP
     C                   ENDIF
     C*********            ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEAP - Write FAEAP record
     C*================================================================
     C     WRTEAP        BEGSR
     C*
     C     EAKY2         CHAIN     PEA01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEA01
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C                   Z-ADD     EOTRDT        EATRDT
     C*
     C                   Z-ADD     EOCHRG        EACHRG
     C                   Z-ADD     EOMRA         EAMRA
     C                   Z-ADD     EOOTHR        EAOTHR
     C                   Z-ADD     EONDIS        EANDIS
     C                   Z-ADD     EOPAID        EAPAID
     C                   MOVEL     EOSYS         EASYS
     C*
     C                   MOVEL(P)  'A'           EASTAT
     C                   MOVEL(P)  EOUSER        EAUSER
     C                   MOVEL(P)  EOPGM         EAPGM
     C                   Z-ADD     EOTIME        EATIME
     C                   Z-ADD     EODT          EADT
     C                   MOVEL(P)  EOUSER        EAIUSR
     C                   MOVEL(P)  EOPGM         EAIPGM
     C                   Z-ADD     EOITIM        EAITIM
     C                   Z-ADD     EOIDT         EAIDT
     C*
     C********   EOBLST    IFEQ 'P'
     C********             MOVEL'E'       EAEPFL
     C********             ENDIF
     C     EOBLST        IFNE      'P'
     C     F1KY          CHAIN(N)  PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C                   ELSE
     C     EOPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF                                                  EOPAID LE 0
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C                   MOVEL(P)  H1CR#         EASINV
     C*
     C*** BEG DEL *** 03/23/10 ****************************************
     C*******    *IN16     IFEQ *ON
     C*** END DEL *** 03/23/10 ****************************************
     C     PDDBA1        IFNE      'Y'
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVE      PDNAME        EAPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EAPNAM
     C                   ENDIF
     C                   MOVE      PDADR1        EAADR1
     C                   MOVE      PDADR2        EAADR2
     C                   ELSE
     C                   MOVE      PDADR1        EAPNAM
     C                   MOVE      PDADR2        EAADR1
     C                   MOVE      *BLANKS       EAADR2
     C                   ENDIF
     C                   MOVE      PDCTY         EACTY
     C                   MOVE      PDST          EAST
     C                   MOVE      PDZIP         EAZIP
     C*** BEG DEL *** 03/23/10 ****************************************
     C*******              ELSE
     C*******              MOVELH1PNAM    EAPNAM    P
     C*******              MOVELH1ADR1    EAADR1    P
     C*******              MOVELH1CTY     EACTY     P
     C*******              MOVELH1ST      EAST      P
     C*******              MOVE H1ZIP     EAZIP
     C*******              ENDIF
     C*** END DEL *** 03/23/10 ****************************************
     C*
     C                   MOVEL(P)  *BLANK        EAPNWC
     C*
     C* Total duplicates do not allow BRS checks
     C*
     C                   MOVE      H1PAID        AMOUNT
     C     AMOUNT        IFEQ      *ZEROS
     C                   MOVEL(P)  'N'           EAPBRC
     C                   MOVEL(P)  '4'           EAPNWC
     C                   ELSE
     C                   MOVEL(P)  H1TTYP        EAPNWC
     C                   ENDIF
     C                   MOVEL     'N'           EAPBRC
     C*
     C                   WRITE     PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEOP - Write FAEOP record
     C*================================================================
     C     WRTEOP        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C* WRITE FIRST EOMB
     C*
     C     EOBKY         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEO01
     C                   MOVEL     H1CR#         EOSINV
     C                   Z-ADD     H1CO#         EOCO#
     C                   Z-ADD     H1FND         EOFND
     C                   Z-ADD     H1FYR         EOFYR
     C                   Z-ADD     H1CASE        EOCASE
     C*
     C                   MOVEL     H1HPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C                   MOVEL     H1HADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOACDT
     C*
     C                   MOVEL     H1FEIN        EOFEIN
     C                   MOVEL     '000'         EOMOD
     C*
     C                   MOVEL     H1FLDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOFRDT
     C*
     C                   MOVEL     H1FLDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOTODT
     C*
     C*** BEG DEL *** 03/23/10 ****************************************
     C*******    *IN16     IFEQ *ON
     C*** END DEL *** 03/23/10 ****************************************
     C     PDDBA1        IFNE      'Y'
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVE      PDNAME        EOPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EOPNAM
     C                   ENDIF
     C                   MOVE      PDADR1        EOADR1
     C                   MOVE      PDADR2        EOADR2
     C                   ELSE
     C                   MOVE      PDADR1        EOPNAM
     C                   MOVE      PDADR2        EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C                   ENDIF
     C                   MOVE      PDCTY         EOCTY
     C                   MOVE      PDST          EOST
     C                   MOVE      PDZIP         EOZIP
     C*** BEG DEL *** 03/23/10 ****************************************
     C*******              ELSE
     C*******              MOVELH1PNAM    EOPNAM    P
     C*******              MOVELH1ADR1    EOADR1    P
     C*******              MOVELH1CTY     EOCTY     P
     C*******              MOVELH1ST      EOST      P
     C*******              MOVE H1ZIP     EOZIP
     C*******              ENDIF
     C*** END DEL *** 03/23/10 ****************************************
     C*
     C                   MOVEL(P)  XXAST         EOAST
     C                   Z-ADD     H1EOB#        EOCHK#
     C                   Z-ADD     H1EOB#        EOEOB#
     C*
     C                   MOVEL(P)  'P'           EOSYS
     C*
     C                   MOVEL(P)  'HES'         EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL(P)  'HES'         EOIUSR
     C                   MOVEL     SRDTO         EOIDT
     C                   MOVEL(P)  'N'           EOTAPE
     C                   MOVEL     EOZIP         XXZIP             5
     C                   MOVEL     XXZIP         EOFLIC
     C                   Z-ADD     1             EO#BLL
     C                   MOVEL     XXAST         EOAST
     C                   Z-ADD     H1SUB#        EOSUB#
     C*
     C                   MOVEL(P)  XXZIP         KKZIPA
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF
     C*
     C                   MOVEL(P)  H1SS#         EOSS#
     C*
     C                   MOVEL(P)  Q1PGM         EOIPGM
     C                   Z-ADD     DSTIME        EOITIM
     C*
     C                   MOVEL(P)  'X'           EOBTYP
     C*
     C                   MOVEL(P)  'C'           EOBLST
     C*
     C* If the claim was denied or closed pend and set the upload flag
     C* to C
     C*
     C********   CMDNY     IFEQ 'Y'
     C********   CMSTAT    OREQ 'C'
     C********             MOVEL'P'       EOBLST
     C********             END
     C*
     C* If the bill is a possible duplicate payment pend the bill and
     C* set the upload flag = D
     C*
     C     *IN94         IFEQ      *ON
     C     H1PRFL        OREQ      'P'
     C                   MOVEL     'P'           EOBLST
     C                   ENDIF
     C*** END ADD *** 08/08/05 ****************************************
     C                   MOVEL     '2'           EOCSCD
     C*
     C                   MOVEL     H1INDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOCSDT
     C*
     C                   MOVEL     H1SBDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOCRDT
     C*
     C                   MOVEL     H1DUE         SIGN              1
     C                   MOVE      H1DUE         AMOUNT            9 2
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EOPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        EOPAID
     C                   ENDIF
     C*
     C                   MOVE      H1BILA        EOCHRG
     C     EOCHRG        SUB       EOPAID        EOMRA
     C*
     C                   MOVEL(P)  H1VNID        EONTWK
     C*
     C*
     C                   MOVEL     EOEOB1        EOSPCD
     C*
     C                   MOVEL(P)  H1SS#         KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CDLSN         EOLSN
     C                   MOVEL(P)  CDFSN         EOFSN
     C                   MOVEL(P)  CDMID         EOMID
     C                   ELSE
     C                   MOVEL     *BLANKS       EOLSN
     C                   MOVEL     *BLANKS       EOFSN
     C                   MOVEL     *BLANKS       EOMID
     C                   ENDIF
     C*
     C*
     C                   WRITE     PEO01
     C     EOBKY         CHAIN(N)  PEO01                              99
     C                   ELSE
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTESP - Write FAESP record
     C*================================================================
     C     WRTESP        BEGSR
     C*
     C     ESKY          CHAIN     PES01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PES01
     C                   Z-ADD     EOCO#         ESCO#
     C                   Z-ADD     EOSUB#        ESSUB#
     C                   Z-ADD     EOFND         ESFND
     C                   Z-ADD     EOFYR         ESFYR
     C                   Z-ADD     EOCASE        ESCASE
     C                   Z-ADD     EOEOB#        ESEOB#
     C                   Z-ADD     EORCN         ESRCN
     C                   MOVEL     EOSS#         ESSS#
     C                   Z-ADD     EOACDT        ESACDT
     C*
     C* Change logic in case bill is being pended
     C*
     C                   MOVEL(P)  EOBLST        ESBLST
     C     EOBLST        IFEQ      'P'
     C                   MOVEL(P)  'DNY'         ESSTCD
     C                   ENDIF
     C                   MOVEL(P)  REAS1         ESREAS
     C*
     C                   MOVEL     'A'           ESSTAT
     C                   MOVEL(P)  EOUSER        ESUSER
     C                   MOVEL(P)  EOPGM         ESPGM
     C                   Z-ADD     EOTIME        ESTIME
     C                   Z-ADD     EODT          ESDT
     C                   MOVEL(P)  EOUSER        ESIUSR
     C                   MOVEL(P)  EOPGM         ESIPGM
     C                   Z-ADD     EOIDT         ESIDT
     C                   Z-ADD     EOTIME        ESITIM
     C*
     C                   WRITE     PES01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTCAP - Write FDCAP record
     C*================================================================
     C     WRTCAP        BEGSR
     C*
     C********** H1EOB#    IFEQ *ZEROS
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C     CAKY          SETGT     PCA02
     C     CAKY2         READPE    PCA02                                  99
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   CLEAR                   PCA01
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C     CMSTAT        IFEQ      'C'
     C                   MOVEL     'P'           CAFLAG
     C                   ENDIF
     C*
     C                   MOVEL     H1FLDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         CAFDAT
     C                   MOVEL     SRDTO         CATDAT
     C*
     C                   Z-ADD     EOTRDT        CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C*
     C                   MOVEL     '1'           CAPRTY
     C*
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   Z-ADD     1             CALIN#
     C                   Z-ADD     EOCHK#        CACHK#
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EOMRA         CAMRA
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C*
     C                   MOVEL(P)  'X'           CARCFL
     C*
     C                   MOVEL(P)  EOUSER        CAUSER
     C                   MOVEL(P)  EOPGM         CAPGM
     C                   Z-ADD     EODT          CADT
     C                   Z-ADD     EOTIME        CATIME
     C                   MOVEL(P)  EOUSER        CAIUSR
     C                   MOVEL(P)  EOPGM         CAIPGM
     C                   Z-ADD     EOIDT         CAIDT
     C                   Z-ADD     EOITIM        CAITIM
     C*
     C                   Z-ADD     EOSUB#        CASUB#
     C                   MOVE      EOZIPA        CAZIPA
     C                   MOVEL(P)  EOAST         CAAST
     C*
     C                   MOVE      H1BILA        CACHRG
     C                   MOVE      H1PRIC        CAMRAA
     C                   MOVE      H1DUE         CAPAID
     C*
     C     CAOTHR        IFEQ      *ZEROS
     C                   MOVEL     *BLANKS       CAOT1C
     C                   ELSE
     C                   MOVEL     'CON'         CAOT1C
     C                   ENDIF
     C*
     C                   Z-ADD     EORCN         CARCN
     C*
     C                   MOVEL(P)  H1NDC         CACODE
     C                   MOVEL(P)  H1NDC         CACPT4
     C                   MOVEL(P)  H1NDC         CABPRC
     C*
     C                   EXSR      WRTDAP                                       FDDAP
     C*
     C                   WRITE     PCA01
     C**********           ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDAP - Write FDDAP record
     C*================================================================
     C     WRTDAP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDA01
     C                   Z-ADD     CACO#         DACO#
     C                   Z-ADD     CASUB#        DASUB#
     C                   Z-ADD     CAFND         DAFND
     C                   Z-ADD     CAFYR         DAFYR
     C                   Z-ADD     CACASE        DACASE
     C                   MOVEL     CASS#         DASS#
     C                   Z-ADD     CAACDT        DAACDT
     C                   Z-ADD     CAEOB#        DAEOB#
     C                   Z-ADD     EORCN         DARCN#
     C                   Z-ADD     CADSEQ        DADSEQ
     C                   Z-ADD     CARDAT        DATRDT
     C*
     C                   MOVE      H1BILA        DACHRG
     C                   Z-ADD     CAMRA         DAMRA
     C     CAOTHR        SUB       ODOTHR        DAOTHR
     C     CANDIS        SUB       ODNDIS        DANDIS
     C                   MOVE      H1DUE         DAPAID
     C*
     C                   MOVEL     'A'           DASTAT
     C                   MOVEL(P)  CAUSER        DAUSER
     C                   MOVEL(P)  CAPGM         DAPGM
     C                   Z-ADD     CATIME        DATIME
     C                   Z-ADD     CADT          DADT
     C                   MOVEL(P)  CAUSER        DAIUSR
     C                   MOVEL(P)  CAPGM         DAIPGM
     C                   Z-ADD     CAITIM        DAITIM
     C                   Z-ADD     CAIDT         DAIDT
     C*
     C                   WRITE     PDA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDTL - Write Detail Records
     C*================================================================
     C     WRTDTL        BEGSR
     C*
     C                   EXSR      WRTCAP                                       FDCAP
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      WRTDPP                                       FADHP
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* WRTRP1 - Write WARPP record  - Provider Check
     C*================================================================
     C     WRTRP1        BEGSR
     C*
     C     EAKY2         CHAIN(N)  PEA01                              99
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKEOBS
     C     RPKY1         CHAIN     PRP01                              99
     C     *IN99         IFEQ      *ON
     C*
     C     RPKY2         SETGT     PRP02
     C     RPKY2         READPE    PRP02                                  99
     C     *IN99         IFEQ      *OFF
     C     RPSEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PRP01
     C                   Z-ADD     EOCO#         RPCO#
     C                   Z-ADD     KKWCF#        RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   Z-ADD     XXSEQ#        RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   Z-ADD     EOFEIN        RPFEIN
     C                   Z-ADD     EOMOD         RPMOD
     C                   Z-ADD     EAPAID        RPAMT
     C                   MOVEL     'G'           RPPGTP
     C*
     C     CMKY3         CHAIN     PCM02                              99
     C                   MOVEL     'P'           KKPTYP
     C                   MOVEL     EOSYS         KKSYS
     C     CIKY          CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
     C                   ELSE
     C*** BEG ADD *** 04/07/10 ****************************************
     C     EOBTYP        IFEQ      'X'
     C                   MOVE(P)   CIXBR2        RPBR2
     C                   ELSE
     C*** END ADD *** 04/07/10 ****************************************
     C                   MOVE(P)   CIBR2         RPBR2
     C                   ENDIF
     C*** BEG ADD *** 04/07/10 ****************************************
     C                   ENDIF
     C*** BEG ADD *** 04/07/10 ****************************************
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE(P)   CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE(P)   CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVEL(P)  CISTS         RPSTS
     C*
     C                   ELSE
     C*
     C     EOSYS         IFEQ      'P'
     C*** BEG ADD *** 04/07/10 ****************************************
     C     EOBTYP        IFEQ      'X'
     C                   MOVE(P)   'DG'          RPBR2
     C                   ELSE
     C*** END ADD *** 04/07/10 ****************************************
     C                   MOVEL(P)  'MD'          RPBR2
     C*** BEG ADD *** 04/07/10 ****************************************
     C                   ENDIF
     C*** BEG ADD *** 04/07/10 ****************************************
     C                   ELSE
     C                   MOVEL(P)  'HP'          RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'LT'          RPBR7
     C                   ELSE
     C                   MOVEL(P)  'NL'          RPBR7
     C                   ENDIF
     C*
     C                   MOVEL(P)  'PD'          RPSTS
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   MOVEL(P)  'MP'          RPTRCD
     C                   MOVEL(P)  'P'           RPTYPE
     C                   MOVEL(P)  'M'           RPRETP
     C                   MOVEL(P)  'ME'          RPRECD
     C*
     C                   MOVEL(P)  RPTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PCDESC        RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  EOPNAM        RPPNAM
     C                   MOVEL(P)  EOADR1        RPADR1
     C                   MOVEL(P)  EOADR2        RPADR2
     C                   MOVEL(P)  EOCTY         RPCTY
     C                   MOVEL(P)  EOST          RPST
     C                   Z-ADD     EOZIP         RPZIP
     C                   MOVEL     'P'           RPSTAT
     C                   MOVEL(P)  EOUSER        RPUSER
     C                   MOVEL(P)  EOPGM         RPPGM
     C                   Z-ADD     EOTIME        RPTIME
     C                   Z-ADD     EODT          RPDT
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   Z-ADD     EORCN         RPEOBS
     C                   Z-ADD     EODT          RPRCVD
     C                   MOVEL(P)  EOSINV        RPSINV
     C                   Z-ADD     EOCSDT        RPCSDT
     C*
     C                   MOVEL(P)  RPSTS         KKKEY
     C     TBKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   PSSRT         RPSORT
     C                   ENDIF
     C*
     C                   MOVEL(P)  'N'           RPSENT
     C                   MOVEL(P)  'HES'         RPRQST
     C*
     C*  Call API to determine the Bank "Code".
     C*
     C                   MOVE      RPCO#         $$CO#
     C                   MOVE      RPFND         $$FND
     C                   MOVE      RPFYR         $$FYR
     C                   MOVE      RPCASE        $$CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C*
     C                   CALL      'WA023R1'
     C                   PARM      '2'           $$MODE            1            Mode
     C                   PARM                    $$CO#             3            Company
     C                   PARM                    $$FND             3            Group #
     C                   PARM                    $$FYR             3            Year
     C                   PARM                    $$CASE            7            Case
     C                   PARM      'WC'          $$APPL            2            Applic Code
     C                   PARM      DATA          $$DATA           75            Selection criteria
     C                   PARM      *BLANKS       $$CKTP           10            Bank Acct Code
     C                   PARM      *BLANKS       $$RTCD            1            Return Code
     C*
     C                   MOVEL(P)  $$CKTP        RPCKTP                         Bank Acct# Code
     C*
     C                   MOVEL(P)  EAIUSR        RPIUSR
     C                   MOVEL(P)  EAPGM         RPIPGM
     C                   Z-ADD     EAITIM        RPITM
     C                   Z-ADD     EAIDT         RPIDT
     C*
     C     RPAMT         IFGT      *ZEROS
     C                   WRITE     PRP01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDPP - Write FDDPP record
     C*================================================================
     C     WRTDPP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDP01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDP01
     C                   Z-ADD     CACO#         DPCO#
     C                   Z-ADD     CASUB#        DPSUB#
     C                   Z-ADD     CAFND         DPFND
     C                   Z-ADD     CAFYR         DPFYR
     C                   Z-ADD     CACASE        DPCASE
     C                   MOVEL     CASS#         DPSS#
     C                   Z-ADD     CAACDT        DPACDT
     C                   Z-ADD     CAEOB#        DPEOB#
     C                   Z-ADD     CADSEQ        DPDSEQ
     C                   MOVEL(P)  H1NDC         DPNDC
     C                   MOVEL(P)  H1RX#         DPRX#
     C                   MOVEL(P)  H1NWRF        DPNWRF
     C                   MOVEL(P)  H1NDC         DPBNDC
     C                   MOVEL(P)  H1NDCD        DPDESC
     C*
     C                   MOVE      H1FLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         DPRXFD
     C*
     C     H1QTY         IFEQ      *ZEROS
     C                   Z-ADD     1             DPUNTS
     C                   ELSE
     C                   MOVE      H1QTY         DPUNTS
     C                   ENDIF
     C*
     C                   MOVEL     'A'           DPSTAT
     C                   MOVEL(P)  CAUSER        DPUSER
     C                   MOVEL(P)  CAPGM         DPPGM
     C                   Z-ADD     CATIME        DPTIME
     C                   Z-ADD     CADT          DPDT
     C                   MOVEL(P)  CAUSER        DPIUSR
     C                   MOVEL(P)  CAPGM         DPIPGM
     C                   Z-ADD     CAITIM        DPITIM
     C                   Z-ADD     CAIDT         DPIDT
     C*
     C                   WRITE     PDP01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEPP - Write FAEPP record
     C*================================================================
     C     WRTEPP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EOBKY1        CHAIN     PEP01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEP01
     C                   Z-ADD     EOCO#         EPCO#
     C                   Z-ADD     EOSUB#        EPSUB#
     C                   Z-ADD     EOFND         EPFND
     C                   Z-ADD     EOFYR         EPFYR
     C                   Z-ADD     EOCASE        EPCASE
     C                   Z-ADD     EOEOB#        EPEOB#
     C                   MOVEL(P)  EOSS#         EPSS#
     C                   Z-ADD     EOACDT        EPACDT
     C                   MOVEL     'N'           EPEDI
     C                   MOVEL(P)  H1NPI#        EPRFNP
     C                   MOVEL(P)  H1LSN         EPPNAM
     C                   MOVEL(P)  H1DEA#        EPPDEA
     C*
     C                   MOVEL(P)  EOUSER        EPUSER
     C                   MOVEL(P)  EOPGM         EPPGM
     C                   Z-ADD     EOTIME        EPTIME
     C                   Z-ADD     EODT          EPDT
     C                   MOVEL(P)  EOUSER        EPIUSR
     C                   MOVEL(P)  EOPGM         EPIPGM
     C                   Z-ADD     EOITIM        EPITIM
     C                   Z-ADD     EOIDT         EPIDT
     C*
     C                   WRITE     PEP01
     C                   ENDIF
     C*
     C                   ENDSR
