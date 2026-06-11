     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE U0060R01 - PRE-CERTIFICATION MAINTENANCE
     H* ***************************************************************
     H*
     H* DESCRIPTION :
     H*    TO PRE-CERTIFY CHARGES, DATES, SERVICES AND LENGTH         *
     H*    OF TIME BEFORE BILL IS TO BE ADJUDICATED.                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    UTILIZATION/REVIEW MENU                                    *
     H*    WORK WITH MEDICAL CASES - OPT 15                           *
     H*    U0511R (BILL ENTRY PRE-CERTIFICATION SEARCH)               *
     H*                                                               *
     H*  CREATED:  1/02/98   47    87MCB                              *
     H* ********************************************************
     H*
     H* REVISIONS:
     H*
     H*  06/09/15 C:079 Phaseware: 144/35239
     H*                 Load diag code description correctly
     H*                 Load screens fields from ICD10 fields
     H*
     H*  05/27/14 C:296 SUPPORT: 85010
     H*                 Change calling indicator from factor1 to
     H*                 factor 2.
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase 2
     H*                 Add Search capabilty for Diagnosis code
     H*                 Apply validation for ICD10 field
     H*
     H*  05/23/13 C:277 PROJECT: MC329
     H*                 Activate ICD10 fields if ICD10 is active
     H*
     H*  10/13/10 C:062 PROJECT: SY190
     H*                 Correct Provider Edits.
     H*
     H*  11/02/09 C:232 PROJECT: 139WMR
     H*                 OFAC Reporting-Add Pending Status Indicator.
     H*
     H*  02/20/07 C:188 SUPPORT: 57091
     H*                 PREVENT INACTIVE INTERVIEWER SELECTION
     H*
     H*  09/26/00 C:62  PROJECT: MC010 24475 BASE ONLY
     H*                 -CHANGE INDICATOR USAGE FOR POSITIONING.
     H*                 -CHANGE ORDER OF ERROR CHECKING.
     H*
     H*  09/06/00 C:62  PROJECT: MC101 24239
     H*                 INITIALIZE XXAST AND SETON *INLR AND SETON
     H*                 PROVIDER ERROR MESSAGE IF SEARCHING POS AND
     H*                 NO PROVIDER HAS BEEN ENTERED.
     H*
     H*  08/15/00 C:62  PROJECT: MC215
     H*                 REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H*  03/27/00 C:62  URMNT
     H*                 GIVE ERROR MESSAGE FOR LOS INFORMATION.
     H*
     H*  03/17/00 C:62  UR025
     H*                 DON'T DISPLAY SAVINGS INFORMATION IF RE-CERT
     H*
     H*  03/06/00 C:62  UR025
     H*                 GET RID OF FBST01 AND USE CMAST OR P1ST.
     H*
     H*  03/06/00 C:62  URMNT
     H*                 AUTO DIARY CHANGES TO REVIEW DATE
     H*
     H*  03/03/00 C:62  URMNT
     H*                 CHANGE THE CALC FOR CALCULATED DAYS.
     H*
     H*  02/25/00 C:62  MCMNT
     H*                 CHANGED INDICATOR FOR DATES LESS THAN ACDT.
     H*
     H*  05/20/99 C:53  MC145
     H*                 REVIEW DATE BROUGHT FROM SCR02 TO SCR01
     H*
     H*
     H*  05/10/99 C:53  MC145
     H*                 QUALIFIED CALL TO WCMC PGMS FOR LINK W/NON PDS
     H*                 QLAIM SYSTEM
      *
      *  05/03/99 C:62  Project: MC098 16754
      *                 Initialized XXEXT so that if a re-cert is added
      *                 and another pre-cert is added after that, with-
      *                 out exiting the application, XXEXT will be set
      *                 to zero.  If this was not added, XXEXT will re-
      *                 remain the same as the last re-cert that was
      *                 added, which results in UMPCP not having match-
      *                 ing FAEOP/FAE1P/NAPBP records.
      *
      *  04/21/99 C:94  PROJECT: MC152
      *                 CHANGE WHAT RRN IS LOADED INTO AA001R AND
      *                 MOVE A 'C' INTO EOBLST
      *
      *  03/15/99 C:56  PROJECT: MC152
      *                 CHANGE EOBD PARM TO 3 NUMERIC
      *  02/11/99 C:94  PROJECT: 48MBV
      *                 CHANGE PROGRAM SO THAT IT WILL UPDATE
      *                 EOTRCD WITH THE CORRECT INFORMATION
      *
      *  01/28/99 C:53  Project: SUPP 16252
      *                 CHANGE INSTALL FLAG BASIS FOR UPDATE OF
      *                 EOSTAT DUE TO FLAG USAGE CHANGE.
      *
      *
      *  01/12/98 C:94  PROJECT: MC152
      *                 REPLACE WRITE TO FAE1P AND FAEOP WITH API
      *                 AA001R
      *
      *  12/31/98 C:62  Project: 25MKK
      *                 TO PROCESS OPT12 FOR U0060R.
      *
      *  12/23/98 C:62  Project: 25MKK
      *                 MOVE WRITE TO FAEOP, FAE1P AND NAPBP TO THIS
      *                 PROGRAM.
      *
      *  08/21/98 C:62  Project: MC177
      *                 -SETON OUTFLG TO ENTER ONLY ONE RECORD ON ADD.
      *                 -ADDED REVIEW DATE
      *
      *  08/19/98 C:62  Project: MC177
      *                 VARIOUS CHANGES TO T/M AND PRE-CERTS.
      *
      *  07/10/98 C:61  Project: 98MAG
      *                 Get rid of F6=Provider Inq.   Chg F10=Search for
      *                 "provider" + "requesting Phys" to give window
      *                 selection (F2014R) that F6 did.
      *
      *  07/08/98  C:94  PROJECT: MC145
      *                  CHANGED POSITION OF LIABILITY &
      *                  INTERVIEWER
      *
      *****************************************************************
      * INDICATOR USAGE:
      *
      * *IN16   -   *ON    -    RE-CERT
      * *IN16   -   *OFF   -    PRE-CERT
      *
      *****************************************************************
     FU0060F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FUMPCP     UF A E             DISK
     F                                     RENAME(PPC01:DBRCDF)
     FUMPCL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FUMPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FSMUPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP0P)
     FSTCTL1    IF   E           K DISK
     FFMCXL     IF   E           K DISK
     F*
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
      *
     FFADAP     IF   E             DISK
     F                                     RENAME(PDA01:PDA0P)
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F                                     INFDS(DBFDS2)
     FFMDIL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFTPSL     IF   E           K DISK    USROPN
      *  GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT0P)
     F                                     INFDS(DBFDS3)
      *  LOS FILE
      *  PRE-CERT STATUS HISTORY FILE
     FUMCHP     O    E             DISK
      *
     F*
     F*  WCMC claim Physician/Provider transaction file
     FFAPPP     IF   E             DISK
     FFMC1L     UF   E           K DISK
     FFMP1L     IF   E           K DISK
     FSTSTL     IF   E           K DISK
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
      /COPY SCOPYMT01D
     D QCA             S             56    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D LOW             S              1    DIM(26) CTDATA PERRCD(1) ASCEND      LOWER CASE/UPPER CAS
     D CAP             S              1    DIM(26) ALT(LOW)
     D LIAB            S              8    DIM(3) CTDATA PERRCD(1)              LIABILITY CODES
      *
      *  UMPCL
     D DBFDS           DS
     D  FLSTPC           *STATUS
     D  DBRRN                397    400B 0
      *
      *  FMCMP
     D DBFDS2          DS
     D  FLSTCM           *STATUS
     D  DBRRN2               397    400B 0
      *
      *  FTGTP
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
      *
     D                 DS                  INZ
     D  @@CMD                  1     56
     D  @@ST                  38     39
      *
     D                 DS                  INZ
     D  XXCERT                 1     15
     D  XXNAME                 1      4
     D  XX1NME                 1      1
     D  XX2NME                 2      2
     D  XX3NME                 3      3
     D  XX4NME                 4      4
     D  XXADDT                 5     12  0
     D  XXSEQ#                13     15  0
     D*
     D                 DS                  INZ
     D  HLCERT                 1     15
     D  HLNAME                 1      4
     D  HLADDT                 5     12  0
     D  HLSEQ#                13     15  0
     D*
     D                 DS                  INZ
     D*** Beg Chg *** 05/23/13 ***************************************
     D**DC                     1     25
     D******                               DIM(5)                               DIAG CODES - SCRN
     D**SCICD1                 1      5
     D**SCD1                   1      1
     D**SCICD2                 6     10
     D**SCD2                   6      6
     D**SCICD3                11     15
     D**SCD3                  11     11
     D**SCICD4                16     20
     D**SCD4                  16     16
     D**SCICD5                21     25
     D**SCD5                  21     21
     D*
     D  DC                     1     35
     D                                     DIM(5)                               DIAG CODES - SCRN
     D  SCICD1                 1      7
     D  SCD1                   1      1
     D  SCICD2                 8     14
     D  SCD2                   8      8
     D  SCICD3                15     21
     D  SCD3                  15     15
     D  SCICD4                22     28
     D  SCD4                  22     22
     D  SCICD5                29     35
     D  SCD5                  29     29
     D*** End Chg *** 05/23/13 ***************************************
     D*
     D                 DS                  INZ
     D  LOS                    1      5
     D                                     DIM(5)                               DIAG OR OPERATION
     D  SCLOS1                 1      1
     D  SCLOS2                 2      2
     D  SCLOS3                 3      3
     D  SCLOS4                 4      4
     D  SCLOS5                 5      5
     D*
     D                 DS                  INZ
     D  DS                     1    100
     D                                     DIM(5)                               DIAG CODES - DESC
     D  SCDGD1                 1     20
     D  SCDGD2                21     40
     D  SCDGD3                41     60
     D  SCDGD4                61     80
     D  SCDGD5                81    100
      *
     D                 DS                  INZ
     D  XXAGE                  1      6  0
     D  XXYR                   1      2  0
     D                 DS                  INZ
     D  XXLPAS                 1      6
     D  XXRGN                  1      1  0
     D  XXGRP                  2      6
     D*
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
      *
     D OPTNM           C                   CONST('Pre-Certification -
     D                                     Information')
     D OPTNMR          C                   CONST('Re-Certification -
     D                                     Information')
     D COMAPI          C                   CONST('FA007R')
     D XCOROP          C                   CONST('Original open of -
     D                                     Pre-Cert')
     D XCRCRT          C                   CONST('Re-Certification')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D***Beg Add *** 05/23/13 ***************************************
     D SA015R          pr                  extpgm('SA015R')
     D @@rtnc                         1
     D*
     D xxicd10         s              1
     D @@rtnc          s              1
     D***End Add *** 05/23/13 ***************************************
     D*** Beg Add *** 08/05/13 ***************************************
     D kkicd10         s                   like(iccode)
     D kkLost          s                   like(DIlost)
     D*** End Add *** 08/05/13 ***************************************

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
     I              PCINTR                      P2INTR
     I              PCPASG                      P2PASG
     I              PCREQD                      P2REQD
     I              PCCERD                      P2CERD
     I              PCRECD                      P2RECD
     I              PCPOS                       P2POS
     I              PCUSER                      P2USER
     I              PCPGM                       P2PGM
     I              PCTIME                      P2TIME
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
     I              PCLOS1                      P2LOS1
     I              PCLOS2                      P2LOS2
     I              PCLOS3                      P2LOS3
     I              PCLOS4                      P2LOS4
     I              PCLOS5                      P2LOS5
     I              PC92N1                      P292N1
     I              PC92N2                      P292N2
     I              PC92N3                      P292N3
     I              PC92N4                      P292N4
     I              PC92N5                      P292N5
     I              PC1A1                       P21A1
     I              PC1A2                       P21A2
     I              PC1A3                       P21A3
     I              PC1A4                       P21A4
     I              PC1A5                       P21A5
     I              PC1A6                       P21A6
     I              PC1A7                       P21A7
     I              PC1A8                       P21A8
     I              PC1A9                       P21A9
     I              PC1A10                      P21A10
     I              PCDT1                       P2DT1
     I              PCDT2                       P2DT2
     I              PCDT3                       P2DT3
     I              PCDT4                       P2DT4
     I              PCDT5                       P2DT5
     I              PC10A1                      P210A1
     I              PC10A2                      P210A2
     I              PC10A3                      P210A3
     I              PCAMTS                      P2AMTS
     I              PCCALS                      P2CALS
     I              PCUSRS                      P2USRS
     I              PCCOMM                      P2COMM
     I              PCLPAS                      P2LPAS
     I*
      *
      *
      * ===================================================================

     C*
     C*  COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    XXCO#
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  DIAGNOSIS CODE FILE
      *
     C     DIKEY         KLIST
     C                   KFLD                    XXICD9
     C                   KFLD                    XXLOS
      *
      *  FS BASE CONTROL FILE
      *
      *
      *   KEY LIST FOR THE FUND
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
      *
      * Xref to WCMS Fund # to get WCMC Sub#/Fund
      *
     C     FNKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
      ** LOS FILE
      **
      **         LSKEY     KLIST
      **                   KFLD           XXRGN
      **                   KFLD           X1GRP
      **                   KFLD           XXCAT
      **                   KFLD           XXAGEC
      **                   KFLD           XXEFDT
      *
      *   KEY LIST FOR THE PRE-CERTIFICATION MASTER FILE
      *
     C     PCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
      *
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
      *
     C     PCKY2         KLIST
     C                   KFLD                    XXCERT
      *
     C     PCKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C                   KFLD                    KKEXT
     C     P1KY          KLIST
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
     C*
     C     P1KY1         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C     STKY          KLIST
     C                   KFLD                    XXAST            10
      *
      *  PAYEE DEMOGRAPHIC RECORD
      *
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
      *  KEY LIST FOR PLACE OF SERVICE CODE FILE   (FTPSLxx)
      *
     C     PSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPOS
      *
      *   KEY LIST FOR THE SUBSCRIBER
      *
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
     C     UPKY1         KLIST
     C                   KFLD                    SCINTR
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** End Add *** 08/05/13 ***************************************
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     *ENTRY        PLIST
     C***** Beg Chg ***** 05/27/14 ******************************
     C***** *INKC         PARM                    @@KC              1
     C***** *INKL         PARM                    @@KL              1
     C     *INKC         PARM      *INKC         @@KC              1
     C     *INKL         PARM      *INKL         @@KL              1
     C***** End Chg ***** 05/27/14 ******************************
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM                    QQOCST            1
     C                   PARM                    QQCONT            1
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM                    QQCONT            1
     C                   PARM                    @@MODE            1
      *
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM                    ##OCST            1
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
      *
      *
      * PROVIDER SELECTION
      *  ZZZ
     C     PROVSL        PLIST
     C                   PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0          FAPPP
     C                   PARM                    ##RRN2            9 0          FMCMP
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANK        ##RTCD            1
      *
      *
      * INITIALIZE WORK FIELDS
      *
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PCFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    PCCASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    PCADDT        XNADDT                         7.0N
     C     *LIKE         DEFINE    PDST          XXST                           2A
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C     *LIKE         DEFINE    PCEXT         XXEXT                          3.0N
     C     *LIKE         DEFINE    CMSS#         XXSS#                          9A
     C     *LIKE         DEFINE    PSPOS         XXPOS                          New Place o      f S
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMACDT        XXDT
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    DILOST        XXLOS
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTTBID        @@TBID
     C     *LIKE         DEFINE    PCCAT         XXCAT                          1.0N
     C     *LIKE         DEFINE    PCLOST        XXLOST                         1A
     C     *LIKE         DEFINE    PCRECD        XXTREC          + 2
     C     *LIKE         DEFINE    PCREQD        XXTREQ          + 2
     C     *LIKE         DEFINE    PCCERD        XXTCER          + 2
     C     *LIKE         DEFINE    PCRVDT        XXRVDT
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    PCEXT         KKEXT
      *
      /COPY SCOPYMT01C
      *
      *===============================================================
      * CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
      *===============================================================
      *
     C     CALLR         BEGSR
      *
     C     SCRN#         IFEQ      #OFSCR
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   END
      *
     C     PCKY1         CHAIN     PPC01                              60
     C                   Z-ADD     DBRRN         @@RRN
      *
     C                   CALL      'U0061R'      ACDPL
      *
     C     *INKL         IFEQ      *ON
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'C'           @@MODE
     C                   END
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
      *
     C                   MOVE      *ON           *IN90
     C                   END
     C                   END
     C                   ENDSR                                                  CALLR
      *
      *===============================================================
      * DATCVT- CONVERTS MDY DATE TO MDYY DATE AFTER SEARCH.
      *===============================================================
      *
     C     DATCVT        BEGSR
      *
     C                   Z-ADD     SCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCADDT
      *
     C                   ENDSR
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C     1             SUBST(P)  Q2RECF:5      SCRN#A            1
     C                   MOVE      SCRN#A        SCRN#
      *
     C     SCRN#         IFEQ      #OFSCR
     C     QQCONT        ANDEQ     'Y'
     C                   EXSR      CALLR
     C                   END
      *
     C                   ENDSR                                                  DSPLYR
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C                   DO
      *
      * VALIDATE THE SUBSCRIBER AND THE GROUP
      *
     C     SMKEY         CHAIN     PSM01                              39
     C     FNKEY         CHAIN     PFN01                              39
     C     *IN39         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      **
      *  VALIDATE LIABILTY ENTRY
      *
     C     SCLIAB        IFNE      'A'
     C     SCLIAB        ANDNE     'D'
     C     SCLIAB        ANDNE     'P'
     C                   MOVEL(P)  'LIAB'        XXTBID
     C                   MOVEL(P)  SCLIAB        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9038
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     SCRVDT        IFNE      *ZEROS
     C                   Z-ADD     SCRVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRVDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END SCRVDT
      *
      **
      * VALIDATE THE INTERVIEWER
      *
     C     UPKY1         CHAIN     PUP02                              60
     C*
     C*
     C* IF INTERVIEWER REC WAS FOUND, CHECK IF INACTIVE
     C*
     C     *IN60         IFEQ      *OFF
     C     UPSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
      *
      *  RESET THE USER PARAMETER INFO AS IT WAS LOST
      *
     C     UPKEY         CHAIN     PUP01                              85
     C     *IN85         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   END
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN66
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * VALIDATE PLACE OF SERVICE
      *
      *   CLOSE FILE IF OPENED
      *
      *
      *   OVERRIDE TO FTPSL WITHOUT STATE
      *
      *
      *
      *   OVERRIDE TO FTPSL WITH STATE
      *
      *
      *   OPEN FILE FOR PROCESSING
      *
      *
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * VALIDATE ADDMISSION DATE
      *
     C     SCADDT        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN37
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END SCADDT EQ *ZEROS
      *
     C     SCADDT        IFNE      *ZEROS
     C                   Z-ADD     SCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCADDT
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * ADMIT/PLAN DATE MUST NOT BE LESS THAN DATE OF INJURY
      *
     C                   Z-ADD     SCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XNADDT
      *
     C     XNADDT        IFLT      XXACDT
     C                   MOVE      *ON           *IN88
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   END                                                    END SCADDT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** VALIDATE PROVIDER/MOD
      *
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDNE     'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN41
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END *IN60
      *
     C                   MOVEL(P)  PDNAME        SCPRNM
     C                   Z-ADD     PDAREA        SCPRAC
     C                   MOVEL     PDPHN#        SCPPH1
     C                   MOVE      PDPHN#        SCPPH2
      *
      *  CONTROL THE DISPLAY OF THE PHONE NUMBER
      *
     C     SCPRAC        IFEQ      *ZEROS
     C     SCPPH1        ANDEQ     *ZEROS
     C     SCPPH2        ANDEQ     *ZEROS
     C                   MOVE      '0'           *IN52
     C                   ELSE
     C                   MOVE      '1'           *IN52
     C                   END
      *
      ** IF TREATING PROV/MOD ENTERED MUST BE VALID
      *
     C     SCTPHY        IFNE      *ZEROS                                       ADD 06/03/97
     C                   MOVEL     SCTPHY        XXFEIN
     C                   Z-ADD     SCTPHM        XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDNE     'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN67
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END *IN60
      *
     C                   MOVE      *BLANKS       SCTPNM
     C                   MOVEL     PDNAME        SCTPNM
     C                   Z-ADD     PDAREA        SCTPAC
     C                   MOVEL     PDPHN#        SCTPH1
     C                   MOVE      PDPHN#        SCTPH2
      *
     C     SCTPAC        IFEQ      *ZEROS
     C     SCTPH1        ANDEQ     *ZEROS
     C     SCTPH2        ANDEQ     *ZEROS
     C                   MOVE      '0'           *IN53
     C                   ELSE
     C                   MOVE      '1'           *IN53
     C                   END
      *
     C                   END                                                    SCTPHY = *ZEROS
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * VALIDATE PLACE OF SERVICE
      *
      *   CLOSE FILE IF OPENED
      *
     C     OPNPS         IFEQ      '1'
     C                   CLOSE     FTPSL
     C                   MOVE      ' '           OPNPS             1
     C                   END                                                    END OPNPS=1
      *
      *   OVERRIDE TO FTPSL WITHOUT STATE
      *
     C     OVRPS         IFEQ      '1'
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRPS             1
     C                   END                                                    END OVRPS=1
      *
     C     OVRPS         IFEQ      ' '
      *
      *   OVERRIDE TO FTPSL WITH STATE
      *
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(6)        @@CMD
     C                   MOVEL     XXST          @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRPS             1
     C                   END                                                    END OVRPS=0
      *
      *   OPEN FILE FOR PROCESSING
      *
     C     OPNPS         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FTPSL
     C                   MOVE      '1'           OPNPS
     C                   END                                                    END OPNPS= ' '
      *
     C                   MOVE      SCPOS         XXPOS
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN30
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   IF DIAGNOSIS CODE ENTERED,DIAG OR OPER MUST BE ENTERED IN SCLOS1-5
      *                                                                        T
     C                   DO        5             Y                 5 0
     C     Y             ADD       46            Y1                2 0
     C     DC(Y)         IFNE      *BLANK
     C     LOS(Y)        IFNE      'D'
     C     LOS(Y)        ANDNE     'O'
     C                   MOVE      '1'           *IN(Y1)
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END NE O OR D
     C                   END                                                    END DC <> BLANK
     C                   ENDDO                                                  END DO
      *                                                                        T
      *   IF DIAGNOSIS CODE IS BLANK, LOS MUST ALSO BE BLANK.
      *                                                                        T
     C                   DO        5             Y                 5 0
     C     Y             ADD       46            Y1                2 0
     C     DC(Y)         IFEQ      *BLANK
     C     LOS(Y)        ANDNE     *BLANK
     C                   MOVE      '1'           *IN(Y1)
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END DC <> BLANK
     C                   ENDDO                                                  END DO
      *
      **  IF DIAGNOSIS CODE ENTERED, MUST BE VALID
      *
     C                   MOVE      '0'           *IN92
     C                   DO        5             Y                 5 0
     C     Y             ADD       92            Y1                2 0
     C     DC(Y)         IFNE      *BLANK
     C                   MOVEL     *BLANK        XXICD9
     C*** Beg Chg *** 08/05/13 ****************************************
     C*****              MOVEL     DC(Y)         XXICD9
     C                   MOVEL     DC(Y)         XXICD9
     C*** End Chg *** 08/05/13 ****************************************
     C                   MOVEL     LOS(Y)        XXLOS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       //*** Beg Chg *** 06/09/15 ****************************************
       //****If xxICD10 = '1' and scICdInd ='Y';
       If scICdInd ='Y';
       //*** End Chg *** 06/09/15 ****************************************
          kkICd10 = DC(Y);
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in92 = *on;
                *IN(Y1) = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN92                          INVALID DIAG CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   MOVE      *ON           *IN90
     C                   LEAVE
      *
     C                   ELSE                                                   *IN60 = 0
     C     DISTAT        IFNE      'A'
     C                   MOVE      '1'           *IN92                          INVALID DIAG CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   ELSE                                                   *IN60 = 0
     C                   MOVE      '0'           *IN92                          INVALID DIAG CODE
     C                   MOVE      '0'           *IN(Y1)
     C                   MOVE      *Off          *IN90
     C*** End Add *** 08/05/13 ****************************************
     C                   END                                                    END NE A
     C                   END                                                    END *IN60
      *
     C                   END                                                    END DC <> BLANK
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDDO                                                  END DO
      *
      ** GET DIANOSTIC CODE DESCRIPTION IF PRESENT
      *
     C*** Beg Add *** 08/05/13 ****************************************
     C                   Eval      Y = 1
     C*** End Add *** 08/05/13 ****************************************
     C                   DO        5             X
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       //*** Beg Chg *** 06/09/15 ****************************************
       //****If xxICD10 = '1' and scICdInd ='Y';
       If scICdInd ='Y';
       //*** End Chg *** 06/09/15 ****************************************
          kkICd10 = DC(Y);
          Chain fmicd10l1key pic01;
                If %found (fmicd10l1);
                   DS(X) = ICdsc1;
                Else;
                   DS(X) = *Blanks;
                Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       XXICD9
     C*** Beg Chg *** 06/09/15 *********************************************
     C*****************  MOVE      DC(X)         XXICD9
     C                   MOVEL     DC(X)         XXICD9
     C*** End Chg *** 06/09/15 *********************************************
     C                   MOVEL     LOS(X)        XXLOS
     C     DIKEY         SETGT     PDI01
     C     NXTDC1        TAG
     C                   READP     PDI01                                  60
     C     *IN60         IFEQ      '0'
      *
     C     DISTAT        IFNE      'I'
     C                   MOVEL     DIDSC1        DS(X)
     C                   ELSE                                                   DISTAT = I
     C                   GOTO      NXTDC1
     C                   END                                                    END DISTAT
      *
     C                   ELSE                                                   *IN60 = 1
     C                   MOVEL     *BLANK        DS(X)
     C                   END                                                    END *IN60
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDDO
      *
     C                   ENDDO
     C                   Z-ADD     1             SCRN#             1 0
     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
      *
     C                   DO
     C*
     C*
      *
      * VALIDATE REQUESTED DAYS - SCREQD NOT LESS THAN 0
      *
     C     SCREQD        IFLT      *ZERO
     C                   MOVE      '1'           *IN44
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END SCREQD
      *
      * VALIDATE LOS CERTIFIED DAYS - SCCERD NOT LESS THAN 0
      *
     C     SCCERD        IFLT      *ZERO
     C                   MOVE      '1'           *IN45
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END SCCERD
      *
      * PASG IS DISPLAY ONLY
      *
      * IF THE PROFESSIONAL ACTIVITY STUDY GROUP IS ENTERED, VALIDATE
      * AGAINST THE LOS MASTER FILE
      *
      *          SCLPAS    IFNE *BLANKS
      *                    Z-ADDXXDT      XXEFDT
      *                    MOVELSCLPAS    XXLPAS
      *                    MOVELXXGRP     X1GRP  10 P
      *
      *          LSKEY     SETGTPLS01
      *                    READPPLS01                    60
      *          *IN60     IFEQ '1'
      *                    MOVE '1'       *IN29
      *                    MOVE *ON       *IN90
      *                    LEAVE
      *                    ELSE
      *
      *          LSSTAT    IFEQ 'I'
      *                    MOVE '1'       *IN29
      *                    MOVE *ON       *IN90
      *                    LEAVE
      *                    ELSE
      *                    MOVELLSLOST    XXLOST
      *                    END                             END LSSTAT
      *
      *                    END                             END *IN60
      *                    END                             END SCLPAS
      *
      * VALIDATE RECOMMENDED DAYS - SCRECD NOT LESS THAN 0
      *
     C     SCRECD        IFLT      *ZERO
     C                   MOVE      '1'           *IN46
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END SCRECD
      *
     C                   ENDDO
      *
     C                   Z-ADD     2             SCRN#
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   Z-ADD     3             SCRN#
     C                   ENDSR
      *================================================================
      * INIT SUBROUTINE - INITALIZE ALL FIELDS ON ENTRY                 *
      *================================================================
     C     INIT          BEGSR
      *
     C                   MOVE      *BLANKS       OPNPS             1            File Open FTPSL
     C                   MOVE      *BLANKS       OVRPS             1            File Ovrd FTPSL
     C                   MOVE      *BLANKS       ZZPOS             3            3A POS for SUBST ope
      *
     C                   Z-ADD     *ZERO         SCSUB#            5 0
     C                   Z-ADD     *ZERO         SCFND             3 0
     C                   Z-ADD     *ZERO         SCFYR             3 0
     C                   Z-ADD     *ZERO         SCYR              3 0
     C                   Z-ADD     *ZERO         SCYR2             2 0
     C                   Z-ADD     *ZERO         SCCASE            7 0
     C                   MOVE      *ZERO         SCSS#             9
     C                   Z-ADD     *ZERO         SCACDT            8 0
     C                   MOVE      *BLANK        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C                   Z-ADD     *ZEROS        SCTREC
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      *BLANKS       SCSTAD
     C                   MOVE      *BLANKS       SCLIAB
     C                   MOVE      *BLANKS       SCLBDS
     C                   MOVE      *BLANKS       SCINTR
     C                   MOVE      *BLANKS       SCINTD
     C                   MOVE      *BLANKS       SCPOS
     C                   MOVE      *BLANKS       SCPOSD
     C                   Z-ADD     *ZEROS        SCADDT
     C                   MOVE      *BLANKS       SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C                   MOVE      *BLANKS       SCPRNM
     C                   MOVE      *BLANKS       SCPRAC
     C                   MOVE      *BLANKS       SCPPH1
     C                   MOVE      *BLANKS       SCPPH2
     C                   MOVE      *BLANKS       SCTPHY
     C                   Z-ADD     *ZEROS        SCTPHM
     C                   MOVE      *BLANKS       SCTPNM
     C                   MOVE      *BLANKS       SCTPAC
     C                   MOVE      *BLANKS       SCTPH1
     C                   MOVE      *BLANKS       SCTPH2
     C                   MOVE      *BLANKS       DC
     C                   MOVE      *BLANKS       LOS
     C                   MOVE      *BLANKS       DS
      * SCREEN 2
     C                   MOVE      *BLANKS       SCLPAS
     C                   Z-ADD     *ZEROS        SCRVDT
     C                   Z-ADD     *ZEROS        SCREQD
     C                   Z-ADD     *ZEROS        SCCERD
     C                   Z-ADD     *ZEROS        SCRECD
     C                   Z-ADD     *ZEROS        XXAGEC            1 0          1.0N
     C                   Z-ADD     *ZEROS        XXRGN
     C                   Z-ADD     *ZEROS        SCAMTS
     C                   Z-ADD     *ZEROS        SCCALS
     C                   Z-ADD     *ZEROS        SCUSRS
     C                   Z-ADD     *ZEROS        SCDT1
     C                   MOVE      *BLANKS       SCCOMM
     C                   ENDSR                                                  INIT
      *===============================================================
      * KFDO - SUBROUTINE FOR F6 = CASE PROVIDERS
      *===============================================================
     C*===============================================================
     C* KGDO - CASE PROVIDER
     C*===============================================================
     C     KGDO          BEGSR
     C*
     C     CMKY1         CHAIN     PCM02                              99
     C*
     C                   CALL      'F0277R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      '1'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN2        ##RRN2            9 0
     C*
     C                   ENDSR
     C*===============================================================
     C* KHDO - BILL HISTORY
     C*===============================================================
     C     KHDO          BEGSR
     C*
     C                   CALL      'F0350R'
     C                   PARM      '1'           ##CALL            1
     C                   PARM      PCSUB#        ##SUB#            5 0
     C                   PARM      PCFND         ##FND             3 0
     C                   PARM      PCFYR         ##FYR             3 0
     C                   PARM      PCCASE        ##CASE            7 0
     C                   PARM      *ZEROS        ##EOB#            9 0
     C                   PARM      PCCERT        ##CERT           15
     C*
     C                   ENDSR
     C*===============================================================
     C* KIDO - CLAIM INFORMATION
     C*===============================================================
     C     KIDO          BEGSR
     C*
     C                   CALL      'W0070R'                             99      CHG 05/10/99
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'M'           ##MODE            1
     C                   PARM                    ##RRN3            9 0
     C                   PARM      *BLANKS       ##SORT            2
     C*
     C                   ENDSR
     C*===============================================================
     C* KKDO - CASE INFORMATION
     C*===============================================================
     C     KKDO          BEGSR
     C*
     C     CMKY1         CHAIN     PCM02                              99
     C*
     C                   CALL      'F0260R'
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      'L'           ##MODE
     C                   PARM      DBRRN2        ##RRN
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C*
     C                   ENDSR
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
     C     KJDO          BEGSR
     C     CMKY1         CHAIN     PCM02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CMAST         XXAST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     STINST        ANDEQ     'Y'
     C*
     C                   MOVE      '0'           *IN98
     C                   MOVE      CMAST         XXST
     C*
     C                   ELSE
     C*
     C     P1KY1         CHAIN     PP101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   SETON                                        98
     C                   MOVEL(P)  P1ST          XXST
     C                   ELSE
     C*
     C     HFLD          IFEQ      'SCPOS'
     C*
     C                   SETON                                        4190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *    *IN54 DENOTES THAT SOME FIELDS HAVE BEEN DEFAULTED
      *    FOR A RE-CERT.  IF *IN54 IS ON, THE FIELDS ARE PROTECTED
      *    AND NOT ELGIBLE FOR A SEARCH.
      *
     C     HFLD          IFEQ      'SCICD1'
     C     HFLD          OREQ      'SCICD2'
     C     HFLD          OREQ      'SCICD3'
     C     HFLD          OREQ      'SCICD4'
     C     HFLD          OREQ      'SCICD5'
     C     CMKY1         CHAIN     PCM02                              99
     C                   Z-ADD     DBRRN2        @@RRN2
     C                   END
      *
     C                   SELECT
      *
      *  LIABILITY
      *
     C     HFLD          WHENEQ    'SCLIAB'
     C                   CALL      'F2380R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C                   PARM      *ZERO         @@RRN3            9 0
     C                   PARM      'LIAB'        @@TBID
      *
     C     @@RRN3        IFEQ      *ZERO
     C                   MOVE      *ON           *IN33                          POS CUSR
     C                   END
      *
     C     @@RRN3        IFNE      *ZERO
     C                   Z-ADD     @@RRN3        DBRRN3
     C     DBRRN3        CHAIN     PGT0P                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTKEY         SCLIAB
     C                   MOVEL(P)  GTDESC        SCLBDS
     C                   MOVE      *ON           *IN14                          POS CUSR
     C                   END
     C                   END
      *
      *  INTERVIEWER
      *
     C     HFLD          WHENEQ    'SCINTR'
     C     *IN54         ANDEQ     *OFF                                         NOT A RE-CERT
     C                   CALL      'S0005R'      UPRMPL
      *
     C     ##RRN         IFEQ      *ZERO
     C                   MOVE      '1'           *IN32
     C                   END
      *
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PUP0P                              99
      *
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '1'           *IN33                          POSITION CURSOR
     C                   MOVEL(P)  UPSTAF        SCINTR
     C                   MOVEL(P)  UPUSNM        SCINTD
     C                   END                                                    ENDEQ '*'
      *
     C                   END                                                    ##RRN NE 0
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   PLACE OF SERVICE SEARCH
      *
      *   CLOSE FILE IF OPENED
      *
     C     HFLD          WHENEQ    'SCPOS'
     C     *IN54         ANDEQ     *OFF                                         NOT A RE-CERT
     C     *IN90         IFEQ      *OFF
      *
      * NOTE: FTPSL2 DOES NOT HAVE TO BE OVERRIDDEN BECAUSE IT'S
      *       NOT USED IN THE SEARCH.
      *
     C     OPNPS         IFEQ      '1'
     C                   CLOSE     FTPSL
     C                   MOVE      ' '           OPNPS             1
     C                   END                                                    END OPNPS=1
      *
      **  OVERRIDE TO FTPSL WITHOUT STATE
      *
     C     OVRPS         IFEQ      '1'
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRPS             1
     C                   END                                                    END OVRPS=1
      *
      **  OVERRIDE TO FTPSL WITH STATE
      *
     C     OVRPS         IFEQ      ' '
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(6)        @@CMD
     C                   MOVEL     XXST          @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRPS             1
     C                   END                                                    END OVRPS=0
      *
      *   OPEN FILE FOR PROCESSING
      *
     C     OPNPS         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FTPSL
     C                   MOVE      '1'           OPNPS
     C                   END                                                    END OPNPS= ' '
      *
     C                   CALL      'F2370R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      XXST          @@ST              2
     C                   PARM      *BLANK        @@POS            15
      *
     C     @@POS         IFEQ      *BLANK
     C                   MOVE      *ON           *IN36
     C                   END
      *
     C     @@POS         IFNE      *BLANK
     C                   MOVE      *ON           *IN34
     C                   MOVEL(P)  @@POS         SCPOS
     C                   MOVEL(P)  SCPOS         XXPOS
     C                   MOVE      *BLANK        SCPOSD
     C                   MOVE      *BLANK        SCOPOS
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PSDESC        SCPOSD
     C                   MOVEL(P)  PSKEY         SCOPOS
     C                   END                                                    END *IN60
     C                   END                                                    END <> BLANK
     C                   ENDIF
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** TREATING PHYSICIAN
      *
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  PROVIDER
      *
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
      *
     C     CMKY1         CHAIN     PCM02                              99
     C                   Z-ADD     DBRRN2        ##RRN2
     C                   CALL      'F2014R'      PROVSL                 99
      *
     C     ##RRN         IFEQ      *ZERO
     C     ##FEIN        ANDEQ     *BLANKS
      *
     C                   MOVE      '1'           *IN31
     C                   ELSE
      *
     C                   MOVE      '1'           *IN35
      * ZZZ T
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PPP01                              60
      *
     C     PPFEIN        IFEQ      *ZERO
     C                   MOVEL(P)  *BLANKS       SCFEIN
     C                   MOVEL(P)  *BLANKS       SCMOD
     C                   MOVEL(P)  PP40A1        SCPRNM
     C                   ELSE
     C                   MOVE      PPFEIN        SCFEIN
     C                   MOVE      PPMOD         SCMOD
     C                   ENDIF                                                  PPFEIN
      *
     C                   ELSE
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C                   ENDIF
      *
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
      *
     C     XXFEIN        IFNE      *ZERO
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
      *
     C                   Z-ADD     PDAREA        SCPRAC
     C                   MOVEL     PDPHN#        SCPPH1
     C                   MOVE      PDPHN#        SCPPH2
      *
      *
     C                   MOVEL(P)  PDNAME        XXLAST
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCPRNM
     C                   ELSE
     C                   MOVEL     PDNAME        SCPRNM
     C                   ENDIF                                                  END PDFSN
      *
     C                   ENDIF                                                  END *IN60
      *
     C                   ENDIF                                                  XXFEIN NE 0
      *
     C                   ENDIF                                                  ##RRN
      *
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  REQUESTING PHYS
      *
     C     HFLD          WHENEQ    'SCTPHY'
     C     HFLD          OREQ      'SCTPHM'
      *
     C     CMKY1         CHAIN     PCM02                              99
     C                   Z-ADD     DBRRN2        ##RRN2
      *
     C                   CALL      'F2014R'      PROVSL                 99
      *
      *
     C     ##RRN         IFEQ      *ZERO
     C     ##FEIN        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN35
     C                   ELSE
      *
     C                   MOVE      '1'           *IN36
      * ZZZ
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PPP01                              60
      *
     C     PPFEIN        IFEQ      *ZERO
     C                   MOVEL(P)  *BLANKS       SCTPHY
     C                   MOVEL(P)  *BLANKS       SCTPHM
     C                   MOVEL(P)  PP40A1        SCTPNM
     C                   MOVE      '0'           *IN53
     C                   ELSE
     C                   MOVE      PPFEIN        SCTPHY
     C                   MOVE      PPMOD         SCTPHM
     C                   ENDIF                                                  PPFEIN
      *
     C                   ELSE
     C                   MOVE      ##FEIN        SCTPHY
     C                   MOVE      ##MOD         SCTPHM
     C                   ENDIF
      *
     C                   MOVE      SCTPHY        XXFEIN
     C                   MOVE      SCTPHM        XXMOD
      *
     C     XXFEIN        IFNE      *ZERO
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C                   Z-ADD     PDAREA        SCTPAC
     C                   MOVEL     PDPHN#        SCTPH1
     C                   MOVE      PDPHN#        SCTPH2
      *
     C     SCTPAC        IFEQ      *ZEROS
     C     SCTPH1        ANDEQ     *ZEROS
     C     SCTPH1        ANDEQ     *ZEROS
     C                   MOVE      '0'           *IN53
     C                   ELSE
     C                   MOVE      '1'           *IN53
     C                   END
      *
     C                   MOVEL(P)  PDNAME        XXLAST
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCTPNM
     C                   ELSE
     C                   MOVEL     PDNAME        SCTPNM
     C                   ENDIF                                                  END PDFSN
      *
     C                   ENDIF                                                  END *IN60
     C                   ENDIF                                                  XXFEIN
     C                   ENDIF                                                  ##RRN
      *
      *
      *
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DIAGNOSIS CODE 1
      *
     C     HFLD          WHENEQ    'SCICD1'
     C*** Beg Chg *** 08/05/13 ****************************************
     C                   If        scICDind <> 'Y'
     C                   eval      xxicd10 = '0'
     C                   eval      @@LOST = *Blanks
     C                   else
     C                   eval      xxicd10 = '1'
     C                   eval      @@LOST = 'D'
     C                   Endif
     C*** End Chg *** 08/05/13 ****************************************
     C                   CALL      'F2012R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZERO         @@RRN1            9 0          FADAP FILE
     C*** Beg Chg *** 08/05/13 ****************************************
     C********           PARM      *BLANK        @@ICD9            5            ICD9 FILE
     C********           PARM      *BLANK        @@LOST            1            ICD9 FILE
     C                   PARM      *BLANK        @@ICD9            7            ICD9 FILE
     C                   PARM                    @@LOST            1            ICD9 FILE
     C*** End Chg *** 08/05/13 ****************************************
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@RRN2            9 0          FMCMP FILE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   PARM                    xxicd10
     C*** End Add *** 08/05/13 ****************************************
      *
      *  DID NOT MAKE A SELECTION
      *
     C     @@RTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN34                          STAY ON FIELD
     C                   ELSE
     C                   MOVE      *ON           *IN21                          GO TO NEXT FLD
     C                   END
      *
     C     @@RRN1        IFNE      *ZERO
     C     @@RRN1        CHAIN     PDA0P                              60
     C     *IN60         IFEQ      *OFF
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-9
     C*
     C     DAiCD9        IFNE      *blanks
     C*** End Add *** 06/09/15 ****************************************
     C                   MOVEL(P)  DAICD9        SCICD1
     C                   MOVEL(P)  DALOST        SCLOS1
      *
     C                   MOVEL(P)  DAICD9        XXICD9
     C                   MOVEL(P)  DALOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD1
     C                   END
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-10
     C*
     C                   Else
     C                   MoveL(P)  DAiCD1001     scICD1
     C                   MoveL(P)  DAlost        scLOS1
      *
     C                   MoveL(P)  DAiCD9        kkICD10
      *
     C     fmicd10l1key  Chain     pic01                              60
     C     *in60         IfEq      *off
     C                   MoveL(P)  ICdsc1        SCDGD1
     C                   EndIf
      *
     C                   EndIf
     C*** End Add *** 06/09/15 ****************************************
     C                   END
      *
     C                   END                                                    @@RRN1 NE 0
      *
     C     @@ICD9        IFNE      *BLANK
     C                   MOVEL(P)  @@ICD9        SCICD1
     C                   MOVEL(P)  @@LOST        SCLOS1
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          scDGd1 = *Blanks;
          kkICD10 = @@icd9;
          Chain fmicd10l1key pic01;
          If %found(fmicd10l1);
             scDGd1 = ICdsc1;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  @@ICD9        XXICD9
     C                   MOVEL(P)  @@LOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD1
     C                   END
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   END                                                    @@ICD9 NE 0
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DIAGNOSIS CODE 2
      *
     C     HFLD          WHENEQ    'SCICD2'
     C*** Beg Chg *** 08/05/13 ****************************************
     C                   If        scICDind <> 'Y'
     C                   eval      xxicd10 = '0'
     C                   eval      @@LOST = *Blanks
     C                   else
     C                   eval      xxicd10 = '1'
     C                   eval      @@LOST = 'D'
     C                   Endif
     C*** End Chg *** 08/05/13 ****************************************
     C                   CALL      'F2012R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZERO         @@RRN1            9 0          FADAP FILE
     C*** Beg Chg *** 08/05/13 ****************************************
     C********           PARM      *BLANK        @@ICD9            5
     C********           PARM      *BLANK        @@LOST            1
     C                   PARM      *BLANK        @@ICD9            7
     C                   PARM                    @@LOST            1
     C*** End Chg *** 08/05/13 ****************************************
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@RRN2                         FMCMP FILE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   PARM                    xxicd10
     C*** End Add *** 08/05/13 ****************************************
      *
      *  DID NOT MAKE A SELECTION
      *
     C     @@RTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN21                          STAY ON FIELD
     C                   ELSE
     C                   MOVE      *ON           *IN22                          GO TO NEXT FLD
     C                   END
      *
     C     @@RRN1        IFNE      *ZERO
     C     @@RRN1        CHAIN     PDA0P                              60
     C     *IN60         IFEQ      *OFF
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-9
     C*
     C     DAiCD9        IFNE      *blanks
     C*** End Add *** 06/09/15 ****************************************
     C                   MOVEL(P)  DAICD9        SCICD2
     C                   MOVEL(P)  DALOST        SCLOS2
      *
     C                   MOVEL(P)  DAICD9        XXICD9
     C                   MOVEL(P)  DALOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD2
     C                   END
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-10
     C*
     C                   Else
     C                   MoveL(P)  DAiCD1001     scICD2
     C                   MoveL(P)  DAlost        scLOS2
      *
     C                   MoveL(P)  DAiCD9        kkICD10
      *
     C     fmicd10l1key  Chain     pic01                              60
     C     *in60         IfEq      *off
     C                   MoveL(P)  ICdsc1        SCDGD2
     C                   EndIf
      *
     C                   EndIf
     C*** End Add *** 06/09/15 ****************************************
     C                   END
     C                   END                                                    @@RRN NE 0
      *
     C     @@ICD9        IFNE      *BLANK
     C                   MOVEL(P)  @@ICD9        SCICD2
     C                   MOVEL(P)  @@LOST        SCLOS2
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICD10 = @@icd9;
          Chain fmicd10l1key pic01;
          If %found(fmicd10l1);
             scDGd2 = ICdsc1;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  @@ICD9        XXICD9
     C                   MOVEL(P)  @@LOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD2
     C                   END
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   END                                                    @@ICD9 NE 0
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DIAGNOSIS CODES
      *
     C     HFLD          WHENEQ    'SCICD3'
     C*** Beg Chg *** 08/05/13 ****************************************
     C                   If        scICDind <> 'Y'
     C                   eval      xxicd10 = '0'
     C                   eval      @@LOST = *Blanks
     C                   else
     C                   eval      xxicd10 = '1'
     C                   eval      @@LOST = 'D'
     C                   Endif
     C*** End Chg *** 08/05/13 ****************************************
     C                   CALL      'F2012R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZERO         @@RRN1            9 0          FADAP FILE
     C*** Beg Chg *** 08/05/13 ****************************************
     C********           PARM      *BLANK        @@ICD9            5
     C********           PARM      *BLANK        @@LOST            1
     C                   PARM      *BLANK        @@ICD9            7
     C                   PARM                    @@LOST            1
     C*** End Chg *** 08/05/13 ****************************************
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@RRN2            9 0          FMCMP FILE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   PARM                    xxicd10
     C*** End Add *** 08/05/13 ****************************************
      *
      *  DID NOT MAKE A SELECTION
      *
     C     @@RTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN22                          STAY ON FIELD
     C                   ELSE
     C                   MOVE      *ON           *IN23                          GO TO NEXT FLD
     C                   END
      *
     C     @@RRN1        IFNE      *ZERO
     C     @@RRN1        CHAIN     PDA0P                              60
     C     *IN60         IFEQ      *OFF
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-9
     C*
     C     DAiCD9        IFNE      *blanks
     C*** End Add *** 06/09/15 ****************************************
     C                   MOVEL(P)  DAICD9        SCICD3
     C                   MOVEL(P)  DALOST        SCLOS3
      *
     C                   MOVEL(P)  DAICD9        XXICD9
     C                   MOVEL(P)  DALOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD3
     C                   END
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-10
     C*
     C                   Else
     C                   MoveL(P)  DAiCD1001     scICD3
     C                   MoveL(P)  DAlost        scLOS3
      *
     C                   MoveL(P)  DAiCD9        kkICD10
      *
     C     fmicd10l1key  Chain     pic01                              60
     C     *in60         IfEq      *off
     C                   MoveL(P)  ICdsc1        SCDGD3
     C                   EndIf
      *
     C                   EndIf
     C*** End Add *** 06/09/15 ****************************************
     C                   END
     C                   END                                                    @@RRN NE 0
      *
     C     @@ICD9        IFNE      *BLANK
     C                   MOVEL(P)  @@ICD9        SCICD3
     C                   MOVEL(P)  @@LOST        SCLOS3
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICD10 = @@icd9;
          Chain fmicd10l1key pic01;
          If %found(fmicd10l1);
             scDGd3 = ICdsc1;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  @@ICD9        XXICD9
     C                   MOVEL(P)  @@LOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD3
     C                   END
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   END                                                    @@ICD9 NE 0
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DIAGNOSIS CODES
      *
     C     HFLD          WHENEQ    'SCICD4'
     C*** Beg Chg *** 08/05/13 ****************************************
     C                   If        scICDind <> 'Y'
     C                   eval      xxicd10 = '0'
     C                   eval      @@LOST = *Blanks
     C                   else
     C                   eval      xxicd10 = '1'
     C                   eval      @@LOST = 'D'
     C                   Endif
     C*** End Chg *** 08/05/13 ****************************************
     C                   CALL      'F2012R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZERO         @@RRN1            9 0          FADAP FILE
     C*** Beg Chg *** 08/05/13 ****************************************
     C********           PARM      *BLANK        @@ICD9            5
     C********           PARM      *BLANK        @@LOST            1
     C                   PARM      *BLANK        @@ICD9            7
     C                   PARM                    @@LOST            1
     C*** End Chg *** 08/05/13 ****************************************
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@RRN2            9 0          FMCMP FILE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   PARM                    xxicd10
     C*** End Add *** 08/05/13 ****************************************
      *
      *  DID NOT MAKE A SELECTION
      *
     C     @@RTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN23                          STAY ON FIELD
     C                   ELSE
     C                   MOVE      *ON           *IN24                          GO TO NEXT FLD
     C                   END
      *
     C     @@RRN1        IFNE      *ZERO
     C     @@RRN1        CHAIN     PDA0P                              60
     C     *IN60         IFEQ      *OFF
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-9
     C*
     C     DAiCD9        IFNE      *blanks
     C*** End Add *** 06/09/15 ****************************************
     C                   MOVEL(P)  DAICD9        SCICD4
     C                   MOVEL(P)  DALOST        SCLOS4
      *
     C                   MOVEL(P)  DAICD9        XXICD9
     C                   MOVEL(P)  DALOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD4
     C                   END
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-10
     C*
     C                   Else
     C                   MoveL(P)  DAiCD1001     scICD4
     C                   MoveL(P)  DAlost        scLOS4
      *
     C                   MoveL(P)  DAiCD9        kkICD10
      *
     C     fmicd10l1key  Chain     pic01                              60
     C     *in60         IfEq      *off
     C                   MoveL(P)  ICdsc1        SCDGD4
     C                   EndIf
      *
     C                   EndIf
     C*** End Add *** 06/09/15 ****************************************
     C                   END
     C                   END                                                    @@RRN NE 0
      *
     C     @@ICD9        IFNE      *BLANK
     C                   MOVEL(P)  @@ICD9        SCICD4
     C                   MOVEL(P)  @@LOST        SCLOS4
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICD10 = @@icd9;
          Chain fmicd10l1key pic01;
          If %found(fmicd10l1);
             scDGd4 = ICdsc1;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  @@ICD9        XXICD9
     C                   MOVEL(P)  @@LOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD4
     C                   END
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   END                                                    @@ICD9 NE 0
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DIAGNOSIS CODES
      *
     C     HFLD          WHENEQ    'SCICD5'
     C*** Beg Chg *** 08/05/13 ****************************************
     C                   If        scICDind <> 'Y'
     C                   eval      xxicd10 = '0'
     C                   eval      @@LOST = *Blanks
     C                   else
     C                   eval      xxicd10 = '1'
     C                   eval      @@LOST = 'D'
     C                   Endif
     C*** End Chg *** 08/05/13 ****************************************
     C                   CALL      'F2012R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZERO         @@RRN1            9 0          FADAP FILE
     C*** Beg Chg *** 08/05/13 ****************************************
     C********           PARM      *BLANK        @@ICD9            5
     C********           PARM      *BLANK        @@LOST            1
     C                   PARM      *BLANK        @@ICD9            7
     C                   PARM                    @@LOST            1
     C*** End Chg *** 08/05/13 ****************************************
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@RRN2            9 0          FMCMP FILE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   PARM                    xxicd10
     C*** End Add *** 08/05/13 ****************************************
      *
      *  DID NOT MAKE A SELECTION
      *
     C     @@RTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN24                          STAY ON FIELD
     C                   ELSE
     C                   MOVE      *ON           *IN33                          GO TO NEXT FLD
     C                   END
      *
     C     @@RRN1        IFNE      *ZERO
     C     @@RRN1        CHAIN     PDA0P                              60
     C     *IN60         IFEQ      *OFF
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-9
     C*
     C     DAiCD9        IFNE      *blanks
     C*** End Add *** 06/09/15 ****************************************
     C                   MOVEL(P)  DAICD9        SCICD5
     C                   MOVEL(P)  DALOST        SCLOS5
      *
     C                   MOVEL(P)  DAICD9        XXICD9
     C                   MOVEL(P)  DALOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD5
     C                   END
     C*** Beg Add *** 06/09/15 ****************************************
     C*
     C* ICD-10
     C*
     C                   Else
     C                   MoveL(P)  DAiCD1001     scICD5
     C                   MoveL(P)  DAlost        scLOS5
      *
     C                   MoveL(P)  DAiCD9        kkICD10
      *
     C     fmicd10l1key  Chain     pic01                              60
     C     *in60         IfEq      *off
     C                   MoveL(P)  ICdsc1        SCDGD5
     C                   EndIf
      *
     C                   EndIf
     C*** End Add *** 06/09/15 ****************************************
     C                   END
     C                   END                                                    @@RRN NE 0
      *
     C     @@ICD9        IFNE      *BLANK
     C                   MOVEL(P)  @@ICD9        SCICD5
     C                   MOVEL(P)  @@LOST        SCLOS5
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICD10 = @@icd9;
          Chain fmicd10l1key pic01;
          If %found(fmicd10l1);
             scDGd5 = ICdsc1;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  @@ICD9        XXICD9
     C                   MOVEL(P)  @@LOST        XXLOS
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDGD5
     C                   END
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   END                                                    @@ICD9 NE 0
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CURSOR IS NOT ON A SEARCHABLE FEILD
      *
     C                   OTHER
     C                   MOVE      *ON           *IN20
     C                   ENDSL
     C                   SETON                                        90
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   ENDSR                                                  KJDO
      *===============================================================
      * KNDO - SUBROUTINE FOR F14 = DARTS
      *===============================================================
      *
     C     KNDO          BEGSR
      *
      * CALL API TO ACCESS DARTS
      *
     C                   MOVE      XXCO#         XACO#
     C                   MOVE      XXSUB#        XASUB#
     C                   MOVE      XXFND         XAFND
     C                   MOVE      XXFYR         XAFYR
     C                   MOVE      XXCASE        XACASE
      *
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@WCIN
     C                   PARM                    @@WCDK
     C                   PARM                    XACO#             3
     C                   PARM                    XASUB#            5
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C                   PARM                    XACASE            7
     C                   PARM      *BLANK        ##KEY            40
      *
     C                   ENDSR
      *===============================================================
      * KPDO - SUBROUTINE FOR F15 = LOS - LENGTH OF STAY
      *                                   PAS GROUP SEARCH
      *===============================================================
      *
     C     KPDO          BEGSR
      *
     C     SCICD1        IFNE      *BLANKS
     C                   MOVE(P)   XXAGEC        QQAGEC            1
     C                   MOVE(P)   XXCAT         QQCAT             1
     C                   MOVEL(P)  SCICD1        QQICD             5
      *
     C                   CALL      'U0535R'
     C                   PARM                    QQPRDS           40
     C                   PARM      *BLANK        QQLPAS            6
     C                   PARM                    QQAGEC
     C                   PARM                    QQCAT
     C                   PARM                    QQICD
     C                   PARM      *BLANK        QQCERD            3
     C                   PARM      *BLANK        QQRTC             1
      *
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
      *
     C     QQRTC         IFEQ      'B'
     C     QQLPAS        OREQ      *BLANKS
     C                   MOVE      '1'           *IN25
     C                   MOVE      *ON           *INKL
     C                   END                                                    ENDEQ 'B'
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQLPAS        SCLPAS
     C                   MOVE      QQAGEC        XXAGEC
     C                   MOVE      QQCAT         XXCAT
     C                   MOVE      QQCERD        SCCERD
     C                   MOVE      '1'           *IN27
     C                   END                                                    ENDEQ '*'
      *
     C                   ELSE                                                   ELSE SCICD1
     C                   MOVE      '1'           *IN28
     C                   END                                                    END SCICD1
      *
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      *OFF          *IN33
     C                   MOVE      *ON           *IN33
     C     *IN40         IFEQ      *ON
     C                   MOVEL     MODEA         SCMODE                         ADD MODE
     C                   END
      *
     C     QQOCST        IFEQ      'R'
     C                   MOVEL(P)  OPTNMR        ##DATA                         RE-CERT
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA                         PRE-CERT
     C                   END
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      * PRE-CERT STATUS
      *
      *
      *  *IN54 = PROTECT INTERVIEWER, PLACE OF SERVICE, ADMIT DATE,
      *          PROVIDER/MOD
      *
     C                   MOVE      *OFF          *IN54
     C     @@MODE        IFEQ      'A'
     C     QQOCST        ANDEQ     'R'
     C                   MOVE      *ON           *IN54
      * RESET FILE
      *
     C                   SUB       1             XXEXT
     C     PCKY1         CHAIN     PPC01                              60
     C                   END
      *
     C     SCSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
      *
      * ONLY CALCULATE THE AGE GROUP CODE ON UPDATE IF A PASG IS NOT
      * PRESENT
      *
     C     PCLPAS        IFEQ      *BLANKS
      *
      * GET THE CLAIMANT'S AGE
      *
     C     XXDT          SUB       CDDTOB        XXAGE
      *
      * NOW CONVERT AGE TO LOS AGE CODE
      *
     C     XXYR          IFGT      0
     C     XXYR          ANDLE     19
     C                   MOVE      1             XXAGEC
     C                   END
      *
     C     XXYR          IFGT      19
     C     XXYR          ANDLE     34
     C                   MOVE      2             XXAGEC
     C                   END
      *
     C     XXYR          IFGT      34
     C     XXYR          ANDLE     49
     C                   MOVE      3             XXAGEC
     C                   END
      *
     C     XXYR          IFGT      49
     C     XXYR          ANDLE     64
     C                   MOVE      4             XXAGEC
     C                   END
      *
     C     XXYR          IFGT      64
     C                   MOVE      5             XXAGEC
     C                   END
      *
     C     XXAGEC        IFEQ      *ZEROS
     C                   MOVE      6             XXAGEC
     C                   END
      *
     C                   ELSE
     C                   Z-ADD     PCAGE         XXAGEC
     C                   END
     C                   END                                                    END *IN60
      *
     C                   MOVEL     QQOCST        SCSTAT
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'D'
     C                   MOVEL     PCSTAT        SCSTAT
     C                   END
      *
     C     SCSTAT        IFEQ      *BLANK
     C                   MOVEL     'O'           SCSTAT
     C                   END
      *
      *  LOAD THE PRE-CERT STATUS DESC
      *
     C                   MOVE      *BLANK        SCSTAD
      *
     C                   SELECT
     C     SCSTAT        WHENEQ    'O'
     C                   MOVEL     'Open   '     SCSTAD
     C     SCSTAT        WHENEQ    'R'
     C                   MOVEL     'Re-Cert'     SCSTAD
     C     SCSTAT        WHENEQ    'C'
     C                   MOVEL     'Closed '     SCSTAD
     C                   ENDSL
      *
      *  Obtain the Liability Code Description
      *
     C                   MOVEL     PCLIAB        SCLIAB
     C                   MOVE      *BLANKS       SCLBDS                         SCREEN LIABILITY
      *
     C                   SELECT
     C     SCLIAB        WHENEQ    'A'
     C                   MOVEL     LIAB(1)       SCLBDS
     C     SCLIAB        WHENEQ    'D'
     C                   MOVEL     LIAB(2)       SCLBDS
     C     SCLIAB        WHENEQ    'P'
     C                   MOVEL     LIAB(3)       SCLBDS
     C                   ENDSL
      *
      * INTERVIEWER
      *
     C     PCINTR        IFNE      *BLANK
     C                   MOVEL(P)  PCINTR        SCINTR
     C     UPKY1         CHAIN     PUP02                              60
     C                   MOVEL(P)  UPUSNM        SCINTD
      *
     C                   ELSE
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVEL(P)  UPSTAF        SCINTR
     C                   MOVEL(P)  UPUSNM        SCINTD
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * PLACE OF SERVICE
      *   GET FIRST 3 POSITIONS OF PC10A1
      *
     C     3             SUBST     PC10A1:1      ZZPOS
     C                   MOVE      ZZPOS         SCPOS
      *
      *   CLOSE FILE IF OPENED
      *
     C     OPNPS         IFEQ      '1'
     C                   CLOSE     FTPSL
     C                   MOVE      ' '           OPNPS
     C                   END                                                    END OPNPS=1
      *
      *   OVERRIDE TO FTPSL WITHOUT STATE
      *
     C     OVRPS         IFEQ      '1'
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRPS             1
     C                   END                                                    END OVRPS=1
      *
      *   OVERRIDE TO FTPSL WITH STATE
      *
     C     OVRPS         IFEQ      ' '
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(6)        @@CMD
     C                   MOVEL     XXST          @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRPS             1
     C                   END                                                    END OVRPS=0
      *
      *   OPEN FILE FOR PROCESSING
      *
     C     OPNPS         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FTPSL
     C                   MOVE      '1'           OPNPS
     C                   END                                                    END OPNPS= ' '
      *
     C                   MOVE      SCPOS         XXPOS
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PSDESC        SCPOSD
     C                   MOVEL     PSKEY         SCOPOS                         HIDDEN FIELD
     C                   END                                                    END *IN60
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  ADMIT DATE
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADDT
      *
      * ONCE ADMIT DATE IS ENTERED, IT MAY NOT BE CHANGED.
      *
     C                   MOVE      *OFF          *IN55
     C     @@MODE        IFEQ      'C'
     C     SCADDT        ANDNE     *ZERO
     C                   MOVE      *ON           *IN55
     C                   END
      *
      *  BILLING PHYSICIAN
     C                   MOVEL     PCFEIN        SCFEIN
     C                   Z-ADD     PCFEIN        XXFEIN
     C                   Z-ADD     PCMOD         SCMOD
     C                   Z-ADD     PCMOD         XXMOD
      *
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C                   Z-ADD     PDAREA        SCPRAC
     C                   MOVEL     PDPHN#        SCPPH1
     C                   MOVE      PDPHN#        SCPPH2
      *
     C     SCPRAC        IFEQ      *ZEROS
     C     SCPPH1        ANDEQ     *ZEROS
     C     SCPPH1        ANDEQ     *ZEROS
     C                   MOVE      '0'           *IN52
     C                   ELSE
     C                   MOVE      '1'           *IN52
     C                   END
      *
     C                   MOVEL(P)  PDNAME        XXLAST           56
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCPRNM
     C                   ELSE
     C                   MOVEL     PDNAME        SCPRNM
     C                   END                                                    END CDFSN
      *
     C                   END                                                    END *IN60
      *
      *  TREATING PHYSICIAN
     C                   MOVEL(P)  PCTPHY        SCTPHY
     C                   Z-ADD     PCTPHM        SCTPHM
     C                   Z-ADD     PCTPHY        XXFEIN
     C                   Z-ADD     PCTPHM        XXMOD
      *
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C                   Z-ADD     PDAREA        SCTPAC
     C                   MOVEL     PDPHN#        SCTPH1
     C                   MOVE      PDPHN#        SCTPH2
      *
     C     SCTPAC        IFEQ      *ZEROS
     C     SCTPH1        ANDEQ     *ZEROS
     C     SCTPH1        ANDEQ     *ZEROS
     C                   MOVE      '0'           *IN53
     C                   ELSE
     C                   MOVE      '1'           *IN53
     C                   END
      *
     C                   MOVEL(P)  PDNAME        XXLAST
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCTPNM
     C                   ELSE
     C                   MOVEL     PDNAME        SCTPNM
     C                   END                                                    END CDFSN
      *
     C                   END                                                    END *IN60
      *
      *  DIAGNOSIS CODES
      *
     C*** Beg Add *** 06/09/15 *********************************************
     C                   If        PCiCDInd <> 'Y'
     C*** End Add *** 06/09/15 *********************************************
     C                   MOVEL     PCICD1        SCICD1
     C                   MOVEL     PCICD2        SCICD2
     C                   MOVEL     PCICD3        SCICD3
     C                   MOVEL     PCICD4        SCICD4
     C                   MOVEL     PCICD5        SCICD5
     C*** Beg Add *** 06/09/15 *********************************************
     C                   EndIf
     C*** End Add *** 06/09/15 *********************************************
     C                   MOVEL     PCLOS1        SCLOS1
     C                   MOVEL     PCLOS2        SCLOS2
     C                   MOVEL     PCLOS3        SCLOS3
     C                   MOVEL     PCLOS4        SCLOS4
     C                   MOVEL     PCLOS5        SCLOS5
     C*** Beg Add *** 06/09/15 *********************************************
      /Free
         scICDind  = PCiCDind;
         If PCiCDInd = 'Y';
            sciCD1 = PCicd1001;
            sciCD2 = PCicd1002;
            sciCD3 = PCicd1002;
            sciCD4 = PCicd1002;
            sciCD5 = PCicd1002;
         Endif;
      /End-Free
     C*** End Add *** 06/09/15 *********************************************
      *
     C*** Beg Add *** 06/09/15 *********************************************
     C*
     C*   ICD-9 Processing
     C*
     C                   If        PCiCDInd <> 'Y'
     C*** End Add *** 06/09/15 *********************************************
     C                   DO        5             X                 2 0
     C                   MOVE      *BLANKS       XXICD9
     C*** Beg Chg *** 06/09/15 *********************************************
     C*****************  MOVE      DC(X)         XXICD9
     C                   MOVEL     DC(X)         XXICD9
     C*** End Chg *** 06/09/15 *********************************************
     C                   MOVEL     LOS(X)        XXLOS
     C     DIKEY         SETGT     PDI01
     C     NXTDC2        TAG
      *
     C                   READP     PDI01                                  60
     C     *IN60         IFEQ      '0'
      *
     C     DISTAT        IFNE      'I'
     C                   MOVEL     DIDSC1        DS(X)
     C                   ELSE
     C                   GOTO      NXTDC2
     C                   END                                                    END DISTAT
      *
     C                   ELSE                                                   *IN60 = 1
     C                   MOVE      *BLANK        DS(X)
     C                   END                                                    END *IN60
      *
     C                   ENDDO                                                  END DO
     C*** Beg Add *** 06/09/15 *********************************************
     C*
     C*   ICD-10 Processing
     C*
     C                   Else
     C*
     C                   DO        5             X                 2 0
     C                   MOVE      *BLANKS       kkICD10
     C                   MOVEL     DC(X)         kkICD10
     C     FMICD10L1Key  SETGT     PIC01
     C     NXTDC3        TAG
      *
     C                   READP     PIC01                                  60
     C     *IN60         IFEQ      '0'
      *
     C     ICSTAT        IFNE      'I'
     C                   MOVEL     ICDSC1        DS(X)
     C                   ELSE
     C                   GOTO      NXTDC3
     C                   END                                                    END DISTAT
      *
     C                   ELSE                                                   *IN60 = 1
     C                   MOVE      *BLANK        DS(X)
     C                   END                                                    END *IN60
      *
     C                   ENDDO                                                  END DO
      *
     C                   Endif                                                  END *IN60
     C*** End Add *** 06/09/15 *********************************************
      *
      * SCREEN 2
      *
      * LOAD SCREEN 2 FROM CERT/EXT
      *
     C                   Z-ADD     PCRVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRVDT
      **         QQOCST    IFNE 'R'
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     PCREQD        SCREQD
     C                   Z-ADD     PCCERD        SCCERD
     C                   MOVEL     PCLPAS        SCLPAS
     C                   Z-ADD     PCRECD        SCRECD
     C                   Z-ADD     PCCAT         XXCAT
     C                   MOVEL     PCLOST        XXLOST
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SAVINGS $ INFO
      *    WILL ONLY DISPLAY IF SFBPF:FBAL01 (POS 2) EQUALS 'Y' (*IN08).
      *
      * If Pre-Cert record, then use values
      *
     C     PCEXT         IFEQ      *ZERO
     C                   Z-ADD     PCAMTS        SCAMTS
     C                   Z-ADD     PCUSRS        SCUSRS
     C                   Z-ADD     PCDT1         SCDT1
     C                   MOVEL     PCCOMM        SCCOMM
     C                   END
      *
      * NOW WE MUST CALCULATE THE SYSTEM DAYS DEPENDING ON VALUE IN SYS FILE
      * ACCUMULATE THE TOTAL NUMBER OF DAYS THRU THE LAST PRE/RE-CERT
      *
     C                   Z-ADD     *ZEROS        XXTREC
     C                   Z-ADD     *ZEROS        XXTREQ
     C                   Z-ADD     *ZEROS        XXTCER
     C                   Z-ADD     *HIVAL        KKEXT
     C     PCKY3         SETGT     PPC01
     C     PCKEY         READPE    PPC01                                  60
      *
     C     *IN60         DOWEQ     '0'
     C     PCCERT        IFEQ      XXCERT
     C                   ADD       PCRECD        XXTREC
     C                   ADD       PCREQD        XXTREQ
     C                   ADD       PCCERD        XXTCER
     C                   END
     C     PCKEY         READPE    PPC01                                  60
     C                   ENDDO
      *
     C                   Z-ADD     XXTREC        SCTREC
      *
      * If Pre-Cert record, then use values
      *
     C     PCEXT         IFEQ      *ZERO
     C     ##SAVC        IFEQ      'L'
     C     XXTCER        SUB       XXTREC        SCCALS
     C                   ELSE
     C     XXTREQ        SUB       XXTREC        SCCALS
     C                   END                                                    END XXSAVC 'L'
      *
     C     SCCALS        IFLT      *ZEROS
     C                   Z-ADD     *ZEROS        SCCALS
     C                   END
     C                   END                                                    PCEXT = 0
      *
      * If Re-Cert record, then get values from Pre-Cert record
      *     after screen is filled with Re-Cert values
      *
     C     SCEXT         IFGT      000
     C                   Z-ADD     0             XXEXT
     C     PCKY1         CHAIN     PPC01                              60        W/EXTENSION
     C     *IN60         IFEQ      '0'                                          NOT VALID PRE-CERT
     C                   Z-ADD     PCAMTS        SCAMTS
     C                   Z-ADD     PCCALS        SCCALS
     C                   Z-ADD     PCUSRS        SCUSRS
     C                   Z-ADD     PCDT1         SCDT1
     C                   MOVEL     PCCOMM        SCCOMM
     C                   END                                                    END *IN60
      *
     C                   Z-ADD     SCEXT         XXEXT
     C                   END                                                    END PCEXT GT 000
      *
     C     @@MODE        IFEQ      'A'                                                           T
     C                   Z-ADD     ##DT03        SCAMTS
     C                   END
     C     CMKY1         CHAIN     PCM02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CMAST         XXAST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     STINST        ANDEQ     'Y'
     C*
     C                   MOVE      '0'           *IN98
     C                   MOVE      CMAST         XXST
     C*
     C                   ELSE
     C*
     C     P1KY1         CHAIN     PP101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   SETON                                        98
     C                   MOVEL(P)  P1ST          XXST
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
         //Beg Chg *** 08/05/13 ****************************************
         //scICDind= 'N';
         //If xxicd10 = *on;
         If xxicd10 = *on and PCicdind ='Y';
         //End Chg *** 08/05/13 ****************************************
            scICd1  = PCicd1001;
            //Beg Chg *** 08/05/13 ****************************************
              If PCicd1001 <> *Blanks;
                 KKICD10 = PCicd1001;
                 Chain fmicd10l1key pic01;
                    If %found(fmicd10l1);
                      scDGd1 = ICdsc1;
                    Endif;
              Endif;
              //End Chg *** 08/05/13 ****************************************
            scICd2  = PCicd1002;
              //Beg Chg *** 08/05/13 ****************************************
              If PCicd1002 <> *Blanks;
                 KKICD10 = PCicd1002;
                 Chain fmicd10l1key pic01;
                    If %found(fmicd10l1);
                      scDGd2 = ICdsc1;
                    Endif;
              Endif;
              //End Chg *** 08/05/13 ****************************************
            scICd3  = PCicd1003;
              //Beg Chg *** 08/05/13 ****************************************
              If PCicd1003 <> *Blanks;
                 KKICD10 = PCicd1003;
                 Chain fmicd10l1key pic01;
                    If %found(fmicd10l1);
                      scDGd3 = ICdsc1;
                    Endif;
              Endif;
              //End Chg *** 08/05/13 ****************************************
            scICd4  = PCicd1004;
              //Beg Chg *** 08/05/13 ****************************************
              If PCicd1004 <> *Blanks;
                 KKICD10 = PCicd1004;
                 Chain fmicd10l1key pic01;
                    If %found(fmicd10l1);
                      scDGd4 = ICdsc1;
                    Endif;
              Endif;

            scICd5  = PCicd1005;

              If PCicd1005 <> *Blanks;
                 KKICD10 = PCicd1005;
                 Chain fmicd10l1key pic01;
                    If %found(fmicd10l1);
                      scDGd5 = ICdsc1;
                    Endif;
              Endif;
              //End Chg *** 08/05/13 ****************************************
            scICDind= PCICDind;
         // Beg Add *** 08/05/13 ****************************************
         Elseif PCICDind ='N';
            scICdind = PCICDind;
         // End Add *** 08/05/13 ****************************************
         Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
      *
     C                   ENDSR
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     XXCO#         PCCO#
     C                   Z-ADD     XXSUB#        PCSUB#
     C                   Z-ADD     XXFND         PCFND
     C                   Z-ADD     XXFYR         PCFYR
     C                   Z-ADD     XXCASE        PCCASE
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BUILD PRE-CERT NUMBER
      *   GET THE FIRST 4 CHARS OF LAST NAME & MAKE SURE
      *     THEY ARE UPPER CASE.
      *
     C     QQOCST        IFEQ      *BLANK
     C                   MOVEL(P)  CDLSN         XXNAME
     C                   Z-ADD     1             P                 5 0
     C     XX1NME        LOOKUP    LOW(P)                                 61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *BLANKS       XX1NME
     C                   MOVE      CAP(P)        XX1NME
     C                   ELSE
     C*
     C     XX1NME        IFEQ      *BLANKS
     C*
     C                   MOVEL     '_'           XX1NME
     C*
     C                   ENDIF                                                  XX1NME EQ *BLANKS
     C                   END                                                    END 1 *IN61
     C                   Z-ADD     1             P
     C     XX2NME        LOOKUP    LOW(P)                                 61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *BLANKS       XX2NME
     C                   MOVE      CAP(P)        XX2NME
     C                   ELSE
     C*
     C     XX2NME        IFEQ      *BLANKS
     C*
     C                   MOVEL     '_'           XX2NME
     C*
     C                   ENDIF                                                  XX2NME EQ *BLANKS
     C                   END                                                    END 1 *IN61
     C                   Z-ADD     1             P
     C     XX3NME        LOOKUP    LOW(P)                                 61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *BLANKS       XX3NME
     C                   MOVE      CAP(P)        XX3NME
     C                   ELSE
     C*
     C     XX3NME        IFEQ      *BLANKS
     C*
     C                   MOVEL     '_'           XX3NME
     C*
     C                   ENDIF                                                  XX3NME EQ *BLANKS
     C                   END                                                    END 1 *IN61
     C                   Z-ADD     1             P
     C     XX4NME        LOOKUP    LOW(P)                                 61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *BLANKS       XX4NME
     C                   MOVE      CAP(P)        XX4NME
     C                   ELSE
     C*
     C     XX4NME        IFEQ      *BLANKS
     C*
     C                   MOVEL     '_'           XX4NME
     C*
     C                   ENDIF                                                  XX4NME EQ *BLANKS
     C                   END                                                    END 1 *IN61
      *
     C                   Z-ADD     SCADDT        XXADDT                         ADMIT DATE
      *
     C                   Z-ADD     99999         XXSEQ#
     C     XXCERT        SETGT     PPC02
     C                   READP     PPC02                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     P2CERT        HLCERT
     C                   ELSE                                                   *IN60 = 1
     C                   Z-ADD     0             HLADDT
     C                   Z-ADD     0             HLSEQ#
     C                   MOVE      *BLANKS       HLNAME
     C                   END                                                    END *IN60
      *
     C     HLNAME        IFEQ      XXNAME
     C     HLADDT        ANDEQ     XXADDT
     C     1             ADD       HLSEQ#        XXSEQ#
     C                   ELSE                                                   <> XX
     C                   Z-ADD     1             XXSEQ#
     C                   END                                                    END HL
      *
     C                   END                                                    QQOCST = BLANK
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   MOVEL(P)  XXCERT        PCCERT
     C                   Z-ADD     SCEXT         PCEXT
     C                   MOVEL     SCSS#         PCSS#
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCACDT
      *
     C                   MOVEL     Q1USER        PCIUSR
     C                   Z-ADD     XXDT          PCIDT
     C                   Z-ADD     XXDT          PCENTD                         DATE ENTERED
     C                   Z-ADD     DSTIME        PCITIM
     C                   END                                                    @@MODE=A
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SCREEN 1
     C                   MOVEL     SCSTAT        PCSTAT
     C                   MOVEL     SCLIAB        PCLIAB
     C                   MOVEL     SCINTR        PCINTR
     C                   MOVE      SCOPOS        PCPOS
     C                   MOVEL(P)  SCPOS         PC10A1
      *
     C                   Z-ADD     SCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCADDT
      *
     C                   Z-ADD     SCFEIN        PCFEIN
     C                   Z-ADD     SCMOD         PCMOD
     C                   Z-ADD     SCTPHY        PCTPHY
     C                   Z-ADD     SCTPHM        PCTPHM
      *
     C                   MOVEL     SCICD1        PCICD1
     C                   MOVEL     SCICD2        PCICD2
     C                   MOVEL     SCICD3        PCICD3
     C                   MOVEL     SCICD4        PCICD4
     C                   MOVEL     SCICD5        PCICD5
     C                   MOVEL     SCLOS1        PCLOS1
     C                   MOVEL     SCLOS2        PCLOS2
     C                   MOVEL     SCLOS3        PCLOS3
     C                   MOVEL     SCLOS4        PCLOS4
     C                   MOVEL     SCLOS5        PCLOS5
      * SCREEN 2
     C*
     C* CALL API TO WRITE DARTS RECORD
     C*
     C                   EXSR      WRDART
     C*
     C                   Z-ADD     SCRVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCRVDT
     C                   Z-ADD     SCREQD        PCREQD                         REQ DAYS
     C                   Z-ADD     SCCERD        PCCERD                         CERTIFIED DAYS
     C                   Z-ADD     SCRECD        PCRECD                         RECCOMMEND DAYS
     C                   MOVEL     SCLPAS        PCLPAS                         NEW LOS PAS GRP
     C                   MOVE      *BLANKS       PCPASG                         PAS GROUP
     C                   Z-ADD     XXAGEC        PCAGE                          LOS AGE GRP CD
     C                   Z-ADD     XXCAT         PCCAT
     C                   MOVEL     XXLOST        PCLOST
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SAVINGS INFO
      * If Pre-Cert record, then use values
      *
     C     PCEXT         IFEQ      000
     C                   Z-ADD     SCAMTS        PCAMTS
     C                   Z-ADD     SCUSRS        PCUSRS
     C                   Z-ADD     SCDT1         PCDT1
     C                   MOVEL     SCCOMM        PCCOMM
      * Else, if Re-Cert record, then zero/blank values
     C                   ELSE                                                   ELSE PCEXT = 000
     C                   Z-ADD     *ZEROS        PCAMTS
     C                   Z-ADD     *ZEROS        PCCALS
     C                   Z-ADD     *ZEROS        PCUSRS
     C                   Z-ADD     *ZEROS        PCDT1
     C                   MOVE      *BLANKS       PCCOMM
     C                   END                                                     END PCEXT = 000
      *
      * To determine the total saved days:
      * Accumulate the total number of days thru the last Pre/Re-Cert
      * Calculation based on value in System Control File
     C                   Z-ADD     *ZEROS        XXTREC
     C                   Z-ADD     *ZEROS        XXTREQ
     C                   Z-ADD     *ZEROS        XXTCER
     C                   MOVEL     PCCERT        XXCERT
      *
      * If on Add of this Pre/Re-Cert...then add in screen values
      *
     C     @@MODE        IFEQ      'A'
     C                   ADD       SCRECD        XXTREC
     C                   ADD       SCREQD        XXTREQ
     C                   ADD       SCCERD        XXTCER
     C                   END
      *
     C     PCKY2         CHAIN     PPC02                              61
     C     *IN61         DOWEQ     '0'
     C                   ADD       P2RECD        XXTREC
     C                   ADD       P2REQD        XXTREQ
     C                   ADD       P2CERD        XXTCER
     C     PCKY2         READE     PPC02                                  61
     C                   ENDDO
      *
      * If Pre-Cert record, then use values
     C     ##SAVC        IFEQ      'L'
     C     XXTCER        SUB       XXTREC        SCCALS
     C                   ELSE
     C     XXTREQ        SUB       XXTREC        SCCALS
     C                   END                                                    END XXSAVC 'L'
      *
     C     SCCALS        IFLT      *ZEROS
     C                   Z-ADD     *ZEROS        SCCALS
     C                   END
      *
     C     PCEXT         IFEQ      000
     C                   Z-ADD     SCCALS        PCCALS
     C                   END
      *
     C                   MOVEL     Q1USER        PCUSER
     C                   MOVEL     Q1PGM         PCPGM
     C                   Z-ADD     XXDT          PCDT
     C                   Z-ADD     DSTIME        PCTIME
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      * MOVE VALUES INTO HISTORY FILE FIELDS
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      XXCO#         CHCO#
     C                   MOVE      XXSUB#        CHSUB#
     C                   MOVE      XXFND         CHFND
     C                   Z-ADD     XXFYR         CHFYR
     C                   MOVE      XXCASE        CHCASE
     C                   MOVEL     XXCERT        CHCERT
     C                   Z-ADD     SCEXT         CHEXT
      *
     C                   Z-ADD     XXDT          CHEFDT
      *
     C                   MOVEL(P)  PCSTAT        CHOCST
      *
     C     PCSTAT        IFEQ      'O'
     C                   MOVEL(P)  XCOROP        CHEXPL
     C                   END
      *
     C     PCSTAT        IFEQ      'R'
     C                   MOVEL(P)  XCRCRT        CHEXPL
     C                   END
      *
     C                   MOVE      *BLANK        CHSTAT
     C                   MOVEL(P)  Q1USER        CHUSER
     C                   MOVEL(P)  Q1PGM         CHPGM
     C                   Z-ADD     XXDT          CHDT
     C                   Z-ADD     DSTIME        CHTIME
     C                   END
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
         PCICDind  = scICDind;
         If xxICD10 = '1' and scICdInd ='Y';
            PCicd1001 = scICd1;
            PCicd1002 = scICd2;
            PCicd1003 = scICd3;
            PCicd1004 = scICd4;
            PCicd1005 = scICd5;

             // If ICD10 is Active then no need of ICD9 fields

            PCICD1 = *Blanks;
            PCICD2 = *Blanks;
            PCICD3 = *Blanks;
            PCICD4 = *Blanks;
            //Beg Chg *** 08/05/13 ****************************************
            PCICD5 = *Blanks;
         Else;
            PCicd1001 = *Blanks;
            PCicd1002 = *Blanks;
            PCicd1003 = *Blanks;
            PCicd1004 = *Blanks;
            PCicd1005 = *Blanks;
            //End Chg *** 08/05/13 ****************************************
         Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
      *
     C                   ENDSR                                                  MVSCDB
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C*
     C     @@MODE        CASEQ     'S'           UPDATR
     C     @@MODE        CASEQ     'L'           DSPLYR
     C                   ENDCS
     C*
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
      * RESET ERROR INDICATORS
      *   SCREEN 1
     C                   MOVEA     '0'           *IN(14)                        *IN14
     C                   MOVEA     '00000'       *IN(20)                        *IN20 - 24
     C                   MOVEA     '0'           *IN(26)                        *IN26
     C                   MOVEA     '0'           *IN(28)                        *IN28
     C                   MOVEA     '0000000'     *IN(30)                        *IN30 - 36
     C                   MOVEA     '000'         *IN(37)                        *IN37 - 39
     C                   MOVEA     '00'          *IN(41)                        *IN41 - 42
     C                   MOVEA     '00000'       *IN(47)                        *IN47 - 51
     C                   MOVEA     '0000'        *IN(66)                        *IN66 - 69
     C                   MOVEA     '000000'      *IN(92)                        *IN92 - 97
      *   SCREEN 2
     C                   MOVEA     '0'           *IN(29)                        *IN29
     C                   MOVEA     '000'         *IN(44)                        *IN44 - 46
      *
     C                   MOVE      *ZERO         @@RRN1
     C                   MOVE      *ZERO         @@RRN3
      *
     C     *INKG         CASEQ     *ON           KGDO                     90     F7 = CASE PROV
     C     *INKH         CASEQ     *ON           KHDO                     90     F8 = BILL HIST
     C     *INKI         CASEQ     *ON           KIDO                     90     F9 = CLAIM INFO
     C     *INKK         CASEQ     *ON           KKDO                     90     F11 = CASE INFO
     C     *INKJ         CASEQ     *ON           KJDO                            F10 = SEARCH
     C     *INKN         CASEQ     *ON           KNDO                     90     F14 = DARTS
     C     *INKP         CASEQ     *ON           KPDO                     90     F15 = LOS
     C                   ENDCS
      *
     C                   EXSR      DATCVT
      *
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
      * If Re-Cert record just updated, then need to update
      *    Pre-Cert record with Calculated Saved Days value
     C     SCEXT         IFGT      000
     C                   Z-ADD     0             XXEXT
     C     PCKY1         CHAIN     PPC01                              60
     C     DBRRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     SCCALS        PCCALS
     C                   UPDATE    DBRCDF
     C                   END                                                    END 0 *IN60
     C                   Z-ADD     SCEXT         XXEXT
     C                   END                                                    SCEXT GT 000
      *
      *   CLOSE FILE IF OPENED
      *
     C     OPNPS         IFEQ      '1'
     C                   CLOSE     FTPSL
     C                   MOVE      ' '           OPNPS             1
     C                   END                                                    END OPNPS=1
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
     C                   SETON                                        LR
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C                   Z-ADD     2             #OFSCR            1 0          TWO SCREENS
     C                   MOVE      *ON           DSPTYP
     C                   MOVEL     *OFF          STOP              1
     C                   Z-ADD     *ZEROS        XXEXT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
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
     C                   PARM                    @@MSS             1            MS SUPPORT
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILLS
     C                   PARM                    @@NPWC            1            NON-PDS WC  LS
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C                   PARM      *BLANKS       ##DIFI            1            DIARY FUNCTION INST
     C                   PARM      *BLANKS       ##APRM            1            PROVIDER MAINT
     C                   PARM      *BLANKS       ##CLED            1            CLAIM EDIT INST
     C                   PARM      *BLANKS       ##DMEI            1            DME INSTALLED
     C                   PARM      *BLANKS       ##DLOS            1            DEFAULT LOS RGN
     C                   PARM      *BLANKS       ##PHPR            1            PHARMACY PROCESSING
     C                   PARM      *ZEROS        ##DT01            7 0
     C                   PARM      *ZEROS        ##DT02            7 0
     C                   PARM      *ZEROS        ##DT03            7 0
     C                   PARM      *ZEROS        ##DT04            7 0
     C                   PARM      *ZEROS        ##DT05            7 0
     C                   PARM      *BLANKS       ##SAV             1
     C                   PARM      *BLANKS       ##SAVC            1
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  WE MUST CHAIN OUT TO THE SYSTEM CONTROL FILE HERE TO SEE IF
      *  A VALID ADJUDICATION STATE IS INSTALLED.  IF WE FIND A STATE
      *  IN FBST01 WE WILL USE THAT STATE'S FILE FOR THE PROCEDURE
      *  CODE SEARCH.  IF WE DO NOT FIND A STATE IN THE CONTROL FILE
      *  WE WILL USE THE REASONABLE AND CUSTOMARY SEARCH.  VERIFICATION
      *  OF ENTERED CODES WILL BE HANDLED THE SAME WAY.
      *
      *
      **  LOAD '0' TO *IN98 REGARDLESS OF STATES INSTALLED FOR OHIO
      **  OHIO USES REASONABLE AND CUSTOMARY FILE, NOT CPT4
      *
      *
      * WE MUST CHECK TO SEE IF THE SAVING MODULE OF PRE-CERT IS INSTALLED
      *
     C     ##SAV         IFEQ      'Y'
     C                   MOVE      '1'           *IN08
     C                   ELSE
     C                   MOVE      '0'           *IN08
     C                   END                                                    END XXSAV
      *
      * WE MUST ALSO LOAD FIELD WITH CODE FOR SYSTEM CALCULATION FOR SAVED DAYS
      *
      *
      * INTERFACED WITH WCMS?
      *
     C                   MOVE      'N'           XXWPFL
     C     @@WCIN        IFEQ      'Y'
     C                   MOVE      'Y'           XXWPFL            1
     C                   END
      *
      * OUTSIDE ACCOUNT?
      *
     C     UPKEY         CHAIN     PUP01                              99
     C                   MOVE      UPCO#         @@CO#
     C                   MOVE      UPSUB#        @@SUB#
     C                   MOVE      UPFND         @@FND
     C                   MOVE      'N'           XXWCFL
      *
     C                   CALL      'FA701R'
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@OUTS            1
      *
     C                   SETOFF                                       18
      *
     C     @@OUTS        IFEQ      'N'
     C                   MOVE      'Y'           XXWCFL            1
     C*
     C*** CALL WCMS CLIAMS MASTER MAINTENANCE
     C*
     C     @@WCIN        IFEQ      'Y'
     C     @@WCIF        ANDEQ     'Y'                                          WC INTERFACE
     C     @@NPWC        ANDNE     'Y'                                          ADD 05/10/99
     C*
     C* IF GROUP IS LINKED WITH WCMS THEN OBTAIN WCMS GROUP NUMBER.
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      QQCO#         #1CO#
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM      QQSUB#        #1SUB#            5
     C                   PARM      QQFND         #1FND
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C*
     C                   CALL      'WA004R'                             99      CHG 05/10/99
     C                   PARM                    #1CO#             3
     C                   PARM      #1WCF#        #1FND             3
     C                   PARM      QQFYR         #1FYR             3
     C                   PARM      QQCASE        #1CASE            7
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C                   SETON                                        18
     C                   Z-ADD     ##RRN         ##RRN3
     C                   ENDIF                                                  ##RRN NE 0
     C                   ENDIF                                                  ##RTCD EQ ' '
     C                   ENDIF                                                  ##WCIN EQ 'Y'
     C                   END
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI                          CURRENT DATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
      * INITIALIZE ALL SCREEN FIELDS
      *
     C                   EXSR      INIT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET INFO FOR HEADING
      *   SUB,GROUP,YEAR,CASE
      *
      * ADD
     C     @@RRN         IFEQ      *ZERO
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C     CMKY1         CHAIN     PCM02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CMSS#         XXSS#
     C                   Z-ADD     CMACDT        XXACDT
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMFYR         SCYR
     C                   Z-ADD     CMFYR         SCYR2
     C                   Z-ADD     CMCASE        SCCASE
     C                   MOVEL     CMSS#         SCSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   END                                                    END *IN99
      *
     C                   END                                                    @@RRN = ZERO
      *   UPDATE
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     PCCO#         XXCO#
     C                   Z-ADD     PCSUB#        XXSUB#
     C                   Z-ADD     PCFND         XXFND
     C                   Z-ADD     PCFYR         XXFYR
     C                   Z-ADD     PCCASE        XXCASE
     C                   MOVEL     PCCERT        XXCERT
     C                   Z-ADD     PCEXT         XXEXT
     C                   MOVEL     PCSS#         XXSS#
     C                   Z-ADD     PCACDT        XXACDT
     C                   Z-ADD     PCSUB#        SCSUB#
     C                   Z-ADD     PCFND         SCFND
     C                   Z-ADD     PCFYR         SCFYR
     C                   Z-ADD     PCFYR         SCYR
     C                   Z-ADD     PCFYR         SCYR2
     C                   Z-ADD     PCCASE        SCCASE
     C                   MOVEL     PCCERT        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C                   MOVEL     PCSS#         SCSS#
     C                   Z-ADD     PCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      PCCO#         QQCO#
     C                   MOVE      PCSUB#        QQSUB#
     C                   MOVE      PCFND         QQFND
     C                   MOVE      PCFYR         QQFYR
     C                   MOVE      PCCASE        QQCASE
     C                   END                                                    END *IN99
     C                   END                                                    @@RRN <> ZERO
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL API TO CREATE A TWO LINE HEADING
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C     SCL1HD        PARM      *BLANK        QQL1             78
     C     SCL2HD        PARM      *BLANK        QQL2             78
      *
     C                   SELECT
      * ADD
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      *ON           *IN40
      *
      * CALLED FROM U0060R
     C     QQOCST        IFEQ      *BLANK
     C                   MOVE      *BLANK        QQCERT
     C                   MOVE      *BLANK        QQEXT
     C                   MOVEL(P)  'A'           ##MODE
     C                   MOVEL     'O'           ##OCST
     C                   CALL      'U0063R01'    ACDPL2
     C                   END
      * RE-CERT
      * CALLED FROM U0062R
     C     QQOCST        IFEQ      'R'
     C                   MOVEL(P)  QQCERT        SCCERT
     C                   MOVEL(P)  QQCERT        XXCERT
     C                   MOVEL(P)  'A'           ##MODE
     C                   MOVEL(P)  'R'           ##OCST                         RECERT
     C                   CALL      'U0063R01'    ACDPL2
      *
     C     QQCERT        SETGT     PPC02
     C                   READP     PPC02                                  60
     C     *IN60         IFEQ      '0'
     C     1             ADD       P2EXT         XXEXT
     C                   ELSE
     C                   Z-ADD     1             XXEXT
     C                   END                                                    END *IN60
     C                   Z-ADD     XXEXT         SCEXT
     C                   END
      **
      * SELECT
     C     @@MODE        WHENEQ    '2'
     C                   MOVEL(P)  MODED         SCMODE                         SELECT
     C                   MOVE      *ON           *IN05
      **
      * DISPLAY
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL(P)  PCCERT        XXCERT
     C                   MOVE      PCEXT         XXEXT
     C                   MOVEL(P)  PCCERT        SCCERT
     C                   MOVE      PCEXT         SCEXT
     C                   ENDSL
      *
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   END
     C     SCEXT         IFEQ      *ZEROS
     C*
     C                   SETOFF                                       16
     C*
     C                   ELSE
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*** Beg Add *** 05/23/13 ****************************************
      /Free

          // Call System API to check wheather the ICD10 is active or not

         @@rtnc = *blanks;
         Callp SA015R(@@rtnc);

         Select;
          // ICD10 is inactive
         When @@rtnc = '1';
         xxicd10 = '0';
         dspICdind = '1';

          //ICD10 is active
         When @@rtnc = '2';
         xxicd10 = '1';
         dspICdind = '0';
         // Beg Add *** 08/05/13 ****************************************
         scICdInd ='Y';
         // End Add *** 08/05/13 ****************************************
         Endsl;

         If @@MODE = 'D';
            DspicdInd = '1';
         Endif;

      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C                   ENDSR                                                  PGMSTR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C     @@MODE        IFNE      'A'
     C     @@MODE        ANDNE     'C'
     C     @@MODE        ANDNE     'D'
     C     @@MODE        ANDNE     'E'
     C*
     C     @@MODE        COMP      'L'                                    09
     C*
     C     STOP          IFEQ      *OFF
     C*
     C                   Z-ADD     2             #OFSCR
     C*
     C                   MOVEL     *ON           STOP
     C*
     C                   EXSR      MVDBSC
     C*
     C     @@MODE        CASEQ     'S'           XSCR02
     C     @@MODE        CASEQ     'L'           XSCR02
     C                   ENDCS
     C*
     C     OPNPS         IFEQ      '1'
     C*
     C                   CLOSE     FTPSL
     C                   MOVE      ' '           OPNPS             1
     C*
     C                   ENDIF                                                  END OPNPS=1
     C*
     C                   RETURN
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      *================================================================
      * UPDATR - Update data base record after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
      *
     C                   EXSR      MVSCDB
      *
     C                   UPDATE    DBRCDF
     C                   END                                                    END 0 *IN60
      *
      * IF IN CHG MODE, CALL THE NEXT PROGRAM
      *
     C     QQCONT        IFEQ      'Y'
     C                   EXSR      CALLR
     C                   END
      *
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
      *
     C                   EXSR      MVSCDB
      *
     C                   WRITE     DBRCDF
     C                   FEOD      UMPCP
     C                   MOVEL     *ON           OUTFLG
      *
      * WRITE HISTORY RECORD
      *
     C                   WRITE     PCH01
     C*
     C* GET THE NEXT AVAILABLE EOB# FROM CO AUXILLARY FILE
     C*
     C     C1KY          CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C     C1EOB#        IFEQ      *HIVAL
     C                   Z-ADD     1             C1EOB#
     C                   ELSE
     C                   ADD       1             C1EOB#
     C                   END
     C                   UPDATE    PC101
     C                   ELSE
     C                   Z-ADD     *HIVAL        C1EOB#
     C                   END
     C*
     C*
     C* MOVE FIELDS INTO PARM FIELDS FOR F15 TIME/MATERIALS CALL
     C*
     C*
     C*
     C**MOVE FIELDS INTO PARM FIELDS FOR F15 TIME/MATERIALS CALL
     C*
     C*
     C*
     C*
     C*
     C*
     C*
      *
     C*
     C*  WRITE THE EOMB AUXILARY FILE
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C     @@PRVB        IFEQ      'Y'
     C*
     C                   MOVE      'W'           XXFAEA
     C                   MOVE      'W'           XXFAEO
     C*
     C                   ELSE
     C*
     C                   MOVE      'W'           XXFAEO
     C                   MOVE      'W'           XXNAPB
     C                   MOVE      'W'           XXFAE1
     C*
     C                   ENDIF
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN2        @@RRN             9 0          RRN OF CASE
     C                   PARM      C1EOB#        @@EOB#            9 0          EOB #
     C                   PARM      *ZEROS        @@RCN             3 0          RECON #
     C                   PARM      Q1PGM         @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C                   PARM      'H'           @@BHRF            1            BRS INV H/R FLAG
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C                   PARM      'U'           @@SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    @@TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    @@TMAA            9 2          TIME/MAT AFTER
     C                   PARM      PCCERT        @@CERT           15            PRE-CERT
     C                   PARM      PCEXT         @@EXT             3 0          EXTEMSION
     C                   PARM      PCFEIN        @@FEIN            9 0          PROVIDER FEIN
     C                   PARM      PCMOD         @@MOD             3 0          PROVIDER MOD
     C                   PARM      *ZEROS        @@BIDT            7 0          BRS INV DT
     C*
     C*
     C* MOVE FIELDS FOR WRITE TO NAPBP FILE
     C*
     C*
     C*
      *
      * IF IN ADD MODE, CALL THE NEXT PROGRAM
      *
     C     QQCONT        IFEQ      'Y'
     C                   EXSR      CALLR
     C                   END
      *
     C                   ENDSR
      *
     C*================================================================
     C* WRDART - Write DARTS
     C*================================================================
     C     WRDART        BEGSR
     C*
     C* IF REVIEW DATE IS CHANGED (ADDED OR CHANGED),
     C* THEN AUTO DIARY WITH REMARK CODE PRE-DETERMINED BY 'UR SERVICE
     C* CODE' TO INTERVIEWER.
     C*
     C                   Z-ADD     SCRVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXRVDT
     C*
     C     XXRVDT        IFNE      PCRVDT
     C*
     C* LOAD PARM FOR DARTS CALL
     C*
     C* GET REMARK CODE DESCRIPTION
     C*
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CXRVDT        XXRMCD
     C     XXRMCD        CHAIN     STCTL1                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTDESC        QQCOMM
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Get Review Date (8 digits)
     C*
     C                   Z-ADD     SCRVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTMP1            7
     C*
     C                   MOVE      XXTMP1        SRDTI                          REVIEW DATE
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQCDAT
     C*
     C*  Program to call to edit Pre-Cert record
     C*
     C                   MOVEL(P)  'U0065R'      QQDCID
     C*
     C                   MOVEL(P)  'U0060R'      QQPNAM
     C                   MOVE      *ZERO         QQSECL
     C*
     C* FROM INTERVIEWER STAFF CODE, GET USER ID
     C*
     C     SCINTR        CHAIN     PUP02                              60
     C                   MOVEL(P)  UPUSR         QQUSER
     C*
     C                   MOVEL(P)  Q1PGM         QQPGM
     C*
     C* CALL API TO BUILD DARTS KEY
     C*
     C                   MOVE      XXCO#         XACO#
     C                   MOVE      XXSUB#        XASUB#
     C                   MOVE      XXFND         XAFND
     C                   MOVE      XXFYR         XAFYR
     C                   MOVE      XXCASE        XACASE
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           @@RTCD
     C                   PARM                    @@WCIN
     C                   PARM                    @@WCDK
     C                   PARM                    XACO#
     C                   PARM                    XASUB#
     C                   PARM                    XAFND
     C                   PARM                    XAFYR
     C                   PARM                    XACASE
     C                   PARM      *BLANK        ##KEY
     C*
     C                   MOVEL(P)  ##KEY         QQFOLD
     C*
     C                   MOVE(P)   'Y'           QQDIFL
     C                   MOVEL(P)  Q1PGM         QQIPGM
     C                   MOVE      'Y'           QQFL01                         DOCID = PROGRAM
     C                   MOVEL(P)  'N'           QQFL02
     C                   MOVEL(P)  XXRMCD        QQRMCD
     C*
     C     @@RTCD        IFEQ      'W'
     C                   CAT       'WC':0        QQPNAM
     C                   ENDIF
     C*
     C* CALL API TO WRITE DARTS RECORD
     C*
     C                   CALL      'DA700R'
     C                   PARM                    ##KEY            40
     C                   PARM                    QQCOMM          256
     C                   PARM                    QQCDAT            8
     C                   PARM                    QQDCID           12
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQSECL            3
     C                   PARM                    QQUSER           10
     C                   PARM                    QQPGM            10
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQDIFL            1
     C                   PARM                    QQIPGM           10
     C                   PARM                    QQFL01            1
     C                   PARM                    QQFL02            1
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQSEQ#            5
     C*
     C                   ENDIF                                                  SCRVDT NE PCRVD
     C*
     C                   ENDSR
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FTPSL)
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
**  LOW - LOWER CASE VALUES vs. CAP UPPER CASE VALUES
aA
bB
cC
dD
eE
fF
gG
hH
iI
jJ
kK
lL
mM
nN
oO
pP
qQ
rR
sS
tT
uU
vV
wW
xX
yY
zZ
**  LIAB - LIABILITY CODE DESCRIPTIONS
Approved
Denied
Pended
