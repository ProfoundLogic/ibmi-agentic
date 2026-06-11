     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE W0081R - WCMC Check Issuance
     H*****************************************************************
     H* Description:  This program creates the payment request trans- *
     H*   actions for EOMBs.  Up to 3 records can be created, these   *
     H*   are; the provider payment, the bill review service payment, *
     H*   and the network payment.  This program only processes one   *
     H*   EOMB at a time (per call).                                  *
     H*                                                               *
     H* Called from:  F7071R1                                         *
     H* Calls to:     none                                            *
     H*                                                               *
     H* Created by:  C:15     Project: 48MBV      12/01/98            *
     H*****************************************************************
     H* NOTE: IF INSTALLING THIS PROGRAM FROM V10, INSTALL WA023R3 AND*
     H*       WMEFP/L*. IF INSTALLING AT EFT CLIENT, ENTIRE WC1806    *
     H*       MAY BE REQUIRED!                                        *
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 10/23/24 C:342 Project: 146WKV                                *
     H*                No Network Fee Checks for Duplicate EOBs       *
     H*                                                               *
     H* 09/22/23 C:62  Project: 146MKP                                *
     H*                Add new processing for other 2 fees            *
     H*                                                               *
     H* 06/20/19 C:62  Project: 152MAV                                *
     H*                Add Negative Fee Check Processing.             *
     H*                                                               *
     H* 03/21/19 C:180 Project: 146MIZ Issue check request for % of   *
     H*                fee schedule changes.                          *
     H*                                                               &
     H* 09/05/18 C:79  Support: 146/73185                             *
     H*                Lookup recon received date using bill status R *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 12/06/16 C:136 Support: 44582                                 *
     H*                Pass ##key through GetEftStatus instead of     *
     H*                #ekey. #ekey wasn't being populated.           *
     H*                Changes not commented.                         *
     H*                                                               *
     H* 07/23/15 C:136 Support: 36153                                 *
     H*                Added bank code to GetEftStatus procedure      *
     H*                                                               *
     H* 07/11/14 C:279 Project: 111WBL (Back to Bas project 111MAI)   *
     H*                -KEEP FROM ISSUING PROVIDER CHECKS.            *
     H*                -CHANGE HOW STATUS AND OTHER FIELDS POPULATED. *
     H*                -REFERENCE FAESL                               *
     H*                                                               *
     H* 07/02/14 C:062 Support: WC129 86059                           *
     H*                Rename all fields in WARPLC                    *
     H*                                                               *
     H* 07/05/13 C:062 Support: MC146 82770                           *
     H*                Populate transaction code and reserve info.    *
     H*                                                               *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                1) Use GetBankcode instead of WA023R1          *
     H*                2) Use GetEFTStatus instead of WA023R3         *
     H*                3) Use New added field RPEFTC instead of       *
     H*                   RPCKTP.                                     *
     H*                                                               *
     H* 04/05/12 C:180 SUPPORT 79676
     H*                Addt'l changes to PV1 error - Trans code
     H*                logic from 10/14/10 not correct.
     H*
     H*
     H* 03/16/12 C:180 SUPPORT 79508
     H*                Fix blank PV1 error.
     H*
     H* 11/30/11 C:062 Support: 78365                                 *
     H*                Don't use EATRCD for fee checks.               *
     H*                                                               *
     H* 10/14/10 C:190 Project:  WC1953                               *
     H*                Change Process so that it will get the correct *
     H*                payment values, etc from the check issuance    *
     H*                file for Network (type = N) based on the policy*
     H*                eff date and claim jurisdiction state = CA.    *
     H*                                                               *
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date.                                   *
     H*                                                               *
     H* 11/09/09 C:220 Project: 136MEP                                *
     H*                Negative fee adjust/ReConsiderations           *
     H*                                                               *
     H* 08/06/09 C:062  Project: MC129 69398                          *
     H*                 ALLOW GROUP 50 TO ISSUE CHECK                 *
     H*                                                               *
     H* 06/29/09 C:191  Project: WC1806                               *
     H*                 UPDATE CALL TO WA023R3                        *
     H*                                                               *
     H* 06/24/08 C:062  Project: MC129 67283                          *
     H*                 ALLOW GROUP 1 TO ISSUE CHECK                  *
     H*                                                               *
     H* 06/24/08 C:191  Project: WC1453 PRISS #08                     *
     H*                 Check and send Pre-note transaction           *
     H*                                                               *
     H* 04/01/08 C:191 Project: 139MIV                                *
     H*                For client#139, write record if RPAMT is -ve   *
     H*                                                               *
     H* 01/22/08 C:47  Project: SEQ61834                              *
     H*                PAYMENT SURCHARGE IS BEING  PAID TO THE
     H*                WRONG PAYEE.
     H*                                                               *
     H* 08/09/07 C:191 Project: WC1453                                *
     H*                Change to pass the RPFEIN, RPMOD and RPDSQ#    *
     H*                to WA023R1 in the "Data" field.                *
     H*                                                               *
     H* 07/31/07 C:193 PROJECT: 129MPO                                *
     H*                REMOVE CHARGES ON RECONSIDERATIONS             *
     H*                                                               *
     H* 01/25/07 C:183 PROJECT: 114WLY                                *
     H*                Added subroutine to check for exact duplicate  *
     H*                payments. This subroutine is called for every  *
     H*                payment that is created. If the payment is d-  *
     H*                -etermined to be an exact duplicate, the pay-  *
     H*                -ment will be pended.                          *
     H*                                                               *
     H* 09/05/06 C:62  Project: 132MDM                                *
     H*                DO NOT AUTO RELEASE IF CLOSED DENIED AND AMOUNT*
     H*                GREATER THAN $250.                             *
     H*                                                               *
     H* 08/04/06 C:15  Project: 135WHU                                *
     H*                Added code to write a payment adjustment record*
     H*                if the BRS or Network amount < 0.              *
     H*                                                               *
     H* 01/11/06 C:15  Support: WC129/50722                           *
     H*                Added code to clear the out of country address *
     H*                screen fields to prevent invalid data from     *
     H*                being loaded into other WARPP records.         *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*                                                               *
     H*  12/08/04 C:62 PROJECT: NY007                                 *
     H*                -ADD PUBLIC GOODS PROCESS                      *
     H*                                                               *
     H*  06/05/03 C:62 PROJECT: 129MBO                                *
     H*                -ASSIGN RPRMDS WITH EACNTL                     *
     H*                                                               *
     H*  05/28/03 C:62 PROJECT: 129MBO                                *
     H*                -DON'T ISSUE BRS CHECK IF THE PRODUCE BRS      *
     H*                 FLAG EQUALS 'N'                               *
     H*                -DON'T ISSUE NWS CHECK IF THE PRODUCE NWS      *
     H*                 FLAG EQUALS 'N'                               *
     H*                                                               *
     H*  04/16/03 C:62 PROJECT: MCMNT                                 *
     H*                CHECK TO SEE IF PROV CHK IS ISSUED IN BATCH    *
     H*                                                               *
     H*  02/07/03 C:62 PROJECT: MCMNT 246                             *
     H*                -CHK TO SEE IF PROVIDER CHECK IS A GROUP CHECK.*
     H*                -CHANGE HOW STATUS AND OTHER FIELDS POPULATED. *
     H*                                                               *
     H* 04/25/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code for mult  *
     H*                bank account processing.                       *
     H*
     H*  09/28/00 C:62 PROJECT: MCMNT
     H*                ADD PROCESSING FOR "B" TYPES IN SRNTWK.
     H*                                                               *
     H*  02/25/00 C:15 Project: WC577                                 *
     H*                Added code to load new maint fields added to   *
     H*                WARPP.                                         *
     H*                                                               *
     H*  10/22/99 C:62 PROJECT: MC212                                 *
     H*                EAKY WAS USING THE WCMS CROSS REF GROUP        *
     H*
     H*  09/09/99 C:62 PROJECT: MC201
     H*                AUTO CHECK FOR NETWORK SERVICES.
     H*
     H*  07/27/99 C:62 PROJECT: MC201
     H*                REMOVE UPDATE OF FAEAP AND FAEOP.
     H*
     H*  03/23/99 C:94 PROJECT: MC194
     H*                LOAD RPPNAM, RPADR1, AND RPADR2 BASED ON
     H*                IF 'USED AS DBA' = Y OR N
     H*
     H*  03/18/99 C:94 PROJ: 25MKL
     H*                MOVE EORCN TO RPEOBS
     H*
     H*  03/12/99 C:94 PROJ:MC152
     H*                UPDATE EOSTAT WITH A 'P'
     H*
     H*  03/04/99 C:94 PROJ:MC152
     H*                CHANGE WHERE RPBR2 IS LOADED FOR BRS AND NWS
     H*                CHECKS
     H*
     H*  03/03/99 C:94 PROJ:MC152
     H*                ADD RCN # TO KEY
     H*
     H*  02/16/99 C:94 PROJ:25MKL                                     *
     H*                CHANGE FAEAL1 TO UPDATE EAAPPF AND EAEPFL      *
     H*                                                               *
     H*  01/29/99 C:94 PROJECT: 48MBV                                 *
     H*                CHANGE RPNTWK TO HOLD A 'B' IF A BRS CHECK     *
     H*                AND AND 'I' IF A NETWORK.                      *
     H*                                                               *
     H*****************************************************************
     H* Indicator usage:                                              *
     H*                                                               *
     H*  Note on *IN16, *IN17 and *IN18 with regard to Auto DARTS:    *
     H*  1) If *IN16 and *IN17 are *ON then Auto DARTS to W0070C50.   *
     H*  2) If *IN16 and *IN18 are *ON then Auto DARTS to F0350C.     *
     H*  *IN16  -  No provider payment. (RPAMT EQ ZERO).              *
     H*  *IN17  -  There is a BRS payment (RPAMT GT ZERO).            *
     H*  *IN18  -  No BRS payment (RPAMT EQ ZERO).                    *
     H*                                                               *
     H*  *IN60  -  General usage                                      *
     H*  *IN61  -  General usage                                      *
     H*****************************************************************
     FFAEAL1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFAEOL3    IF   E           K DISK
     FWMCIL     IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWARPL     IF A E           K DISK
     F                                     INFDS(DBFDS1)
     FWMCXL     IF   E           K DISK
     FWTPSL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F**          PB201                             KIGNORE
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
     FWMNRL     IF   E           K DISK
     FFAESL     IF   E           K DISK
     F*** Begin Chg *** 07/02/14 ****************************************
     F**********WARPLC    IF   E           K DISK
     F**********                           RENAME(PRP01:PRP03)
     FWARPLC    IF   E           K DISK    Prefix(R2:2) Rename(prp01:prp03)
     F*** End   Chg *** 07/02/14 ****************************************
     FWDPAL     IF A E           K DISK
     FWDRPL     O    E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWDELL     UF   E           K DISK
     FWMRHP     O    E             DISK
     FWMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     D*
     D*
     D DESC1           C                   CONST('Adjustment due to re-
     D                                     duced BRS fee')
     D DESC2           C                   CONST('Pool Department')
     D DBFDS1          DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  F1NW                   1     50
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
     D                 DS
     D  F1NF                   1     10
     D                                     DIM(10)
     D  F1NF01                 1      1
     D  F1NF02                 2      2
     D  F1NF03                 3      3
     D  F1NF04                 4      4
     D  F1NF05                 5      5
     D  F1NF06                 6      6
     D  F1NF07                 7      7
     D  F1NF08                 8      8
     D  F1NF09                 9      9
     D  F1NF10                10     10
     D                 DS
     D  F1NI                   1     90  0
     D                                     DIM(10)
     D  F1NI01                 1      9  0
     D  F1NI02                10     18  0
     D  F1NI03                19     27  0
     D  F1NI04                28     36  0
     D  F1NI05                37     45  0
     D  F1NI06                46     54  0
     D  F1NI07                55     63  0
     D  F1NI08                64     72  0
     D  F1NI09                73     81  0
     D  F1NI10                82     90  0
     D                 DS
     D  F1NM                   1     30  0
     D                                     DIM(10)
     D  F1NM01                 1      3  0
     D  F1NM02                 4      6  0
     D  F1NM03                 7      9  0
     D  F1NM04                10     12  0
     D  F1NM05                13     15  0
     D  F1NM06                16     18  0
     D  F1NM07                19     21  0
     D  F1NM08                22     24  0
     D  F1NM09                25     27  0
     D  F1NM10                28     30  0
     D                 DS
     D  F1N$                   1     90  2
     D                                     DIM(10)
     D  F1N$01                 1      9  2
     D  F1N$02                10     18  2
     D  F1N$03                19     27  2
     D  F1N$04                28     36  2
     D  F1N$05                37     45  2
     D  F1N$06                46     54  2
     D  F1N$07                55     63  2
     D  F1N$08                64     72  2
     D  F1N$09                73     81  2
     D  F1N$10                82     90  2
     D                 DS
     D  F1NT                   1     10
     D                                     DIM(10)
     D  F1NT01                 1      1
     D  F1NT02                 2      2
     D  F1NT03                 3      3
     D  F1NT04                 4      4
     D  F1NT05                 5      5
     D  F1NT06                 6      6
     D  F1NT07                 7      7
     D  F1NT08                 8      8
     D  F1NT09                 9      9
     D  F1NT10                10     10
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
     D  QXSEQ#                30     34  0
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D*
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
      //*** Begin Add *** 06/20/19 ****************************************
      /copy sPrModule
      //*** End   Add *** 06/20/19 ****************************************
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
     d ##Co#           s              3
     d ##Fnd           s              3
     d #1fnd           s              3
     d ##Fyr           s              3
     d ##Case          s              7
     d ##Appl          s              2
     d ##Data          s             75
     d ##Cktp          s             10
     d #rtcd           s              1
     d ##Eftc          s              1
     d ##Eft           s              1
     d ##Pre           s              1
     d ##ptyp          s              1
     d #ekey           s             30
     d ##Date          s              7
     d ##key           s             30
      *******End Add********04/17/12**********************************
      *** beg add ***** 07/23/15 *************************************
     d ##outRRN        s              9                                          07
     d ##EobType       s              9                                          07
      *** end add ***** 07/23/15 *************************************
     I*
     I*** Begin Del *** 07/02/14 ****************************************
     I**********PRP03
     I**********    RPRECD                      R2RECD
     I**********    RPFEIN                      R2FEIN
     I**********    RPFRDT                      R2FRDT
     I**********    RPTODT                      R2TODT
     I**********    RPAMT                       R2AMT
     I**********    RPSTAT                      R2STAT
     I**********    RPSEQ#                      R2SEQ#
     I**********    RPTYPE                      R2TYPE
     I*** End   Del *** 07/02/14 ****************************************
     C*=========================================================================
      *
     C     CXKEY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
      *
      *  REQUEST PAYMENT FILE - PARTIAL
      *
     C     RPKEY2        KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
      *
      *  PAYMENT DETAIL FILE - PARTIAL
      *
     C     PAKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
      *
     C*  Process the initialization subroutine
     C*
     C                   EXSR      SRINIT
     C*
     C*  Retrieve the EOMB information
     C*
     C     EOKY          CHAIN     PEO01                              60
     C   60              GOTO      TERM
     C                   MOVE      EORCN         XXRCN
     C*
     C     EAKY          CHAIN     PEA01                              60
     C   60              GOTO      TERM
     C*
     C*  Retrieve the Claim record
     C*
     C     CMKY          CHAIN(N)  PCM01                              60
     C   60              GOTO      TERM
     C*
     C*  Determine Next sequence number for payment requests
     C*
     C     RPKY          SETGT     PRP01
     C     RPKY          READPE(N) PRP01                                  60
     C   60              Z-ADD     0             RPSEQ#
     C*
     C*  Process the subroutines which create the various payments.
     C*
     C     @@PCIB        IFEQ      'Y'
      /free
       //******* beg add ********** 07/11/14 ********************************
       If @@clt# <> '111';
       //******* end add ********** 07/11/14 ********************************
      /end-free
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'N'
     C     EASYS         OREQ      'V'
     C*
     C                   EXSR      SRPROV
     C                   ENDIF
      /free
       //******* beg add ********** 07/11/14 ********************************
       Endif;
       //******* end add ********** 07/11/14 ********************************
      /end-free
     C                   ENDIF
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'U'
     C     EASYS         OREQ      'M'
     C                   EXSR      SRBRS
     C                   ENDIF
      /free
       //******* beg add ********** 07/11/14 ********************************
       If @@clt# <> '111';
       //******* end add ********** 07/11/14 ********************************
      /end-free
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'N'
       //******* Beg Chg ********** 10/23/24 ********************************
     C****               EXSR      SRNTWK
          If @@Clt# = '146' and EApaid = 0 and EAchrg = EAmra;
          Else;
             Exsr SRNtWk;
          Endif;
       //******* End Chg ********** 10/23/24 ********************************
     C                   ENDIF
      /free
       //******* beg add ********** 07/11/14 ********************************
       Endif;
       //******* end add ********** 07/11/14 ********************************
      /end-free
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'N'
     C     EASYS         OREQ      'V'
     C     FNKYWC        CHAIN     PFN02                              60
     C     *IN60         IFEQ      *OFF
     C     FNCGFL        IFNE      'Y'
     C     @@CLT#        OREQ      '129'
     C     FNFND         ANDEQ     1
     C     @@CLT#        OREQ      '129'
     C     FNFND         ANDEQ     50
     C                   EXSR      SRPGPC
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C******************************************************************
     C*  SRINIT - This subroutine is used to define and initialize
     C*    program variables.
     C******************************************************************
     C     SRINIT        BEGSR
     C*
     C*  KEY LIST FOR HEADER BILL AMOUNT FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN             3 0
     C*
     C     ESKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXIDT
     C*
     C     ESKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
     C*  Key list for the WCMC Group Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C*
     C*  Key list for the EOMB file
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXEOB#
     C*
     C*  Key list for the EOMB Auxiliary files
     C*
     C     EOKYA         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
     C*
     C*  Key list for the WCMC Check Issuance file
     C*
     C     CIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPTYP
     C                   KFLD                    XXSYS
     C*
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C*  Key list for the Request Payment file
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C*  Key list for the Provider file
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  NETWORK REVIEW ID FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C*  Key list for the WCMS table files
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     PAKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C     ELKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C     ELKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     FNKYWC        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@GRP#            3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@EOB#            9
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@NAMT
      *** Begin Add *** 03/21/19 **********************************************
      *** Begin Chg *** 09/22/23 *******************************************
     C**********         PARM                    @@MRA
     C                   PARM                    @@OCA2
      *** End   Chg *** 09/22/23 *******************************************
      *** End Add ***** 03/21/19 **********************************************
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
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANK        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXGRP#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    CIPTYP        XXPTYP
     C     *LIKE         DEFINE    CISYS         XXSYS
     C     *LIKE         DEFINE    PCKEY         XXKEY
     C     *LIKE         DEFINE    EANAMT        @@NAMT
      *** Begin Add *** 03/21/19 **********************************************
      *** Begin Del *** 09/22/23 *******************************************
     C******LIKE         DEFINE    EANAMT        @@MRA
      *** End   Del *** 09/22/23 *******************************************
      *** End   Add *** 03/21/19 **********************************************
      *** Begin Add *** 09/22/23 *******************************************
     C     *LIKE         DEFINE    EAOCA2        @@OCA2
      *** End   Add *** 09/22/23 *******************************************
     C     *LIKE         DEFINE    ESIDT         XXIDT
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    RPAMT         SVAMT
     C                   Z-ADD     1             X                 3 0
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@SUB#        XXSUB#
     C                   MOVE      @@GRP#        XXGRP#
     C                   MOVE      @@EOB#        XXEOB#
     C*
     C*  Retrieve the WCMS group #.
     C*
     C                   Z-ADD     0             XXFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FNWCF#        XXFND
     C                   ENDIF
     C*
     C*  Retrieve the Group Auxiliary record.
     C*
     C     FNKY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     *BLANKS       F1NW
     C                   MOVEA     *BLANKS       F1NF
     C                   MOVEA     *ZEROS        F1NI
     C                   MOVEA     *ZEROS        F1NM
     C                   MOVEA     *ZEROS        F1N$
     C                   MOVEA     *BLANKS       F1NT
     C                   ENDIF
     C                   CALL      'SA001R'
01   C                   PARM                    @@WCIF            1            WC INST
02   C                   PARM                    @@MCIF            1            MC INST
03   C                   PARM                    @@WCIN            1            WC INTERFACE
04   C                   PARM                    @@AR              1            MC AR INST
05   C                   PARM                    @@NTWK            1            NETWORK INST
06   C                   PARM                    @@UR              1            PRECERT INST
07   C                   PARM                    @@MED             1            MEDMGMT INST
08   C                   PARM                    @@EDI             1            EDI INST
09   C                   PARM                    @@WCDK            1            WC DARTS KEY
10   C                   PARM                    @@RHAB            1            REHAB INST
11   C                   PARM                    @@MSS             1            MS SUPPORT
12   C                   PARM                    @@FSCH            1            FEE SCHEDULE
13   C                   PARM                    @@PRVB            1            PROVIDER BILLS
14   C                   PARM      ' '           @@NPWC            1
15   C                   PARM      ' '           @@OS              1
16   C                   PARM      ' '           @@ARLS            1
17   C                   PARM      ' '           @@CCOD            1
18   C                   PARM      ' '           @@CRVW            1
19   C                   PARM      ' '           @@PROM            1
20   C                   PARM      ' '           @@MCRV            1
21   C                   PARM      ' '           @@FREQ            1
22   C                   PARM      ' '           @@CRAC            1
23   C                   PARM      ' '           @@PREO            1
24   C                   PARM      ' '           @@PARN            1
25   C                   PARM      ' '           @@MGRI            1
26   C                   PARM      ' '           @@KCIF            1
27   C                   PARM      ' '           @@CURS            1
28   C                   PARM      ' '           @@PCIB            1
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
     C                   ENDSR
     C******************************************************************
     C*  SRPROV - This subroutine creates the provider payment.
     C******************************************************************
     C     SRPROV        BEGSR
     C*
     C*  Retrieve the provider WCMC check issuance record.
     C*
     C                   MOVE      'P'           XXPTYP
     C                   MOVE      EOSYS         XXSYS
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C*
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   MOVE      EOFEIN        RPFEIN
     C                   MOVE      EOMOD         RPMOD
     C                   MOVE      EAPAID        RPAMT
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C     EOBTYP        IFEQ      'X'
     C                   MOVE      CIXBR2        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C                   MOVE      EOTRCD        RPTRCD
     c*** Begin Add *** 07/05/13 *******************************************
     c                   If        RPtrCd=' '
     C                   If        CMcvFl<>'Y'
     C                   Eval      RPtrCd=CItrCd
     C                   Else
     C                   Eval      RPtrCd=CItrCv
     C                   EndIf
     c                   EndIf
     c*** End   Add *** 07/05/13 *******************************************
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVE      EORMDS        RPRMDS
     C                   MOVE      EORMK1        RPRMK1
     C                   MOVE      EORMK2        RPRMK2
     C                   MOVE      EORMK3        RPRMK3
     C                   MOVE      EOPNAM        RPPNAM
     C                   MOVE      EOADR1        RPADR1
     C                   MOVE      EOADR2        RPADR2
     C                   MOVE      EOCTY         RPCTY
     C                   MOVE      EOST          RPST
     C                   MOVE      EOZIP         RPZIP
     C                   MOVE(P)   EOADR3        RPADR3
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      *BLANK        RPPGTP
     C     NRKY          CHAIN     PNR01                              99
     C     *IN99         IFEQ      *OFF
     C     NRSTAT        ANDNE     'I'
     C     NRGRTY        IFEQ      'O'
     C     NRGRTY        OREQ      ' '
     C                   MOVEL     'G'           RPPGTP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      EOIUSR        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C                   MOVE      *BLANK        RPOFF#
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      *BLANKS       RPDSQ#
     C                   MOVE      RPDSQ#        QXDSQ#
      *
      *******Beg Chg********04/17/12**********************************
     C**********         CALL      'WA023R1'
     C**********         PARM      '2'           ##MODE            1            Mode
     C**********         PARM                    ##CO#             3            Company
     C**********         PARM                    ##FND             3            Group #
     C**********         PARM                    ##FYR             3            Year
     C**********         PARM                    ##CASE            7            Case
     C**********         PARM      'WC'          ##APPL            2            Applic Code
     C**********         PARM      DATA          ##DATA           75            Selection criteria
     C**********         PARM                    ##CKTP           10            Bank Acct Code
     C**********         PARM                    ##RTCD            1            Return Code
     C*
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: ##appl:
                 ##data: #rtcd: ##eftc);
       RPeftc = ##eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
      *
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   EXSR      CHKEFT
     C                   MOVE      *BLANK        RPACCT
     C                   MOVE      EACKST        RPSTS
     c*** Begin Add *** 07/05/13 *******************************************
     c                   If        RPsts=' '
     C                   Eval      RPsts=CIsts
     c                   EndIf
     c*** End   Add *** 07/05/13 *******************************************
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVEL     EOIUSR        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
     C                   MOVE      *BLANKS       RPNTWK
     C                   Z-ADD     EORCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C     RPAMT         IFGT      0
     C                   EXSR      EFTSLT
     C                   ENDIF                                                  RPAMT GT 0
     C*
     C                   ENDIF                                                  *IN60 EQ *OFF
     C*
     C                   ENDSR
     C******************************************************************
     C*  SRBRS - This subroutine creates the bill review service pmt.
     C******************************************************************
     C     SRBRS         BEGSR
     C*
     C*  Retrieve the BRS WCMC check issuance record.
     C*
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      EOSYS         XXSYS
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C*
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   MOVE      CIFEIN        RPFEIN
     C                   MOVE      CIMOD         RPMOD
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C     @@CLT#        IFEQ      '129'
     C     EARCN         ANDGT     0
     C                   GOTO      NXTB
     C                   ENDIF
     C                   Z-ADD     EABAMT        RPAMT
     C                   ELSE
     C   60              GOTO      NXTB
     C                   ENDIF
     C*
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EATRCD        IFNE      *BLANKS
     C     @@CLT#        ANDNE     '146'
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CITRCV        RPTRCD
     C                   ELSE
     C                   MOVE      CITRCD        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C                   MOVEL     CIFEIN        XXFEIN
     C                   MOVEL     CIMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C   60              GOTO      NXTB
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
     C                   ELSE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
     C*
      ******* beg chg ********* 07/11/14 ***********************************
     C*******            MOVE      EAUSER        RPUSER
      /free
       If @@clt# <> '111';
          RPuser = EAuser;
       Else;
          RPrqst = EAuser;
       Endif;
      /end-free
      ******* end chg ********* 07/11/14 ***********************************
     C                   MOVEL     *BLANKS       RPCKCN
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EORCN         IFEQ      *ZEROS
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   ELSE
     C                   Z-ADD     *HIVAL        XXIDT
     C     ESKY          SETGT     PES01
     C     ESKY2         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C*** Beg Add *** 09/05/18 *********************************************
     C     ESBLST        ANDEQ     'R'
     C*** End Add *** 09/05/18 *********************************************
     C                   Z-ADD     ESRRDT        RPRCVD
     C*** Beg Add *** 09/05/18 *********************************************
     C                   ELSE
     C                   DOW       *IN99 = *OFF
     C     ESKY2         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C     ESBLST        ANDEQ     'R'
     C                   Z-ADD     ESRRDT        RPRCVD
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*** End Add *** 09/05/18 *********************************************
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     EOTRDT        RPRCVD
     C                   ENDIF
     C*
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      *BLANKS       RPDSQ#
     C                   MOVE      RPDSQ#        QXDSQ#
      *
      *******Beg Chg********04/17/12**********************************
     C**********         CALL      'WA023R1'
     C**********         PARM      '2'           ##MODE            1            Mode
     C**********         PARM                    ##CO#             3            Company
     C**********         PARM                    ##FND             3            Group #
     C**********         PARM                    ##FYR             3            Year
     C**********         PARM                    ##CASE            7            Case
     C**********         PARM      'WC'          ##APPL            2            Applic Code
     C**********         PARM      DATA          ##DATA           75            Selection criteria
     C**********         PARM                    ##CKTP           10            Bank Acct Code
     C**********         PARM                    ##RTCD            1            Return Code
      *
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: ##appl:
                 ##data: #rtcd: ##eftc);
       RPeftc = ##eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   EXSR      CHKEFT
     C                   MOVE      *BLANK        RPACCT
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EACKST        IFEQ      *BLANKS
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C     EACKST        IFEQ      'PY'
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C                   MOVE      EACKST        RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      CISTS         RPSTS
     C                   ENDIF
     C*
     C*
     C* CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
     C*
     C     @@CLT#        IFEQ      '132'
     C     RPAMT         IFGT      250
     C     CMDNY         OREQ      'Y'
     C     CMSTAT        OREQ      'C'
     C                   MOVEL(P)  'PR'          RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVEL     EAUSER        RPRQST
      /free
       //****** beg add ******** 07/11/14 ***************************************
       If @@clt# = '111';
          RPckcn = ' ';
       Endif;
       //****** end add ******** 07/11/14 ***************************************
      /end-free
     C                   MOVE      *BLANKS       RPLTCD
     C                   MOVE      'B'           RPNTWK
     C                   Z-ADD     EORCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C     RPAMT         IFGT      0
     C     EAPBRC        ANDNE     'N'
     C                   EXSR      EFTSLT
     C                   ENDIF                                                  RPAMT GT 0
     C     RPAMT         IFLT      0
     C     @@CLT#        ANDEQ     '135'
     C*
     C     PAKY1         SETGT     PPA01
     C     PAKY1         READPE(N) PPA01                                  60
     C  N60              Z-ADD     PASEQ#        XXSEQ#
     C   60              Z-ADD     0             XXSEQ#
     C                   CLEAR                   PPA01
     C*
     C     XXSEQ#        ADD       1             PASEQ#
     C*
     C                   Z-ADD     RPAMT         PAAMT
     C                   Z-ADD     EOCO#         PACO#
     C                   Z-ADD     EOFND         PAFND
     C                   Z-ADD     EOFYR         PAFYR
     C                   Z-ADD     EOCASE        PACASE
     C                   Z-ADD     0             PACHK#
     C                   Z-ADD     0             PABASQ
     C                   Z-ADD     RPFEIN        PAFEIN
     C                   Z-ADD     RPMOD         PAMOD
     C*
     C                   Z-ADD     RPDT          PATRDT
     C                   Z-ADD     RPDT          PAPODT
     C                   Z-ADD     RPFRDT        PAFRDT
     C                   Z-ADD     RPTODT        PATODT
     C                   Z-ADD     0             PAWKS
     C                   Z-ADD     0             PADAYS
     C*
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
     C*
     C     *LOVAL        SETLL     PPC01
     C                   READ      PPC01                                  60
     C     *IN60         DOWEQ     '0'
     C     PCRECD        IFEQ      RPRECD
     C     PCTRTP        ANDEQ     'A'
     C                   LEAVE
     C                   ENDIF
     C                   READ      PPC01                                  60
     C                   ENDDO
     C*
     C  N60              MOVEL     PCKEY         PATRCD
     C   60              MOVE      RPTRCD        PATRCD
     C                   MOVEL     DESC1         PADESC
     C                   MOVE      'A'           PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
     C*                    MOVE *BLANK    PAADST
     C                   Z-ADD     RPEOB#        PAEOB#
     C                   Z-ADD     RPEOBS        PAEOBS
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   Z-ADD     RPDT          PADT
     C                   Z-ADD     RPTIME        PATIME
     C                   MOVEL     Q1USER        PAIUSR
     C                   MOVEL     Q1PGM         PAIPGM
     C                   Z-ADD     RPDT          PAIDT
     C                   Z-ADD     RPTIME        PAITIM
     C*
     C                   WRITE     PPA01
     C*
     C*  CREATE REQUEST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   MOVE      *BLANKS       RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   WRITE     PRP02
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C     PAKY1         CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  60
     C*
     C     CMSTAT        IFEQ      'C'
     C     PAAMT         ORGT      RR(X)
     C                   EXSR      RSVHST
     C                   ELSE
     C                   SUB       PAAMT         RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C                   ADD       PAAMT         AR(X)
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*   RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     CMSTAT        IFEQ      'O'
     C                   SUB       PAAMT         ELOUTS
     C                   ENDIF                                                  CMSTAT=O
     C                   ADD       PAAMT         ELTCLP
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF
     C*
     C                   ENDIF                                                  EABAMT < 0
     C                   ENDIF                                                  *IN60 EQ *OFF
     C*
     C     NXTB          TAG
     C*
     C                   ENDSR
     C******************************************************************
     C*  SRNTWK - This subroutine creates the network payment.
     C******************************************************************
     C     SRNTWK        BEGSR
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C*
     C**N61*****           GOTO NXTC
     C*
     C*
     C*  Retrieve the Network WCMC check issuance record.
     C*
     C*
     C*
     C*
     C*
     C*
     C                   MOVEL     EONTWK        XXNWCD            5
     C*
     C                   Z-ADD     1             X
      *** Begin Add *** 03/21/19 *********************************************
     C     NxtNwCd       TAG
      *** End   Add *** 03/21/19 *********************************************
     C     XXNWCD        LOOKUP    F1NW(X)                                61
     C*
     C  N61              GOTO      NXTC
     C*
     C* IF IN-HOUSE NETWORK:
     C*
B01  C     F1NF(X)       IFEQ      'I'
     C*
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      'N'           XXSYS
     C*
     C* RETRIEVE WCMC CHECK ISSUANCE INFORMATION:
     C*
     C     CIKY          CHAIN     PCI01                              60
     C*
     C   60              GOTO      NXTC
     C*
     C                   Z-ADD     CIFEIN        RPFEIN
     C                   Z-ADD     CIMOD         RPMOD
     C*
     C* RETRIEVE BILLING AMOUNT:
     C*
     C*
     C   60              GOTO      NXTC
     C                   Z-ADD     EANAMT        RPAMT
     C*
     C* RETRIEVE BILLING AMOUNT:
     C*
S01  C                   ELSE
     C*
     C* RETRIEVE WCMC CHECK ISSUANCE INFORMATION:
     C*
     C                   MOVEL     'N'           XXPTYP
     C                   MOVEL     EOSYS         XXSYS
     C*
     C     CIKY          CHAIN     PCI01                              60
     C*
     C   60              GOTO      NXTC
     C*
     C* ESTABLISH PAYEE FEIN/MOD
     C*
     C                   Z-ADD     F1NI(X)       RPFEIN
     C                   Z-ADD     F1NM(X)       RPMOD
     C*
     C* CALCULATE OUTSIDE NETWORK FEE:
     C* IF OUTSIDE NETWORK, DISCOUNT CALCULATED OUTSIDE, THEN USE FEE
     C* STORED IN FAEAP/EANAMT
     C*
B02  C     F1NF(X)       IFEQ      'Y'
     C*
     C                   Z-ADD     EANAMT        RPAMT
     C*
S02  C                   ELSE
     C*
     C* IF OUTSIDE NETWORK, DISCOUNT CALCULATED IN-HOUSE, THEN
     C* CALCULATE FEE AND STORE IN EANAMT, CALCULATION BASED ON "TYP"
     C* IN GROUP SETUP:
     C*      S = % OF NETWORK SAVINGS (WHERE "FEE AMOUNT" = X)
     C*      F = % OF NEWORKD SAVINGS (Based on % of MRA savings)
     C*      B = FLAT RATE(WHERE "FEE AMOUNT" = FLAT RATE)
     C*      N = N/A (NO BILL RATE)
     C*
B03  C     F1NT(X)       IFEQ      'B'
     C*
     C                   Z-ADD     F1N$(X)       RPAMT
     C*
S03  C                   ELSE
     C*
      *//*** Begin  Change *** 03/21/19 ***********************************************
B04  C***     F1NT(X)       IFEQ      'S'
     C***
     C***     F1N$(X)       DIV       100           XXPERC            9 2
     C***     XXPERC        MULT      EANDIS        RPAMT
     C***                   ELSE
     C***
     C***                   Z-ADD     *ZEROS        RPAMT
     C***
E04  C***                   ENDIF
      /Free
        Select;
        When F1NT(X) ='S';
           RPAMT = (F1N$(X)/100) * EAndis;
        When F1NT(X) ='F';
           RPAMT = (F1N$(X)/100) * EAmra;
        //*** Begin Add *** 09/22/23 **********************************
        When F1NT(X) ='2';
           RPAMT = (F1N$(X)/100) * EAoth2;
        //*** End   Add *** 09/22/23 **********************************
        Other;
           RPAMT = *Zeros;
        Endsl;
       //*** End Change    *** 03/21/19 ***********************************************
      /End-Free
     C*
E03  C                   ENDIF
     C*
     C* UPDATE FAEAP/EANAMT WITH CALCULATE NETWORK FEE:
     C*
      *** Begin Change *** 03/21/19 ********************************************
     C***  EARCN         IFEQ      0
     C***  RPAMT         ANDLT     0
     C***                Z-ADD     0             @@NAMT
     C***                Z-ADD     0             RPAMT
     C***                ELSE
     C***                Z-ADD     RPAMT         @@NAMT
     C***                ENDIF
      *** End Add ******** 03/21/19 ********************************************
      /Free
       Select;
       When F1NT(X) = 'F' and Q1Prms >= 10;
          If EArcn = *zeros AND RPamt < *Zeros;
             //*** Begin Chg *** 09/22/23 **********************************
             //*** @@MRA = *Zeros;
             @@OCA2 = *Zeros;
             //*** Begin Chg *** 09/22/23 **********************************
             RPamt = *Zeros;
          Else;
             //*** Begin Chg *** 09/22/23 **********************************
             //*** @@MRA = RPamt;
             @@OCA2= RPamt;
             //*** End   Chg *** 09/22/23 **********************************
          Endif;
       //*** Begin Add *** 09/22/23 **********************************
       When F1NT(X) = '2' And Q1Prms >= 10;
          If EArcn = *zeros AND RPamt < *Zeros;
             @@OCA2 = *Zeros;
             RPamt = *Zeros;
          Else;
             @@OCA2 = RPamt;
          Endif;
       //*** End   Add *** 09/22/23 **********************************
       When F1NT(X) <> 'F';
          If EArcn = *zeros AND RPamt < *Zeros;
             @@NAMT = *Zeros;
             RPamt = *Zeros;
          Else;
             @@NAMT = RPamt;
          Endif;
       Other;
             @@NAMT = *Zeros;
             RPamt = *Zeros;
       Endsl;
      /End-Free
     C*
E02  C                   ENDIF
     C*
E01  C                   ENDIF
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C*
     C                   MOVE      *OFF          @ICC              1
      *
     C                   SELECT
      *
      * Identify Cost-containment PV1
      *
     C     CMJRST        WHENEQ    'CA'
     C     ELPFDT        ANDGE     1100701
     C     *IN60         ANDEQ     *OFF
      *
     C                   MOVE      *BLANKS       XXKEY
      *
     C     CMCVFL        IFEQ      'Y'
     C     CICABC        ANDGT     *BLANKS
     C                   MOVEL(P)  CICABC        XXKEY
     C                   ELSE
     C     CICAB2        IFGT      *BLANKS
     C                   MOVEL(P)  CICAB2        XXKEY
     C                   ENDIF                                                  CICAB2
     C                   ENDIF                                                  CMCVFL = 'Y'
      *
      *  Determine if cost containment PV1 feature is turned on.
      *  Otherwise, use old logic.
      *
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      *OFF
     C     B2CCW         ANDEQ     'Y'
     C     XXKEY         ANDGT     *BLANKS
     C                   MOVEL     XXKEY         RPBR2
     C                   MOVE      *ON           @ICC
     C                   ELSE
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   OTHER
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C                   ENDSL
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C     EATRCD        IFNE      *BLANKS
     C     @@CLT#        ANDNE     '146'
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C                   SELECT
      *
      * Cost-containment PV1
      *
     C     @ICC          WHENEQ    *ON
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CICATC        RPTRCD
     C                   ELSE
     C                   MOVE      CICATR        RPTRCD
     C                   ENDIF
      *
     C                   OTHER
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF                                                  EATRCD      '
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVEL(P)  EACNTL        RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C   60              GOTO      NXTC
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
     C                   ELSE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
     C*
     C                   MOVE      EAIUSR        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      *BLANKS       RPDSQ#
     C                   MOVE      RPDSQ#        QXDSQ#
      *
      *******Beg Chg********04/17/12**********************************
     C**********         CALL      'WA023R1'
     C**********         PARM      '2'           ##MODE            1            Mode
     C**********         PARM                    ##CO#             3            Company
     C**********         PARM                    ##FND             3            Group #
     C**********         PARM                    ##FYR             3            Year
     C**********         PARM                    ##CASE            7            Case
     C**********         PARM      'WC'          ##APPL            2            Applic Code
     C**********         PARM      DATA          ##DATA           75            Selection criteria
     C**********         PARM                    ##CKTP           10            Bank Acct Code
     C**********         PARM                    ##RTCD            1            Return Code
      *
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: ##appl:
                 ##data: #rtcd: ##eftc);
       RPeftc = ##eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   EXSR      CHKEFT
     C                   MOVE      *BLANK        RPACCT
     C     EACKST        IFEQ      *BLANKS
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C     EACKST        IFEQ      'PY'
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C                   MOVE      EACKST        RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C* CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
     C*
     C     @@CLT#        IFEQ      '132'
     C     RPAMT         IFGT      250
     C     CMDNY         OREQ      'Y'
     C     CMSTAT        OREQ      'C'
     C                   MOVEL(P)  'PR'          RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVE      EAIUSR        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
     C     F1NF(X)       IFEQ      'I'
     C                   MOVE      'I'           RPNTWK
     C                   ELSE
     C                   MOVE      *BLANKS       RPNTWK
     C                   ENDIF
     C                   Z-ADD     EARCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   MOVEL     *BLANKS       RPCKCN
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C*** Begin Chg *** 06/20/19 ****************************************
     C*****RPAMT         IFGT      0
     C**** @@CLT#        ANDNE     '139'
     C**** @@CLT#        ANDNE     '136'
     C**** RPAMT         ANDNE     0
     C**** EAPNWC        ANDNE     'N'
     C**** EAPNWC        ANDNE     '3'
     C**** EAPNWC        ANDNE     '4'
      /free
           If RPamt > 0 And Not IsModEnabled('Negative Fee Checks') And
              EAPNWC <> 'N' And EAPNWC <> '3' and EAPNWC <> '4';
      /end-free
     C*** End   Chg *** 06/20/19 ****************************************
     C                   EXSR      EFTSLT
     C                   ENDIF
     C*** Begin Chg *** 06/20/19 ****************************************
     C*****@@CLT#        IFEQ      '139'
     C*****RPAMT         ANDNE     0
     C*****EAPNWC        ANDNE     'N'
     C*****EAPNWC        ANDNE     '3'
     C*****EAPNWC        ANDNE     '4'
     C*****@@CLT#        OREQ      '136'
     C*****RPAMT         ANDNE     0
     C*****EAPNWC        ANDNE     'N'
     C*****EAPNWC        ANDNE     '3'
     C*****EAPNWC        ANDNE     '4'
      /free
           If RPamt <> 0 And IsModEnabled('Negative Fee Checks') And
              EAPNWC <> 'N' And EAPNWC <> '3' and EAPNWC <> '4';
      /end-free
     C*** End   Chg *** 06/20/19 ****************************************
     C                   WRITE     PRP01
     C                   ENDIF
     C     RPAMT         IFLT      0
     C     @@CLT#        ANDEQ     '135'
     C*
     C     PAKY1         SETGT     PPA01
     C     PAKY1         READPE(N) PPA01                                  60
     C  N60              Z-ADD     PASEQ#        XXSEQ#
     C   60              Z-ADD     0             XXSEQ#
     C                   CLEAR                   PPA01
     C*
     C     XXSEQ#        ADD       1             PASEQ#
     C*
     C                   Z-ADD     RPAMT         PAAMT
     C                   Z-ADD     EOCO#         PACO#
     C                   Z-ADD     EOFND         PAFND
     C                   Z-ADD     EOFYR         PAFYR
     C                   Z-ADD     EOCASE        PACASE
     C                   Z-ADD     0             PACHK#
     C                   Z-ADD     0             PABASQ
     C                   Z-ADD     RPFEIN        PAFEIN
     C                   Z-ADD     RPMOD         PAMOD
     C*
     C                   Z-ADD     RPDT          PATRDT
     C                   Z-ADD     RPDT          PAPODT
     C                   Z-ADD     RPFRDT        PAFRDT
     C                   Z-ADD     RPTODT        PATODT
     C                   Z-ADD     0             PAWKS
     C                   Z-ADD     0             PADAYS
     C*
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
     C*
     C     *LOVAL        SETLL     PPC01
     C                   READ      PPC01                                  60
     C     *IN60         DOWEQ     '0'
     C     PCRECD        IFEQ      RPRECD
     C     PCTRTP        ANDEQ     'A'
     C                   LEAVE
     C                   ENDIF
     C                   READ      PPC01                                  60
     C                   ENDDO
     C*
     C  N60              MOVEL     PCKEY         PATRCD
     C   60              MOVE      RPTRCD        PATRCD
     C                   MOVEL     DESC1         PADESC
     C                   MOVE      'A'           PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
     C*                    MOVE *BLANK    PAADST
     C                   Z-ADD     RPEOB#        PAEOB#
     C                   Z-ADD     RPEOBS        PAEOBS
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   Z-ADD     RPDT          PADT
     C                   Z-ADD     RPTIME        PATIME
     C                   MOVEL     Q1USER        PAIUSR
     C                   MOVEL     Q1PGM         PAIPGM
     C                   Z-ADD     RPDT          PAIDT
     C                   Z-ADD     RPTIME        PAITIM
     C*
     C                   WRITE     PPA01
     C*
     C*  CREATE REQUEST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   MOVE      *BLANKS       RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   WRITE     PRP02
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C     PAKY1         CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  60
     C*
     C     CMSTAT        IFEQ      'C'
     C     PAAMT         ORGT      RR(X)
     C                   EXSR      RSVHST
     C                   ELSE
     C                   SUB       PAAMT         RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C                   ADD       PAAMT         AR(X)
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*   RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     CMSTAT        IFEQ      'O'
     C                   SUB       PAAMT         ELOUTS
     C                   ENDIF                                                  CMSTAT=O
     C                   ADD       PAAMT         ELTCLP
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF
     C*
     C                   ENDIF                                                  RPAMT < 0
     C*
      *** Begin Add *** 03/21/19 ****************************************
     C                   IF        X < 10
     C                   ADD       1             X
     C                   GOTO      NxtNwCd
     C                   ENDIF
      *** End   Add *** 03/21/19 ****************************************
     C     NXTC          TAG
     C*
     C                   ENDSR
     C******************************************************************
     C*  SRPGPC - This subroutine creates the Public Goods Pool Check
     C******************************************************************
     C     SRPGPC        BEGSR
     C*
     C     EOAST         IFEQ      'NY'
     C     CMKY          CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'P'           XXPTYP
     C                   MOVEL     EOSYS         XXSYS
     C     CIKY          CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     150329043     XXFEIN
     C                   Z-ADD     1             XXMOD
     C                   MOVEL(P)  DESC2         RPRMDS
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     RPSEQ#        HDSEQ#            5 0
     C                   CLEAR                   PRP01
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C     HDSEQ#        ADD       1             RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   Z-ADD     XXFEIN        RPFEIN
     C                   Z-ADD     XXMOD         RPMOD
     C*
     C     EAINPE        ADD       EAOUTE        RPAMT
     C                   ADD       EAASCE        RPAMT
     C                   ADD       EACLNE        RPAMT
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE(P)   CIBR2         RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE(P)   CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE(P)   CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVEL(P)  CISTS         RPSTS
     C*
     C     EATRCD        IFNE      *BLANKS
     C     @@CLT#        ANDNE     '146'
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        RPPNAM
     C                   MOVEL(P)  PDADR2        RPADR1
     C                   MOVEL(P)  *BLANKS       RPADR2
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        RPPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      RPPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        RPADR1
     C                   MOVEL(P)  PDADR2        RPADR2
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   PSSRT         RPSORT
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDCTY         RPCTY
     C                   MOVEL(P)  PDST          RPST
     C                   Z-ADD     PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   MOVEL     'G'           RPPGTP
     C                   MOVEL     'P'           RPSTAT
     C*
     C                   MOVE      EOIUSR        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   Z-ADD     EORCN         RPEOBS
     C                   Z-ADD     EOCRDT        RPRCVD
     C*
     C                   MOVEL(P)  'N'           RPSENT
     C                   MOVEL(P)  EAUSER        RPRQST
     C*
     C*  Call API to determine the Bank "Code".
     C*
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      *BLANKS       RPDSQ#
     C                   MOVE      RPDSQ#        QXDSQ#
      *
      *******Beg Chg********04/17/12**********************************
     C**********         CALL      'WA023R1'
     C**********         PARM      '2'           ##MODE                         Mode
     C**********         PARM                    ##CO#                          Company
     C**********         PARM                    ##FND                          Group #
     C**********         PARM                    ##FYR                          Year
     C**********         PARM                    ##CASE                         Case
     C**********         PARM      'WC'          ##APPL                         Applic Code
     C**********         PARM      DATA          ##DATA                         Selection criteria
     C**********         PARM                    ##CKTP                         Bank Acct Code
     C**********         PARM                    ##RTCD                         Return Code
     C*
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: ##appl:
                 ##data: #rtcd: ##eftc);
       RPeftc = ##eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   EXSR      CHKEFT
     C*
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   EXSR      CHEDPT
     C     CXKEY         CHAIN     PCX01                              60
     C     *IN90         IFEQ      *ON
     C     *IN60         ANDEQ     '0'
     C                   MOVE      CXDPPC        RPSTS
     C                   MOVE      'P'           RPSTAT
     C                   CALL      'WA061R'
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM      *BLANKS       ##RTCD            1
     C                   ENDIF
     C*
     C     RPAMT         IFGT      *ZEROS
     C                   EXSR      EFTSLT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C     CHEDPT        BEGSR
      *
      *  Check the Request Payment File for exact or possible duplicates.
      *
     C                   MOVE      *OFF          *IN66
     C                   MOVE      *OFF          *IN67
      *
     C     RPKEY2        CHAIN     PRP03                              60
     C     *IN60         DOWEQ     '0'
      *
      *  Skip record if Payment Type is Not 'M' or 'P'.
      *
     C     R2TYPE        IFNE      'M'
     C     R2TYPE        ANDNE     'P'
     C                   GOTO      NXTRP1
     C                   ENDIF
      *
      *  Payment Request is an Exact Duplicate if a Previous Request has
      *  the Same Payment Reserve Code, Provider, Dates of Service,
      *  Payment Amount, and the Sequence Number of Both WARPP records
      *  are different.
      *
     C     RPRECD        IFEQ      R2RECD
     C     RPFEIN        ANDEQ     R2FEIN
     C     RPFRDT        ANDEQ     R2FRDT
     C     RPTODT        ANDEQ     R2TODT
     C     RPAMT         ANDEQ     R2AMT
     C     R2STAT        ANDNE     'D'
     C     RPSEQ#        ANDNE     R2SEQ#
      *
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN66
      *
     C                   SETON                                        90
      *
     C                   ENDIF                                                  Exact Dup
     C     NXTRP1        TAG
      *
     C     RPKEY2        READE     PRP03                                  60
     C                   ENDDO                                                  DOW *IN60 RPKEY
      *
     C     PAKY          CHAIN     PPA01                              60
      *
     C     *IN60         DOWEQ     '0'
      *
      *  Skip record if Payment Type is Not 'M' or 'P'.
      *
     C     PATYPE        IFNE      'M'
     C     PATYPE        ANDNE     'P'
     C     PATYPE        ANDNE     'A'
     C     PATYPE        ANDNE     'Z'
     C                   GOTO      NXTPA1
     C                   ENDIF
      *
      *  Payment Request is an Exact Duplicate if a Previous Payment has
      *  the Same Payment Reserve Code, Provider, Dates of Service and
      *  Payment Amount.
      *
     C     RPRECD        IFEQ      PARECD
     C     RPFEIN        ANDEQ     PAFEIN
     C     RPFRDT        ANDEQ     PAFRDT
     C     RPTODT        ANDEQ     PATODT
     C     RPAMT         ANDEQ     PAAMT
      *
      *  DETERMINE IF A DUP PAYMENT HAS BEEN VOIDED.
      *
     C     PABASQ        IFNE      *ZERO
     C     PAADST        ORNE      *BLANKS
     C                   MOVE      *ON           XXVOID            1
     C                   ELSE
     C                   MOVE      *OFF          XXVOID
     C                   ENDIF
      *
     C     XXVOID        IFEQ      *OFF
      *
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN66
      *
     C                   SETON                                        90
      *
     C                   ENDIF
     C                   ENDIF                                                  Exact Dup
      *
     C     NXTPA1        TAG
      *
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO                                                  DOW *IN60 PAKY
      *
     C                   ENDSR
     C*===============================================================*
     C*  RSVHST - Write a reserve history record
     C*
     C*   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
     C*   ADJUSTMENT TO KEEP CLAIM MSTR (WMCMP) AND RSRV HIST (WMRHP)
     C*   IN BALANCE.
     C*===============================================================*
     C     RSVHST        BEGSR
     C*
     C                   CLEAR                   PRH01
     C*
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C                   Z-ADD     1             Y                 3 0
     C     PARECD        LOOKUP    RT(Y)                                  99
     C                   MOVEL     PARECD        RRT(Y)
     C                   Z-ADD     PAAMT         RSU(Y)
     C                   MOVEL     DESC1         RHEXPL
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   MOVEL     SRDTO         RHEFDT
     C                   Z-ADD     DSTIME        RHTIME
      *
     C                   MOVEL     SRDTO         RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
      *
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
     C*
     C                   WRITE     PRH01
     C*
     C                   ENDSR                                                  RSVHST
     C*===============================================================*
     C*  CHKEFT
     C*===============================================================*
     C     CHKEFT        BEGSR
     C* If EFT payee, then determine if pre-notification record is required
     C                   MOVE      *OFF          ##EFT                          EFT flag
     C                   MOVE      *OFF          ##PRE                          PRE note flag
     C                   MOVE      CMSS#         XXSS#
      *******Beg Chg********04/17/12**********************************
     C*****RPCKTP        IFEQ      'EFT'
      /Free
       If RPeftc = 'E';
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C                   MOVEL(P)  RPFEIN        XXWK12           12
     C                   MOVE      RPMOD         XXWK12
     C                   MOVEL(P)  XXWK12        ##KEY
     C                   MOVE      'P'           ##PTYP
     C                   MOVE      RPFND         #1FND
     C                   MOVE      RPDT          ##DATE
     C*
      *******Beg Chg********04/17/12**********************************
     C******             CALL      'WA023R3'
     C******             PARM                    ##CO#                          Company
     C******             PARM                    ##PTYP            1            Payee Type
     C******             PARM                    #1FND             3            Group #
     C******             PARM                    ##KEY            30            Key
     C******             PARM                    ##DATE            7            Date
     C******             PARM                    ##EFT             1            EFT flag
     C******             PARM                    ##PRE             1            PRE note flag
     C*
     C*****##EFT         IFEQ      *OFF
     C*****              CLEAR                   RPCKTP
     C*****              ENDIF                                                  ##EFT = *OFF
     c*
      /Free
       //Call the Procedure to retrieve EFT status.
       //*** beg chg ***** 07/23/15 *********************************
       //***If Not GetEFTStatus(##co#: ##ptyp: #1fnd: #ekey: ##date:
       //***##pre);
       If Not GetEFTStatus(##co#: ##ptyp: #1fnd: ##key: ##date:
        ##pre:##OutRRN:##EobType:##cktp);
       //*** end chg ***** 07/23/15 *********************************
          RPeftc = *Blanks;
       EndIf;
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C                   ENDIF                                                  RPCKTP='EFT'
     C                   ENDSR                                                  CHKEFT
     C*===============================================================*
     C*
     C*===============================================================*
     C*  EFTSLT - Write EFT RECORDS
     C*===============================================================*
     C*
     C     EFTSLT        BEGSR
     C                   SELECT
     C     ##EFT         WHENEQ    *OFF
     C     ##EFT         OREQ      *ON
     C     ##PRE         ANDEQ     *OFF
     C* Not EFT payee OR
     C* EFT payee but pre-notification is not required, write transaction
     C                   WRITE     PRP01
     C     ##EFT         WHENEQ    *ON
     C     ##PRE         ANDEQ     *ON
     C* EFT payee and Pre-notification is required
     C                   Z-ADD     RPAMT         SVAMT
     C* Create Pre-notification with RPAMT = 0
     C                   CLEAR                   RPAMT
     C                   WRITE     PRP01
     C* Create check request
     C                   Z-ADD     SVAMT         RPAMT
      *******Beg Chg********04/17/12**********************************
     C*****              CLEAR                   RPCKTP
      /Free
       RPeftc = *Blanks;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   WRITE     PRP01
     C                   ENDSL
     C                   ENDSR                                                  EFTSLT
     C*===============================================================*
     C**************************************************************************
