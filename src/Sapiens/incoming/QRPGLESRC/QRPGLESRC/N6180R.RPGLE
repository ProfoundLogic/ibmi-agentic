     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N6180R - CALCULATE BILLING AMT IN DAILY BATCH.
     H*****************************************************************
     H*                                                              **
     H*   P R O G R A M      S U M M A R Y                           **
     H*                                                              **
     H*   THIS PROGRAM WILL DETERMINE WHAT THE TOTAL INVOICE AMT     **
     H*   SHOULD BE FOR EACH NEW EOB ( GENERATED THAT DAY ) AND      **
     H*   UPDATE THE ACTUAL AMT WITH THAT VALUE.  DIFFERENT          **
     H*   CALCULATIONS WILL BE USED, DEPENDING ON THE TYPE OF EOB TO **
     H*   BE PROCESSED : HBA, PBR, OR NETWORK EOBS.                  **
     H***                                                           ***
     H*   1. CALL NETWORK BILLING PGMS :                             **
     H*                                                              **
     H*      HBA - N93H1R, N93H2R, N93H3R -- UPDATES FAHBL.          **
     H*                                                              **
     H*      PBR - N93P1R, N93P2R -- UPDATES FAEOL.                  **
     H*                                                              **
     H*      NET - N93N1R, N93N2R                                    **
     H*                                                              **
     H*      PRE/RE-CERT - N93U1R, N93U2R                            **
     H*                                                              **
     H*      MED MANAGEMENT - N93M1R                                 **
     H*                                                              **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  11/05/14 C:280 SUPPORT  87169                               **
     H*                 change brs to be client specific             **
     H*                                                              **
     H*  12/23/13 C:296 PROJECT  129MSD                              **
     H*                 BACK TO BASE INITIATIVE-MANAGED CARE -       **
     H*                 129MPO - REMOVE CHARGE ON RECONSIDERATIONS   **
     H*                                                              **
     H*  07/22/02 C:62  PROJECT: 126MAD                              **
     H*                 CORRECT PROCESS OF GETTING BDPC25            **
     H*                                                              **
     H*  01/22/02 C:62  PROJECT: AR033                               **
     H*                 UPDATE NETWORK CALCULATED AMOUNT.            **
     H*                                                              **
     H*  06/07/01 C:62  PROJECT: MC048 27745                         **
     H*                 DON'T SELECT INACTIVE NETWORK CONTRACTS.     **
     H*                                                              **
     H*  02/09/01 C:62  PROJECT: MCMNT                               **
     H*                 KEEP FROM UPDATING FMCMP IF INITIAL CHAIN.   **
     H*                 FAILS.                                       **
     H*                                                              **
     H*  11/12/99 C:62  PROJECT: MCMNT                               **
     H*                 ADD PARM CALL OF AA001R TO INDICATE BATCH.   **
     H*                                                              **
     H*  10/21/99 C:62  PROJECT: MC212                               **
     H*                 UPDATE FLAG TO LET IT BE KNOWN THAT IT IS    **
     H*                 OK TO BE INVOICED                            **
     H*                                                              **
     H*  10/05/99 C:62  PROJECT: MC212                               **
     H*                 NEED TO FIND OUT IF THERE IS ANY B AMT.      **
     H*                                                              **
     H*  07/26/99 C:62  PROJECT: MC082 18912                         **
     H*                 MAKE SURE IF XXAMT NE 0 LOAD BLANK INTO BBLF.**
     H*                                                              **
     H*  04/02/99 C:94  PROJECT: SUP17478                            **
     H*                 CHANGE FAEOL2 TO FAEOL24 TO SELECT OMIT      **
     H*                 ALL CANCELLED, DELETED, AND PENDED BILLS     **
     H*                                                              **
     H*  03/09/99 C:94  PROJECT: MC152                               **
     H*                 CHANGE FDCAL2 TO FDCALL AND CHANGE KEYLIST   **
     H*                                                              **
     H*  03/06/99 C:94  PROJECT: MC152                               **
     H*                 ADD RECON NUMBER TO AA001R                   **
     H*                                                              **
     H*  01/14/99 C:94  PROJECT: 25MKL                               **
     H*                 CHANGE PARMS CALLING N93CL                   **
     H*                                                              **
     H*  01/04/99 C:94  PROJECT: MC152                               **
     H*                 CHANGE PROGRAM TO USE FAEAP INSTEAD OF       **
     H*                 FEO1P, FAHBP, NATRP, NAMBP, AND NAPBP        **
     H*                 ALSO TO USE API(AA001R) FOR ANY UPDATE       **
     H*                 THIS MAY AFFECT THE CHANGE MADE ON           **
     H*                 07/05/95.                                    **
     H*                                                              **
     H*  10/26/98 C:62  Project: MC145                               **
     H*                 REMOVE THE EXCPT STATEMENTS AFTER ELSES.     **
     H*                                                              **
     H*  02/18/98 C:62  Project: MC159                               **
     H*                 1) CHANGED NMPHL & NMHSL WITH FMS4L2/L3.     **
     H*                 2) IF XXTO EQ *ZEROS, THEN MAKE IT 9999999.   *
     H*                                                               *
     H*  12/04/95 C:43  REQ/PROJ: AR014                              **
     H*                 1. THIS PROGRAM IS NO LONGER CALLED WITH AN  **
     H*                    OVRDBF WITH OPNQRYF, SO SELECT AND LAST   **
     H*                    RECORD PROCESSING LOGIC HAS BEEN ADDED.   **
     H*                                                              **
     H*  07/05/95 C:35  REQ/PROJ: 68MAP                              **
     H*                 1. ADDED MED MANAGEMENT BILLING              **
     H*                 2. ONLY PROCESS MED BILLING IF CMSETU NE Y   **
     H*                                                              **
     H*****************************************************************
     F***
     F*** BEGIN CHANGE ** 12/04/95 ************************************
     F*** FAEOL   UF  E           K        DISK
     F*** BEG CHG *** 04/02/99 ****************************************
     FFAEOL24   UF   E           K DISK
     F**********FAEOL2  UF  E           K        DISK
     F*** END CHG *** 04/02/99 ****************************************
     F*** BEGIN ADD *** 12/16/99 ************************************
     FFAEOL2    UF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** END   ADD *** 12/16/99 ************************************
     F*** END CHANGE ** 12/04/95 **************************************
     F*** BEGIN CHG *** 02/18/98 ************************************
     F********** NMHSL   IF  E           K        DISK
     F********** NMPHL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 ************************************
     FFMPDL     IF   E           K DISK
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEG CHG *** 03/09/99 ****************************************
     FFDCALL    IF   E           K DISK
     F**********FDCAL2  IF  E           K        DISK
     F*** END CHG *** 03/09/99 ****************************************
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FFMCML1    UF   E           K DISK
     F*** BEG ADD *** 01/04/99 **************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/04/99 **************************************
     F*** BEGIN ADD *** 10/05/99 **************************************
     FFAEAL1    IF   E           K DISK
     F*** END   ADD *** 10/05/99 **************************************
     F*
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F***

     D CMD             S              8    DIM(1) CTDATA PERRCD(1)
     D*** BEGIN ADD *** 10/05/99 **************************************
     D FPC             S              1    DIM(10)                              FILE BDPC25
     D*** END   ADD *** 10/05/99 **************************************
     D**
     D*** BEG ADD *** 01/04/99 ***************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
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
     D*
     D*** END ADD *** 01/04/99 ***************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 12/04/95 **************************************
     D ARGUMT          DS
     D  XQCO#                  1      2P 0
     D  XQSUB#                 3      5P 0
     D  XQFND                  6      7P 0
     D  XQFYR                  8      9P 0
     D  XQCASE                10     13P 0
     D  XQEOB#                14     18P 0
     D CNTRL           DS
     D  EOCO#                  1      2P 0
     D  EOSUB#                 3      5P 0
     D  EOFND                  6      7P 0
     D  EOFYR                  8      9P 0
     D  EOCASE                10     13P 0
     D  EOEOB#                14     18P 0
     D*
     D*** END ADD *** 12/04/95 **************************************
     D**
     D                 DS
     D  XXPGM                  1      8
     D  XXTYPE                 4      5
     D**  SYSTEM SUBSCRIBED TO :
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              GROUP SYS CODE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D** BILLING CODES FOR SYSTEM TYPES :
     D                 DS
     D  FBR                    1     30
     D                                     DIM(10)                              FILE BILL/RATE CD
     D  FNBR01                 1      3
     D  FNBR02                 4      6
     D  FNBR03                 7      9
     D  FNBR04                10     12
     D  FNBR05                13     15
     D  FNBR06                16     18
     D  FNBR07                19     21
     D  FNBR08                22     24
     D  FNBR09                25     27
     D  FNBR10                28     30
     D*
     D*** BEG ADD *** 11/05/14 **************************************
     D*
     D xxCLT           s              3
     D*
     D RtvCltid        PR                  EXTPGM('RTVCLTID')
     D xxCLT                          3
     D*
     D*** END ADD *** 11/05/14 **************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**********************************************************************
     C*** BEGIN DEL *** 02/18/98 ************************************
     C*
     C*   KEY LIST FOR THE DOCTOR PROVIDER FILE
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           EOFEIN
     C**********           KFLD           EOMOD
     C**********           KFLD           XXNWCD
     C**********           KFLD           EOTODT
     C*
     C*
     C*   KEY LIST FOR THE HOSPITAL PROVIDER FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           EOFEIN
     C**********           KFLD           EOMOD
     C**********           KFLD           XXNWCD
     C**********           KFLD           EOTODT
     C*** END   DEL *** 02/18/98 ************************************
     C*** BEGIN ADD *** 02/18/98 ************************************
     C*
     C*   KEY LIST FOR PROVIDER NETWORK CONTRACT FILE.
     C*
     C     S4KEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    XXNWCD
     C                   KFLD                    EOTODT
     C*** END   ADD *** 02/18/98 ************************************
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C*  CASE DETAIL RECORDS
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C**********           KFLD           EOTRCD  2        DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C*  GROUP FILE - FMFNL
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXDATE            7 0
     C*
     C* CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*** BEGIN ADD *** 12/04/95 *************************************
     C*
     C* FAEOL2, WHEN XQEOB# NOT EQUAL ZERO
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XQCO#
     C                   KFLD                    XQSUB#
     C                   KFLD                    XQFND
     C                   KFLD                    XQFYR
     C                   KFLD                    XQCASE
     C                   KFLD                    XQEOB#
     C*** BEGIN ADD *** 10/05/99 **************************************
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*** END   ADD *** 10/05/99 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1SUB#            5
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1CASE            7
     C                   PARM                    Q1EOB#            9
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
     C                   MOVE      Q1CO#         XQCO#
     C                   MOVE      Q1SUB#        XQSUB#
     C                   MOVE      Q1FND         XQFND
     C                   MOVE      Q1FYR         XQFYR
     C                   MOVE      Q1CASE        XQCASE
     C                   MOVE      Q1EOB#        XQEOB#
     C*
     C*** END ADD *** 12/04/95   *************************************
     C*
     C*
     C     *LIKE         DEFINE    BDAM01        XXAM01                          MIN. BILL AMT.
     C     *LIKE         DEFINE    EOSUB#        OLDSUB                          ADD 01/07/94
     C     *LIKE         DEFINE    EOFND         OLDFND                          ADD 01/07/94
     C     *LIKE         DEFINE    S4BGDT        XXFROM
     C     *LIKE         DEFINE    S4ENDT        XXTO
     C*
     C                   MOVE      *BLANKS       XXNWCD            5
     C                   Z-ADD     *ZEROS        X                 5 0
     C*
     C*  GET THE CURRENT DATE TO USE FOR KEY TO BILLING MASTER
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*** BEG ADD *** 03/18/99 ***************************************
     C*
     C* FIND OUT IF PROVIDER BILLS IS INSTALLED
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*** END ADD *** 03/18/99 ***************************************
     C*
     C*** Beg Add *** 11/05/14 **************************************  ***************
     C*
      /Free
       RtvCltid(xxCLT);
      /End-Free
     C*
     C*** End Add *** 11/05/14 **************************************  ***************
     C*** BEGIN ADD *** 12/16/99 *************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** END   ADD *** 12/16/99 *************************************
     C*** BEGIN CHANGE  12/04/95 *************************************
     C*
     C********** *LOVAL ** SETLLPEO01
     C*
     C* ARE WE PROCESSING ALL EOMBS OR A PARTICULAR ONE THAT HAS BEEN
     C* PASSED INTO THE PROGRAM
     C*
     C                   MOVE      *OFF          *IN60
     C     XQEOB#        IFNE      *ZEROS                                       GET ONE EOB
     C*** BEGIN CHG *** 12/16/99 *************************************
     C*********  EOKEY     SETLLPEO01                6060
     C   16EOKEY         SETLL     PEO01                              6060
     C  N16EOKEY         SETLL     PEO02                              6060
     C*** END   CHG *** 12/16/99 *************************************
     C                   ELSE
     C*** BEGIN CHG *** 12/16/99 *************************************
     C********** *LOVAL    SETLLPEO01                6060  GET ALL EOB
     C   16*LOVAL        SETLL     PEO01                              6060      GET ALL EOB
     C  N16*LOVAL        SETLL     PEO02                              6060      GET ALL EOB
     C*** END   CHG *** 12/16/99 *************************************
     C                   ENDIF
     C*
     C* BEGIN READING FROM THE EOMB FILE, ONLY THOSE RECORDS WHERE
     C* EOSTAT <> 'P' AND EOBLST = 'C' WILL BE PROCESSED.
     C*
     C******************** READ PEO01                    60
     C*** BEGIN CHG *** 12/16/99 *************************************
     C**N60*****           READ PEO01                    60
     C  N60
     CAN 16              READ      PEO01                                  60
     C  N60
     CANN16              READ      PEO02                                  60
     C*** END   CHG *** 12/16/99 *************************************
     C*** END CHANGE 12/04/95 ***************************************
     C     *IN60         DOWEQ     '0'
     C*** Beg Add *** 12/23/13 **************************************
     C*
     C*** SKIP RECONSIDERATION RECORDS
     C*
     C                   MOVE      *ON           BRS               1
     C     EORCN         IFGT      0
     C*
     C*** Beg Add *** 11/05/14 **************************************  ***************
     C*
     C     xxclt         andeq     '129'
     C*
     C*** End Add *** 11/05/14 **************************************  ***************
     C*
     C                   MOVE      *OFF          BRS
     C                   ENDIF
     C*** End Add *** 12/23/13 ****************************************
     C*** BEGIN ADD * 12/04/95 ****************************************
     C*
     C*** IF A SPECIFIC EOB REQUESTED, TEST FOR END OF SUBSET
     C*
     C     XQEOB#        IFNE      *ZEROS
     C     CNTRL         IFNE      ARGUMT
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 12/04/95 ****************************************
     C*
     C*** CHECK IF FUND HAS CHANGED, YES MEANS BILLING CODE MAY CHANGE
     C*
     C     EOSUB#        IFNE      OLDSUB
     C     EOFND         ORNE      OLDFND
     C                   EXSR      FUND
     C                   END
     C*
     C*
     C*** CLEAR WORK AMT FIELDS
     C*
     C                   MOVE      *ZEROS        QQBAMT
     C                   MOVE      *ZEROS        XXBAMT
     C                   MOVE      *ZEROS        XXAMT
     C*** BEGIN CHG *** 07/22/02 **************************************
     C*** BEGIN ADD *** 10/04/99 **************************************
     C**********           Z-ADD1         X       50
     C********** EOSYS     LOKUPFSY,X                    62
     C*
     C********** *IN62     IFEQ *ON
     C*
     C**********           MOVELFPC,X     BDPC25    P
     C*
     C**********           ENDIF
     C                   Z-ADD     2             BDPC25
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 62
     C     *IN62         IFEQ      *ON
     C                   MOVEL(P)  FPC(X)        BDPC25
     C                   ENDIF
     C*** END   CHG *** 07/22/02 **************************************
     C*
     C* IS PROVIDER BILLS INSTALLED
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C* IS THERE A NETWORK DISCOUNT
     C*
     C     EONDIS        IFGT      *ZEROS
     C*
     C* DO WE PROCESS FOR THE PARTICULAR SYSTEM AS WELL
     C*
     C*** BEGIN CHG *** 07/22/02 **************************************
     C********** BDPC25    IFEQ 2
     C     BDPC25        IFNE      1
     C*** END   CHG *** 07/22/02 **************************************
     C*
     C                   EXSR      EAP
     C*
     C                   ENDIF
     C*
     C* CARRY OUT THE NETWORK CALCULATIONS
     C*
     C                   EXSR      NET
     C*
     C                   ELSE
     C*
     C                   EXSR      EAP
     C*
     C                   ENDIF
     C*
     C* PROVIDER BILLS IS NOT INSTALLED
     C*
     C                   ELSE
     C*** END   ADD *** 10/04/99 **************************************
     C*
     C* IS THERE A NETWORK DISCOUNT
     C*
     C     EONDIS        IFGT      *ZEROS
     C*
     C* DO WE PROCESS FOR THE PARTICULAR SYSTEM AS WELL
     C*
     C     BDPC25        IFEQ      2
     C*
     C     EOSYS         IFEQ      'P'
     C                   EXSR      PBR
     C                   END                                                    EOSYS = P
     C*
     C     EOSYS         IFEQ      'H'
     C                   EXSR      HBA
     C                   END                                                    EOSYS = H
     C*
     C     EOSYS         IFEQ      'U'
     C                   EXSR      CRT
     C                   END                                                    EOSYS = U
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'M'
     C                   EXSR      MED
     C                   END                                                    EOSYS = M
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   END                                                    BDPC25 = 2
     C*
     C* CARRY OUT THE NETWORK CALCULATIONS
     C*
     C                   EXSR      NET
     C**
     C                   ELSE                                                   EONDIS LT 0
     C*
     C* IF NO NETWORK DISCOUNT PROCESS JUST FOR THE SYSTEM
     C*
     C     EOSYS         IFEQ      'P'
     C                   EXSR      PBR
     C                   END                                                    EOSYS = P
     C*
     C     EOSYS         IFEQ      'H'
     C                   EXSR      HBA
     C                   END                                                    EOSYS = H
     C*
     C     EOSYS         IFEQ      'U'
     C                   EXSR      CRT
     C                   END                                                    EOSYS = U
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'M'
     C                   EXSR      MED
     C                   END                                                    EOSYS = M
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   END                                                    EONDIS GT 0
     C*** BEGIN ADD *** 10/04/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/04/99 **************************************
     C*
     C* POPULATE HOLD FIELDS FOR GROUP AND SUBSCRIBER TO INDICATE
     C* GROUP BREAKS.
     C*
     C                   Z-ADD     EOFND         OLDFND
     C                   Z-ADD     EOSUB#        OLDSUB
     C*
     C* READ NEXT EOMB
     C*
     C*** BEGIN CHG *** 12/16/99 *************************************
     C**********           READ PEO01                    60
     C   16              READ      PEO01                                  60
     C  N16              READ      PEO02                                  60
     C*** END   CHG *** 12/16/99 *************************************
     C                   END                                                    60 = 0
     C*
     C* TERMINATE THE PROGRAM
     C*
     C                   SETON                                          LR
     C*
     C*************************************************
     C*  CALCULATE INVOICE AMT FOR NETWORK TYPE EOBS  *
     C*************************************************
     C*
     C     NET           BEGSR
     C*
     C*** DETERMINE IF GROUP & PROVIDER ARE PARTICIPATING IN THE
     C*** NETWORK PROGRAM -- IF NOT, DO NOT CALL PGMS.
     C*
     C     PDKEY         CHAIN     PPD01                              63
     C     *IN63         IFEQ      '0'
     C*
 B2  C     PDPARN        IFEQ      'P'
     C*
     C*  DETERMINE IF SERVICE DATES ARE WITHIN PROVIDERS DATE RANGE ...
     C*
     C                   EXSR      SRPROV                                                   VDS
     C*
     C*
     C     XNTYPE        IFNE      *BLANKS                                      NTWRK GRP
     C     MATCH         ANDEQ     'Y'                                          WITHIN
     C*                                                    CONTRACT DTS
     C*
     C*** CALL THE APPROPRIATE PGM TO CALCULATE THE NETWORK BILLING
     C*** AMT BASED ON THE BILLING TYPE IN BDTYPE.
     C*
     C                   MOVE      *ZEROS        QCO#              3
     C                   MOVE      *ZEROS        QSUB#             5
     C                   MOVE      *ZEROS        QFUND             3
     C                   MOVE      *ZEROS        QFYR              3
     C                   MOVE      *ZEROS        QCASE             7
     C                   MOVE      *ZEROS        QFEIN             9
     C                   MOVE      *ZEROS        QMOD              3
     C                   MOVE      *ZEROS        QEOB#             9
     C                   MOVE      *ZEROS        QQBAMT            9
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        QRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   MOVE      EOCO#         QCO#              3
     C                   MOVE      EOSUB#        QSUB#             5
     C                   MOVE      EOFND         QFUND             3
     C                   MOVE      EOFYR         QFYR              3
     C                   MOVE      EOCASE        QCASE             7
     C                   MOVE      EOFEIN        QFEIN             9
     C                   MOVE      EOMOD         QMOD              3
     C                   MOVE      EOEOB#        QEOB#
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EORCN         QRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XNTYPE        XXTYPE            2
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFUND
     C                   PARM                    QEOB#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   MOVE      QQBAMT        XXBAMT            9 2
     C*
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*** UPDATE NETWORK TRANS FILE RECORD
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*
     C* INDICATES WHAT FILE IS TO BE UPDATED BY AA001R
     C* XXFAEA = 'U' WILL UPDATE FAEAP
     C* XXNATR = 'U' WILL UPDATE NATRP
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   MOVE      'U'           XXFAEA
     C*
     C                   ELSE
     C*** END   ADD *** 07/26/99 **************************************
     C                   MOVE      'U'           XXNATR
     C*** BEGIN ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/26/99 **************************************
     C*
     C* IF THERE IS NO AMOUNTS, THEN THE NETWORK BILLING FLAG WILL
     C* BE SET SO THAT IT WILL NOT BE PICKED UP FOR INVOICING
     C*
     C     XXBAMT        IFEQ      *ZEROS
     C                   MOVE      'P'           ##NBLF
     C*** BEGIN ADD *** 07/26/99 **************************************
     C                   MOVEL     *BLANKS       ##BBLF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       ##NBLF
     C                   MOVEL     *BLANKS       ##BBLF
     C*** END   ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*
     C* UPDATE FAEAP OR NATRP
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/06/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/06/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C*** BEGIN CHG *** 11/09/99 **************************************
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C*** END   CHG *** 11/09/99 **************************************
     C                   PARM      XXBAMT        ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM      'R'           ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C*** BEGIN CHG *** 11/09/99 **************************************
     C**********           PARM           ##TMBA  92       TIME/MAT BEFORE
     C**********           PARM           ##TMAA  92       TIME/MAT AFTER
     C                   PARM      *ZEROS        ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM      *ZEROS        ##TMAA            9 2          TIME/MAT AFTER
     C*** END   CHG *** 11/09/99 **************************************
     C*** BEGIN ADD *** 11/12/99 **************************************
18   C                   PARM      *BLANKS       ##CERT           15            PRE-CERT
19   C                   PARM      *ZEROS        ##EXT             3 0          EXTENSION
20   C                   PARM      *ZEROS        ##FEIN            9 0          PROVIDER FEIN
21   C                   PARM      *ZEROS        ##MOD             3 0          PROVIDER MOD
22   C                   PARM      *ZEROS        ##BIDT            7 0          BRS INV DT
23   C                   PARM      ' '           ##BPFL            1
24   C                   PARM      'B'           ##MODE            1
     C*** END   ADD *** 11/12/99 **************************************
     C*** BEGIN ADD *** 01/22/02 **************************************
25   C                   PARM      *ZEROS        ##NIDT            7 0
26   C                   PARM      XXBAMT        ##NCAM            9 2
     C*** END   ADD *** 01/22/02 **************************************
     C*
     C*** END CHG *** 01/04/99 ***************************************
     C*
     C*
     C*
     C                   END                                                    XNTYPE &
     C*                                                    MATCH
     C                   END                                                    PDPARN=Y
     C*
     C*** IF THE NETWORK BILLING AMOUNT (TRBAMT) IS ZERO, AND THE BILLING
     C*** FLAG IS SUCH THAT ONLY ONE BILLING AMOUNT SHOULD BE CALCULATED
     C*** THEN CALCULATE PBR OR HBA BILLING AMOUNT BASED ON SYSTEM TYPE
     C*** BEG CHG *** 01/04/99 *************************************
     C     XXBAMT        IFEQ      *ZERO
     C********** TRBAMT    IFEQ *ZERO
     C*** END CHG *** 01/04/99 *************************************
     C     BDPC25        ANDEQ     1
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*
     C* IS PROVIDER BILLS INSTALLED
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   EXSR      EAP
     C*
     C* PROVIDER BILLS IS NOT INSTALLED
     C*
     C                   ELSE
     C*** END   ADD *** 07/26/99 **************************************
     C*
     C     EOSYS         IFEQ      'P'
     C                   EXSR      PBR
     C                   END                                                    EOSYS = P
     C*
     C     EOSYS         IFEQ      'U'
     C                   EXSR      CRT
     C                   END                                                    EOSYS = U
     C*
     C     EOSYS         IFEQ      'H'
     C                   EXSR      HBA
     C                   END                                                    EOSYS = H
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'M'
     C                   EXSR      MED
     C                   END                                                    EOSYS = M
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*** BEGIN ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/26/99 **************************************
     C*
     C                   END                                                    ENDEQ 0/1
     C*
     C                   END                                                    63 = 0
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C*****************************************************************
     C*  CRT SUBROUTINE: CALCULATE INVOICE AMT FOR PRE/RE-CERT EOMBS  *IL.
     C*****************************************************************
     C     CRT           BEGSR
     C*
     C*                                                                89
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        XFYR              3
     C                   MOVE      *ZEROS        XCASE             7
     C                   MOVE      *ZEROS        XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*** DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C*
     C     XUTYPE        IFNE      *BLANKS
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XUTYPE        XXTYPE            2
     C                   MOVE      EOCO#         XXCO#             3
     C                   MOVE      EOSUB#        XXSUB#            5
     C                   MOVE      EOFND         XXGRP#            3
     C                   MOVE      EOEOB#        XXEO#             9
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR
     C                   MOVE      EOCASE        XCASE
     C                   MOVE      EORCN         XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM             8
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   END                                                    XHTYPE
     C*
     C*** UPDATE PRE/RE-CERT TRANS FILE RECORD
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*
     C* GET THE RELATIVE RECORD NUMBER OF THE CASE RECORD TO PASS
     C* IT TO AA001R.
     C*
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EOFLG5        IFNE      'Y'
     C*** END   ADD *** 09/23/99 **************************************
     C                   MOVE      'P'           EOBLFL
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*** END   ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE QQBAMT    ##BAMT           BRS AMOUNT
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAPB
     C*
     C* UPDATE VALUES IN NAPBP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/06/99  **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/06/99  **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 01/04/99 ***************************************
     C*
     C* UPDATE EOSTAT WITH A 'P' SO THAT IT WILL NOT BE PROCESSED
     C* AGAIN BY THIS PROGRAM.
     C*
     C*** BEGIN DEL *** 12/21/99 *************************************
     C**********           MOVE 'P'       EOSTAT
     C*** END   DEL *** 12/21/99 *************************************
     C*** BEGIN CHG *** 12/16/99 *************************************
     C**********           UPDATPEO01
     C                   UPDATE    PEO02
     C*** END   CHG *** 12/16/99 *************************************
     C*
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  HBA SUBROUTINE: CALCULATE INVOICE AMT FOR HBA TYPE EOBS.     *IL.
     C*****************************************************************
     C     HBA           BEGSR
     C*
     C*                                                                89
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        XFYR              3
     C                   MOVE      *ZEROS        XCASE             7
     C                   MOVE      *ZEROS        XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*** DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C*
     C     XHTYPE        IFNE      *BLANKS
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XHTYPE        XXTYPE            2
     C                   MOVE      EOCO#         XXCO#             3
     C                   MOVE      EOSUB#        XXSUB#            5
     C                   MOVE      EOFND         XXGRP#            3
     C                   MOVE      EOEOB#        XXEO#             9
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR              3
     C                   MOVE      EOCASE        XCASE             7
     C                   MOVE      EORCN         XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM             8
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   END                                                    XHTYPE
     C*
     C*** UPDATE HBA TRANS FILE RECORD
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*
     C* GET THE RELATIVE RECORD NUMBER OF THE CASE RECORD TO PASS
     C* IT TO AA001R.
     C*
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EOFLG5        IFNE      'Y'
     C*** END   ADD *** 09/23/99 **************************************
     C                   MOVE      'P'           EOBLFL
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*** END   ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 12/16/99 **************************************
     C                   UPDATE    PEO02
     C*** END   ADD *** 12/16/99 **************************************
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE QQBAMT    ##BAMT           BRS AMOUNT
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
     C*
     C* UPDATE VALUES IN FAHBP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/06/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/06/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** END CHG *** 01/04/99 ****************************************
     C*
     C                   ENDSR
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*****************************************************************
     C*  MED SUBROUTINE: CALCULATE INVOICE AMT FOR MED SET-UP         *
     C*****************************************************************
     C     MED           BEGSR
     C*
     C* GET THE RELATIVE RECORD NUMBER OF THE CASE RECORD TO PASS
     C* IT TO AA001R AND DETERMINE IF THIS CASE HAS ALREADY BEEN
     C* PROCESSED ONCE BEFORE.
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* IF CMSETU IS NOT EQUAL TO 'Y', THEN IT HASN'T BEEN PROCESSED
     C* YET.  ANY SUBSEQUENT EOMBS OFF OF THIS CASE WILL NOT GO THRU
     C* THIS PROCESS AGAIN('M' TYPES THAT IS).
     C*
     C     CMSETU        IFNE      'Y'
     C*                                                                89
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        XFYR              3
     C                   MOVE      *ZEROS        XCASE             7
     C                   MOVE      *ZEROS        XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C* DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C     XMTYPE        IFNE      *BLANKS
     C*
     C* Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C*
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XMTYPE        XXTYPE            2
     C                   MOVE      EOCO#         XXCO#             3
     C                   MOVE      EOSUB#        XXSUB#            5
     C                   MOVE      EOFND         XXGRP#            3
     C                   MOVE      EOEOB#        XXEO#             9
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR              3
     C                   MOVE      EOCASE        XCASE             7
     C                   MOVE      EORCN         XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM             8
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   END                                                    XMTYPE
     C*
     C* UPDATE MEDICAL MANAGEMENT RECORD
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*** BEGIN DEL *** 09/23/99 **************************************
     C********** CMKEY     CHAINPCM01               N99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDDBRRN     ##RRN
     C**********           ENDIF
     C*** END   DEL *** 09/23/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C     EOFLG5        IFNE      'Y'
     C*
     C                   MOVE      'P'           EOBLFL
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE QQBAMT    ##BAMT           BRS AMOUNT
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAMB
     C*
     C* UPDATE VALUES IN NAMBP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C*** BEGIN ADD *** 09/23/99 **************************************
     C**********           PARM           ##RRN   90       RRN OF CASE
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C*** END   ADD *** 09/23/99 **************************************
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/06/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/06/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** END CHG *** 01/04/99 ***************************************
     C*** BEGIN DEL *** 12/21/99 *************************************
     C**********           MOVE 'P'       EOSTAT
     C*** END   DEL *** 12/21/99 *************************************
     C*** BEGIN CHG *** 12/16/99 *************************************
     C**********           UPDATPEO01
     C                   UPDATE    PEO02
     C*** END   CHG *** 12/16/99 *************************************
     C*
     C                   MOVE      'Y'           CMSETU
     C                   UPDATE    PCM01
     C                   ELSE
     C*** BEGIN DEL *** 10/26/98 **************************************
     C**********           EXCPTPCM01E
     C*** END   DEL *** 10/26/98 **************************************
     C*** BEGIN ADD *** 10/26/98 **************************************
     C     EOFLG5        IFNE      'Y'
     C*
     C                   MOVEL     'P'           EOBLFL
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 10/26/98 **************************************
     C*** BEGIN DEL *** 12/21/99 *************************************
     C**********           MOVE 'P'       EOSTAT
     C*** END   DEL *** 12/21/99 *************************************
     C*** BEGIN CHG *** 12/16/99 *************************************
     C**********           UPDATPEO01
     C                   UPDATE    PEO02
     C*** END   CHG *** 12/16/99 *************************************
     C                   END                                                    END CMSETU
     C                   END                                                    END *IN60
     C*
     C                   ENDSR
     C*
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*****************************************************************
     C*  PBR SUBROUTINE: CALCULATE INVOICE AMT FOR PBR TYPE EOBS.     *
     C*****************************************************************
     C     PBR           BEGSR
     C*
     C                   MOVE      *ZEROS        QQBAMT
     C*
     C*  GET THE CASE DETAIL CLAIM RECORD.
     C*
     C     CAKEY         CHAIN     PCA01                              52
     C*
     C*  TEST FOR A HOSPTIAL PROVIDER TYPE.
     C*
     C     CAPRTY        COMP      '4'                                    91
     C*
     C*  READ ALL THE CASE DETAIL RECORDS IN ORDER TO ACCUMULATE TOTAL
     C*
     C     *IN52         DOWEQ     '0'
     C*
     C                   ADD       CATOTC        TTABIL            9 2
     C                   ADD       CATOTD        TTARED            9 2
     C                   ADD       CATOTP        TTAALW            9 2
     C*
     C                   ADD       1             #LIN              5 0
     C*
     C*  READ THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C*
     C     CAKEY         READE     PCA01                                  52
     C*
     C                   END                                                    END DOWEQ 52
     C*
     C*** DETERMINE BILLING METHOD
     C                   Z-ADD     *ZERO         XX#LIN            5 0
     C*
     C*** CALL THE PROGRAM TO CALCULATE THE PBR BILLING AMT
     C*
     C     XPTYPE        IFNE      *BLANKS
     C*
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XPTYPE        XXTYPE
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXGRP#
     C                   MOVE      EOEOB#        XXEO#
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR
     C                   MOVE      EOCASE        XCASE
     C                   MOVE      EORCN         XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   END                                                    XPTYPE
     C*
     C                   MOVE      QQBAMT        XXAMT             9 2
     C*
     C     EO#BLL        IFEQ      *ZERO
     C                   Z-ADD     1             EO#BLL
     C                   END                                                    ENDIF EO#BLL
     C*
     C     XPTYPE        IFEQ      'P1'
     C     EO#BLL        MULT      XXAMT         XXAMT
     C                   END
     C*
     C*** UPDATE EOB TRANS FILE RECORD
     C*** BEGIN DEL *** 09/23/99 **************************************
     C**********
     C*** BEG ADD *** 03/18/99 **************************************
     C********** ##PRVB    IFEQ 'Y'
     C********** CMKEY     CHAINPCM01               N99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDDBRRN     ##RRN
     C**********           ENDIF
     C*
     C*
     C********** XXAMT     IFEQ *ZEROS
     C**********           MOVE 'P'       ##BBLF
     C*** BEGIN ADD *** 07/26/99 **************************************
     C**********           MOVEL*BLANKS   ##NBLF
     C*
     C**********           ELSE
     C*
     C**********           MOVEL*BLANKS   ##NBLF
     C**********           MOVEL*BLANKS   ##BBLF
     C*** END   ADD *** 07/26/99 **************************************
     C**********           ENDIF
     C*
     C**********           MOVE XXAMT     ##BAMT           BRS AMOUNT
     C**********           MOVE XXAMT     ##BCAM           BRS CALC AMOUNT
     C**********           MOVE *BLANKS   KKWRUP
     C**********           MOVE 'U'       XXFAEO
     C*
     C**********           CALL 'AA001R'
     C**********           PARM           ##RRN   90       RRN OF CASE
     C**********           PARM EOEOB#    ##EOB#  90       EOB #
     C**********           PARM EORCN     ##RCN   30       RECON #
     C**********           PARM Q1PGM     ##PGM  10        NAME OF PGM CALLING
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C**********           PARM           ##NAMT  92       NWS AMOUNT
     C**********           PARM           ##BHRF  1        BRS INV H/R FLAG
     C**********           PARM           ##BBLF  1        BRS INV PROC FLAG
     C**********           PARM           ##BINV  90       BRS INVOICE #
     C**********           PARM           ##BCAM  92       BRS CALC AMOUNT
     C**********           PARM           ##NHRF  1        NWS INV H/R FLAG
     C**********           PARM           ##NBLF  1        NWS INV PROC FLAG
     C**********           PARM           ##NINV  90       NWS INVOICE #
     C**********           PARM EOSYS     ##SYS   1        TYPE OF SERVICE
     C**********           PARM KKWRUP    ##WRUP  7        WRITE/UPDATE FLAG
     C**********           PARM           ##TMBA  92       TIME/MAT BEFORE
     C**********           PARM           ##TMAA  92       TIME/MAT AFTER
     C**********           ELSE
     C*** END ADD *** 03/18/99 **************************************
     C*** END   DEL *** 09/23/99 **************************************
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           Z-ADDXXAMT     EOBAMT           ACTUAL BILL AMT
     C                   ADD       XXAMT         EOBAMT                         ACTUAL BILL AMT
     C*** END   CHG *** 09/23/99 **************************************
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     EOBAMT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EOFLG5        ANDNE     'Y'
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C                   MOVE      'P'           EOBLFL                         BILLED FLAG
     C                   END                                                    ENDEQ *ZERO
     C*** BEGIN ADD *** 09/23/99 **************************************
     C*** BEGIN DEL *** 12/21/99 *************************************
     C**********           MOVE 'P'       EOSTAT           PROCESSED
     C*** END   DEL *** 12/21/99 *************************************
     C*** END   ADD *** 09/23/99 **************************************
     C                   MOVE      Q1PGM         EOPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME                         TIME
     C*** BEGIN CHG *** 12/16/99 *************************************
     C**********           UPDATPEO01
     C                   UPDATE    PEO02
     C*** END   CHG *** 12/16/99 *************************************
     C*** BEGIN DEL *** 09/23/99 **************************************
     C*** BEG ADD *** 03/18/99 **************************************
     C**********           ENDIF
     C*** END ADD *** 03/18/99 **************************************
     C*** END   DEL *** 09/23/99 **************************************
     C                   ENDSR
     C*
     C*
     C*****************************************************************--*
     C*  PERFORM DATE CALCULATIONS TO DETERMINE IF NETWORK BILL NEEDED.
     C*****************************************************************--*
     C*
     C     SRPROV        BEGSR
     C*
     C*  A FLAG MATCH IS RETURNED WITH 'Y' IF SERVICE
     C*  DATES ALL WITHIN PROVIDER CONTRACT EFFECTIVE
     C*  DATES
     C*
     C                   MOVE      'Y'           MATCH             1
     C*
     C* CHAIN TO THE EOMB AUXILIARY FILE TO GET NETWORK CODE
     C*
     C                   MOVE      *BLANKS       XXNWCD
     C*** BEG CHG *** 01/04/99 ****************************************
     C*
     C                   MOVE      EONTWK        XXNWCD
     C*
     C*** END CHG *** 01/04/99 ****************************************
     C*
     C*  GET THE EFFECTIVE AND TERMINATION DATE FOR PROVIDER
     C*
     C     PDTYPE        IFEQ      '4'
     C*** BEGIN CHG *** 02/18/98 ************************************
     C********** HSKEY     SETGTPHS01
     C**********           READPPHS01                    60
     C     S4KEY         SETGT     PS401
     C*** BEGIN ADD *** 06/07/01 **************************************
     C     NXTHOS        TAG
     C*** END   ADD *** 06/07/01 **************************************
     C                   READP     PS401                                  60
     C     *IN60         IFEQ      '0'
     C********** HSNWCD    ANDEQXXNWCD
     C********** HSFEIN    ANDEQEOFEIN
     C********** HSFMOD    ANDEQEOMOD
     C**********           Z-ADDHSCEDT    XXFROM
     C**********           Z-ADDHSTMDT    XXTO
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     EOFEIN
     C     S4MOD         ANDEQ     EOMOD
     C*** BEGIN ADD *** 06/07/01 **************************************
     C     S4STAT        IFEQ      'I'
     C*
     C                   GOTO      NXTHOS
     C*
     C                   ENDIF
     C*** END   ADD *** 06/07/01 **************************************
     C                   Z-ADD     S4BGDT        XXFROM
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4ENDT        XXTO
     C*
     C                   ENDIF
     C*
     C*** END   CHG *** 02/18/98 ************************************
     C                   ELSE                                                   *IN60 ON
     C*
     C** IF NO ASSOCIATED RECORD, NOT IN DATE RANGE, READ NEXT RECORD.
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    *IN60 OFF
     C                   END                                                    PDTYPE=4
     C*
     C     PDTYPE        IFEQ      '1'
     C*** BEGIN CHG *** 02/18/98 ************************************
     C********** PHKEY     SETGTPPH01
     C**********           READPPPH01                    60
     C     S4KEY         SETGT     PS402
     C*** BEGIN ADD *** 06/07/01 **************************************
     C     NXTPHY        TAG
     C*** END   ADD *** 06/07/01 **************************************
     C                   READP     PS402                                  60
     C     *IN60         IFEQ      '0'
     C********** PHNWCD    ANDEQXXNWCD
     C********** PHFEIN    ANDEQEOFEIN
     C********** PHFMOD    ANDEQEOMOD
     C**********           Z-ADDPHNEDT    XXFROM
     C**********           Z-ADDPHNXDT    XXTO
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     EOFEIN
     C     S4MOD         ANDEQ     EOMOD
     C*** BEGIN ADD *** 06/07/01 **************************************
     C     S4STAT        IFEQ      'I'
     C*
     C                   GOTO      NXTPHY
     C*
     C                   ENDIF
     C*** END   ADD *** 06/07/01 **************************************
     C                   Z-ADD     S4BGDT        XXFROM
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4ENDT        XXTO
     C*
     C                   ENDIF
     C*
     C*** END   CHG *** 02/18/98 ************************************
     C                   ELSE                                                   *IN60 ON
     C*
     C** IF NO ASSOCIATED RECORD, NOT IN DATE RANGE, READ NEXT RECORD.
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    *IN60 OFF
     C                   END                                                    PDTYPE=1
     C*
     C*  COMPARE DATE RANGE -- IF FROM/TO SERVICE DATES ARE BOTH BEFORE
     C*    OR BOTH AFTER THE PROVIDER CONTRACT DATES, NO NETWORK BILL
     C*    IS REQUIRED. BUT, IF ANY SERVICE DATES ARE IN RANGE, A BILL
     C*    WILL BE PRODUCED..
     C*
     C     EOFRDT        IFLT      XXFROM                                       BEFORE THE
     C     EOTODT        ANDLT     XXFROM                                       CONTRACT
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    EOFRDT LT
     C*
     C     EOFRDT        IFGT      XXTO                                         AFTER THE
     C     EOTODT        ANDGT     XXTO                                         CONTRACT
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    EOFRDT GT
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************--*
     C*  WHEN FUND HAS CHANGED, MUST GET NEW BILLING CODES ...
     C*****************************************************************--*
     C*
     C     FUND          BEGSR
     C*
     C*** CLEAR BILLING TYPES
     C*
     C                   MOVE      *BLANKS       XPTYPE
     C                   MOVE      *BLANKS       XHTYPE
     C                   MOVE      *BLANKS       XUTYPE
     C                   MOVE      *BLANKS       XNTYPE
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   MOVE      *BLANKS       XMTYPE
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   MOVE      *BLANKS       XXCODE
     C*
     C     FNKEY         CHAIN     PFN01                              61
     C     *IN61         IFEQ      '0'
     C*
     C*** FIRST GET PBR TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'P'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDP        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDP
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C*** BEGIN ADD *** 10/05/99 **************************************
     C                   MOVEL     BDPC25        FPC(X)
     C*** END   ADD *** 10/05/99 **************************************
     C                   MOVEL     BDTYPE        XPTYPE            2
     C                   END                                                    64=0 P TYPE
     C*
     C                   END                                                    62=1 P TYPE
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C*** NEXT, GET HBA TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'H'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDH        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDH
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C*** BEGIN ADD *** 10/05/99 **************************************
     C                   MOVEL     BDPC25        FPC(X)
     C*** END   ADD *** 10/05/99 **************************************
     C                   MOVEL     BDTYPE        XHTYPE            2
     C                   END                                                    64=0 H TYPE
     C*
     C                   END                                                    62=1 H TYPE
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C*** NEXT, GET PRE/RE-CERT TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'U'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDU        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDU
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C*** BEGIN ADD *** 10/05/99 **************************************
     C                   MOVEL     BDPC25        FPC(X)
     C*** END   ADD *** 10/05/99 **************************************
     C                   MOVEL     BDTYPE        XUTYPE            2
     C                   END                                                    64=0 U TYPE
     C*
     C                   END                                                    62=1 U TYPE
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C* NEXT, GET MEDICAL MANGEMENT BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'M'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C* NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C* CODE IN XXCODE.
     C*
     C* GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDM        TAG
     C                   READP     PBD01                                  64
     C*
     C* VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDM
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C*** BEGIN ADD *** 10/05/99 **************************************
     C                   MOVEL     BDPC25        FPC(X)
     C*** END   ADD *** 10/05/99 **************************************
     C                   MOVEL     BDTYPE        XMTYPE            2
     C                   END                                                    64=0 M TYPE
     C*
     C                   END                                                    62=1 M TYPE
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C*** LAST- GET NETWORK TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDN        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDN
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C                   Z-ADD     0             XXAM01
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XNTYPE            2
     C                   Z-ADD     BDAM01        XXAM01                         10/17/89 ADD
     C*** BEGIN ADD *** 10/05/99 **************************************
     C                   MOVEL     BDPC25        FPC(X)
     C*** END   ADD *** 10/05/99 **************************************
     C                   END                                                    64=0 N TYPE
     C*
     C                   END                                                    62=1 N TYPE
     C*
     C                   END                                                    61=0
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 10/04/99 **************************************
     C*****************************************************************--*
     C*  EAP - IF PROVIDER BILLS INSTALLED UPDATE FAEAP.
     C*****************************************************************--*
     C*
     C     EAP           BEGSR
     C*** Beg Add *** 12/23/13 **************************************
     C     BRS           IFEQ      *ON
     C*** End Add *** 12/23/13 **************************************
     C*
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C                   MOVE      *ZEROS        XFYR
     C                   MOVE      *ZEROS        XCASE
     C                   MOVE      *ZEROS        XRCN
     C*
     C     EOSYS         IFEQ      'M'
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C                   ENDIF
     C*
     C*** DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C*
     C     XPTYPE        IFNE      *BLANKS
     C     EOSYS         ANDEQ     'P'
     C     XHTYPE        ORNE      *BLANKS
     C     EOSYS         ANDEQ     'H'
     C     XUTYPE        ORNE      *BLANKS
     C     EOSYS         ANDEQ     'U'
     C     XMTYPE        ORNE      *BLANKS
     C     EOSYS         ANDEQ     'M'
     C     CMSETU        ANDNE     'Y'
     C     *IN60         ANDEQ     *OFF
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C*
     C                   MOVEL     CMD(1)        XXPGM
     C*
     C                   SELECT
     C*
     C     EOSYS         WHENEQ    'P'
     C                   MOVE      XPTYPE        XXTYPE
     C*
     C     EOSYS         WHENEQ    'H'
     C                   MOVE      XHTYPE        XXTYPE
     C*
     C     EOSYS         WHENEQ    'U'
     C                   MOVE      XUTYPE        XXTYPE
     C*
     C     EOSYS         WHENEQ    'M'
     C                   MOVE      XMTYPE        XXTYPE
     C*
     C                   ENDSL
     C*
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXGRP#
     C                   MOVE      EOEOB#        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C                   MOVE      EOFYR         XFYR
     C                   MOVE      EOCASE        XCASE
     C                   MOVE      EORCN         XRCN
     C*
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*
     C                   ENDIF                                                  XHTYPE
     C*
     C     EOSYS         IFNE      'M'
     C*
     C     CMKEY         CHAIN(N)  PCM01                              99
     C*
     C                   ENDIF
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EABAMT EQUAL TO ZERO.
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     EABAMT        ANDEQ     *ZEROS
     C*
     C                   MOVE      'P'           ##BBLF
     C                   MOVEL     *BLANKS       ##NBLF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       ##BBLF
     C                   MOVEL     *BLANKS       ##NBLF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       ##NBLF
     C                   MOVEL     *BLANKS       ##BBLF
     C*
     C                   ENDIF
     C*
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAEA
     C*
     C* UPDATE VALUES IN FAEAP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C                   PARM      *ZEROS        ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM      *ZEROS        ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** BEGIN ADD *** 09/07/99 **************************************
18   C                   PARM      *BLANKS       ##CERT           15            PRE-CERT
19   C                   PARM      *ZEROS        ##EXT             3 0          EXTENSION
20   C                   PARM      *ZEROS        ##FEIN            9 0          PROVIDER FEIN
21   C                   PARM      *ZEROS        ##MOD             3 0          PROVIDER MOD
22   C                   PARM      *ZEROS        ##BIDT            7 0          BRS INV DT
23   C                   PARM      'Y'           ##BPFL            1
     C*** END   ADD *** 09/07/99 **************************************
     C*** BEGIN ADD *** 11/12/99 **************************************
24   C                   PARM      'B'           ##MODE            1
     C*** END   ADD *** 11/12/99 **************************************
     C*
     C* UPDATE EOSTAT WITH 'P' SO THAT THIS EOMB DOES NOT GET
     C* PROCESSED AGAIN.
     C*
     C                   MOVE      'P'           EOSTAT
     C                   UPDATE    PEO01
     C*
     C* UPDATE CMSETU WITH 'Y' IF THE SYSTEM IS 'M', SO THAT ANY 'M'
     C* TYPE ACTIVITY ON THIS CASE IS ONLY PROCESSED ONCE.
     C*
     C     EOSYS         IFEQ      'M'
     C*** BEGIN ADD *** 02/09/01 **************************************
     C     *IN60         ANDEQ     *OFF
     C*** END   ADD *** 02/09/01 **************************************
     C*
     C                   MOVEL     'Y'           CMSETU
     C*
     C                   UPDATE    PCM01
     C*
     C                   ENDIF
     C*
     C*** Beg Add *** 12/23/13 ****************************************
     C                   ENDIF
     C*** End Add *** 12/23/13 ****************************************
     C                   ENDSR
     C*** END   ADD *** 10/04/99 **************************************
     O*** BEGIN DEL *** 10/26/98 **************************************
     O**********PCM01   E                PCM01E
     O*** END   DEL *** 10/26/98 **************************************
**
N93XXR
