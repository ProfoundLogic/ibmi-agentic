     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*    PROGRAM: N7720R                                           **
     H*    DESCRIPTION: AUTO EOB/HBA RELEASE                         **
     H*                                                              **
     H* PROGRAM DESCRIPTION:                                         **
     H*       - DRIVER FILE: FAEOL                                   **
     H*       - ONE/ALL SUBSCRIBER PROCESSING (*IN80)                **
     H*       - IF # DAYS SINCE EOTRDT IS GE CRITERIA DATE,          **
     H*            THEN, ALL BILLS FOR THAT CASE ARE RELEASED.       **
     H*       - IF # BILLS (I.E. #EOMBS) IS GE CRITERIA # BILLS,     **
     H*            THEN, ALL BILLS FOR THAT CASE ARE RELEASED.       **
     H*                                                              **
     H* FILES:                                                       **
     H* FAEOL (OPEN QUERY): EOHRFL EQ 'H' (HOLD/RELEASE STATUS)      **
     H*                     EOBLFL NE 'P' (BILLING FLAG)             **
     H*                     EOTRDT LT PROMPT DATE                    **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*  REVISIONS:                                                  **
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*                  Retrieve brand Name                          *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                              **
     H*  03/18/99 C:94  PROJECT: 25MKL
     H*                 ADD RECON #
     H*
     H*  01/06/99 C:94  PROJECT: MC152                               **
     H*                 CHANGE PROGRAM TO CALL API(AA001R) TO UPDATE **
     H*                 NAMBP, NAPBP, FAHBP, AND FAEAP               **
     H*                                                              **
     H*  07/05/95 C:35  REQ/PROJ: NW008                              **
     H*                 1. ADDED MEDICAL MANAGEMENT BILLING FILE     **
     H*                 2. CONDITION DISPLAY OF SYSTEM TYPE M RECORDS**
     H*                    ON POS. 9 OF FBAL01 EQ 'Y'.               **
     H*                                                              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*  INDICATOR USAGE:                                            **
     H**  80       ALL SUBSCRIBERS (READ)                            **
     H** N80       ONE SUBSCRIBER  (READE)                           **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEG ADD *** 01/06/99 ***************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/06/99 ***************************************
     FFMCDL     IF   E           K DISK
     FFAEOL     UF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEG CHG *** 01/06/99 ***************************************
     FFAHBL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FNAMBL1    IF   E           K DISK
     F*** END CHG *** 01/06/99 ***************************************
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEG ADD *** 01/06/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/06/99 ***************************************
     FQSYSPRT   O    F  198        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
     F*

     D DS              S             30    DIM(2) CTDATA PERRCD(1)              RELEASE DESC.
     D*** BEG ADD *** 01/06/99 ***************************************
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
     D*** END ADD *** 01/06/99 ***************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D                 DS
     D  XF#                    1     14
     D  XXFND                  1      3  0
     D  XXHYP1                 4      4
     D  XXYR                   5      6  0
     D  XXHYP2                 7      7
     D  XXCASE                 8     14  0
     D                 DS
     D  DA                     1     30
     D                                     DIM(30)                              DESC. ARRAY
     D  XD#DAY                23     25  0
     D  XD#BLL                11     15  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 01/06/99 ***************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0

     I*** END ADD *** 01/06/99 ***************************************

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE EOB TRANS FILE
     C*
     C     EOKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE PRE/RE-CERT TRANS FILE
     C*
     C     PBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   KEY LIST FOR THE MEDICAL MANAGEMENT BILLING FILE
     C*
     C     MBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   KEY LIST FOR THE HBA TRANS FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*** BEG ADD *** 01/06/99 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*** END ADD *** 01/06/99 ***************************************
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQRDAT            8
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
     C* INITIALIZATION SECTION
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*
     C*** BEG ADD *** 01/06/99 ***************************************
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
     C*** END ADD *** 01/06/99 ***************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C*
     C                   MOVE      QQRDAT        XXRDAT            8 0
     C*
     C                   Z-ADD     XXRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1RDAT            6 0
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C**********           Z-ADD9         T       50
     C********** 1         SUBSTFBAL01:T  XXMFLG  1
     C**********           ELSE
     C**********           MOVE 'N'       XXMFLG
     C**********           END
     C*** END   DEL *** 08/15/00 **************************************
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*  DETERMINE IF ONE SUBSCRIBER OR ALL SUBSCRIBERS
     C*
     C     XXSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN80                          ALL SUBSC
     C                   ELSE
     C                   MOVE      '0'           *IN80                          ONE SUBSC
     C                   END
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*** GET ROUTING INFORMATION FOR REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         MOVE      '0'           XXPRT
     C**********     ORKEY         CHAIN     POR01                              60
     C**********     *IN60         IFEQ      '0'
     C**********         EXCEPT    ROUTE
     C**********         END                                                    END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*  READ EOB OPEN QUERY FILE (ALL HELD RECORDS/NOT PRINTED YET)
     C*
     C   80              READ      PEO02                                  60     ALL SUBSC
     C  N80EOKEY1        CHAIN     PEO02                              60         ONE SUBSC
     C*
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     EOCO#         XXCO#             3 0
     C                   Z-ADD     EOSUB#        XXSUB#            5 0
     C                   Z-ADD     EOFND         XXFND             3 0
     C                   Z-ADD     EOFYR         XXFYR             3 0
     C                   Z-ADD     EOFYR         XXYR              2 0
     C                   Z-ADD     EOCASE        XXCASE            7 0
     C                   Z-ADD     0             CASCNT            1 0
     C*
     C*  CHAIN TO COMPANY MASTER FILE - GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              51
     C     *IN51         IFEQ      '0'
     C*                    -----***REMOVE LEADING BLANKS FROM COMPANY NAME
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    CONM(Z)                            52
     C                   MOVEA     CONM(Z)       XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*  GET THE RELEASE CRITERIA FROM THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FN#DAY        XX#DAY            3 0
     C                   MOVE      FN#BLL        XX#BLL            5 0
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XXSBNM           40
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXSR      SRCINF                                       NW CASE INFO
     C*
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     EOSYS         IFEQ      'M'
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXMFLG    ANDNE'Y'
     C     ##MED         ANDNE     'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C                   GOTO      NXTRD
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   ADD       1             CASCNT
     C*                                                                AME
     C*** IF CASE CHANGES, PRINT CASE TOTALS
     C*
     C     EOCASE        IFNE      XXCASE
     C     XXCASE        ANDNE     *ZEROS
     C                   EXSR      SRCASE                                       CASE TOTALS
     C                   EXSR      SRCINF                                       NW CASE INFO
     C                   END                                                    ENDNE XXCASE
     C*
     C*** IF GROUP CHANGES, PRINT GROUP TOTALS
     C*
     C     EOFND         IFNE      XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   END                                                    ENDNE XXFND
     C*
     C*** IF SUBSCRIBER CHANGES,PRINT SUBSCRIBER TOTALS,
     C*** PRINT PAGE BREAK HEADINGS
     C*
     C     EOSUB#        IFNE      XXSUB#
     C     XXSUB#        ANDNE     *ZEROS
     C     *IN93         IFEQ      '0'
     C                   EXCEPT    MSG                                          NO RELEASES
     C                   END                                                    END *IN93
     C                   EXSR      SRSUBS                                       SUBS TOTALS
     C                   END                                                    ENDNE XXSUB#
     C*
     C*  DETERMINE # OF DAYS SINCE TRANSACTION DATE
     C*
     C                   EXSR      SRDAYS
     C*
     C* ONLY PROCESS AGAINST ONE RELEASE CRITERIA OR THE OTHER
     C*
     C     XX#DAY        IFNE      *ZEROS
     C*
     C*  IF # OF DAYS > OR = CRITERIA, RELEASE ALL RECORDS FOR CASE
     C*
     C     QQDAYS        IFGE      XX#DAY
     C                   MOVE      '1'           *IN99                          >/= 30 DAYS
     C                   EXSR      SRREL
     C                   GOTO      NXTRD
     C                   END                                                    ENDGE XX#DAY
     C*
     C                   ELSE                                                   XX#DAY = 0
     C*
     C*  IF # OF BILLS > OR = CRITERIA, RELEASE ALL RECORDS FOR CASE
     C*
     C     CASCNT        IFGE      XX#BLL
     C                   EXSR      SRREL
     C                   GOTO      NXTRD
     C                   END                                                    ENDGE XX#BLL
     C*
     C                   END                                                    END XX#DAY
     C*
     C*  READ NEXT RECORD
     C*
     C     NXTRD         TAG
     C   80              READ      PEO02                                  60     ALL SUBSC
     C  N80EOKEY1        READE     PEO02                                  60     ONE SUBSC
     C*
     C                   END                                                    END DOWEQ 60
     C*
     C*
     C                   EXSR      SRCASE
     C                   EXSR      SRGRP
     C                   MOVE      '1'           *INLR
     C                   EXSR      SRSUBS
     C*
     C     *IN93         IFEQ      '0'
     C                   EXCEPT    MSG
     C                   END
     C*
     C****  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************--*
     C*  PERFORM CASE TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRCASE        BEGSR
     C                   ADD       XXTCAS        XXTGRP            4 0          GRP # REL
     C                   ADD       XXCAMT        XXGAMT           10 2
     C*
     C     *IN90         IFEQ      '1'                                          SRREL DONE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    CASTOT
     C*
     C                   MOVE      '1'           *IN91                          SRCASE DONE
     C*
     C                   END                                                    END IF *IN90
     C*
     C     *INLR         IFEQ      '0'
     C     EOFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   END
     C*
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOFYR         XXYR
     C                   Z-ADD     EOCASE        XXCASE
     C                   Z-ADD     0             CASCNT
     C                   Z-ADD     0             XXCAMT
     C                   MOVE      '0'           *IN90
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  CASE INFORMATION FOR REPORT
     C*****************************************************************
     C     SRCINF        BEGSR
     C*
     C*  CHAIN TO CASE MASTER FILE - GET CLAIMANT SS#
     C*
     C     CMKEY         CHAIN     PCM01                              61
     C     *IN61         IFEQ      '0'
     C*
     C*  CHAIN TO CLAIMANT DEMOGRAPHICS - GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              62
     C     *IN62         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        XXCNAM           30
     C                   ELSE
     C                   MOVEL     *BLANKS       XXCNAM
     C                   END
     C*
     C                   ELSE                                                   ELSEIF *IN61
     C                   MOVE      *BLANKS       XXCNAM
     C                   END                                                    END IF *IN61
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM SUBSCRIBER TOTAL CALCULATIONS AND OUTPUT
     C*****************************************************************--*
     C*
     C     SRSUBS        BEGSR
     C*
     C                   ADD       XXTSUB        XXTALL            4 0          TOTAL # REL
     C*
     C     *IN92         IFEQ      '1'                                          SRGRP DONE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    SUBTOT
     C*
     C                   END                                                    END IF *IN92
     C*
     C                   Z-ADD     EOSUB#        XXSUB#
     C                   MOVE      '0'           *IN92
     C*
     C     *INLR         IFEQ      '0'
     C*
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XXSBNM
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM
     C                   END
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN93
     C*
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  CALCULATE # OF DAYS
     C*****************************************************************--*
     C*
     C     SRDAYS        BEGSR
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     XXRDAT        XQTO              8 0
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRGRP         BEGSR
     C                   ADD       XXTGRP        XXTSUB            4 0          SUBS # REL
     C                   ADD       XXGAMT        XXSAMT           10 2
     C*
     C     *IN91         IFEQ      '1'                                          SRCASE DONE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    GRPTOT
     C                   MOVE      '1'           *IN92                          SRGRP DONE
     C                   Z-ADD     0             XXGAMT
     C*
     C                   END                                                    END IF *IN91
     C*
     C     *INLR         IFEQ      '0'
     C     EOSUB#        ANDNE     XXSUB#
     C     XXSUB#        ANDNE     *ZEROS
     C                   EXSR      SRSUBS
     C                   END
     C*
     C                   Z-ADD     EOFND         XXFND
     C                   MOVE      '0'           *IN91
     C*
     C*  GET THE RELEASE CRITERIA FROM THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FN#DAY        XX#DAY            3 0
     C                   MOVE      FN#BLL        XX#BLL            5 0
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  RELEASE SUBROUTINE
     C*****************************************************************
     C*
     C     SRREL         BEGSR
     C*
     C     *IN99         IFEQ      '1'
     C                   MOVEA     DS(1)         DA
     C                   MOVE      XX#DAY        XD#DAY
     C                   ELSE
     C                   MOVEA     DS(2)         DA
     C                   MOVE      XX#BLL        XD#BLL
     C                   END
     C*
     C*  RELEASE EOB RECORDS
     C*
     C     EOKEY         CHAIN     PEO02                              50                    D
     C     *IN50         IFEQ      '0'
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     EOSYS         IFEQ      'M'
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXMFLG    ANDNE'Y'
     C     ##MED         ANDNE     'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C                   GOTO      SRRELE
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOBLFL        IFNE      'P'
     C                   MOVE      'R'           EOHRFL
     C                   UPDATE    PEO02                                                    D
     C     EOSYS         IFEQ      'P'
     C*** BEGIN ADD *** 10/11/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKEY         CHAIN     PEA01                              55
     C*
     C     *IN55         IFEQ      '0'
     C*
     C     EABBLF        IFNE      'P'
     C*
     C                   Z-ADD     EABAMT        XXBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAEA
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'R'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 10/11/99 **************************************
     C                   Z-ADD     EOBAMT        XXBAMT            9 2
     C*** BEGIN ADD *** 10/11/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/11/99 **************************************
     C                   END
     C*
     C     EOSYS         IFEQ      'U'
     C*
     C*** BEG CHG *** 01/06/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              55
     C     *IN55         IFEQ      '0'
     C     EABBLF        IFNE      'P'
     C                   Z-ADD     EABAMT        XXBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C*** BEGIN CHG *** 10/11/99 **************************************
     C**********           MOVE 'U'       XXNAPB
     C                   MOVE      'U'           XXFAEA
     C*** END   CHG *** 10/11/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'R'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     PBKEY         CHAIN     PPB01                              55
     C     *IN55         IFEQ      '0'
     C     PBBLFL        IFNE      'P'
     C                   Z-ADD     PBBAMT        XXBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAPB
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'R'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   END                                                    ENDNE 'P'
     C                   END                                                    END IFEQ 55
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*
     C*** END CHG *** 01/06/99 ***************************************
     C                   END                                                    END IFEQ 'U'
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXMFLG    IFEQ 'Y'
     C     ##MED         IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C     EOSYS         IFEQ      'M'
     C*
     C*** BEG CHG *** 01/06/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              55
     C     *IN55         IFEQ      '0'
     C     EABBLF        IFNE      'P'
     C                   Z-ADD     EABAMT        XXBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C*** BEGIN CHG *** 10/11/99 **************************************
     C**********           MOVE 'U'       XXNAMB
     C                   MOVE      'U'           XXFAEA
     C*** END   CHG *** 10/11/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'R'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     MBKEY         CHAIN     PMB01                              55
     C     *IN55         IFEQ      '0'
     C     MBBLFL        IFNE      'P'
     C                   Z-ADD     MBBAMT        XXBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAMB
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'R'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   END                                                    ENDNE 'P'
     C                   END                                                    END IFEQ 55
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/06/99 ***************************************
     C                   END                                                    END IFEQ 'U'
     C*
     C                   END
     C*
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'H'
     C*
     C*** BEG CHG *** 01/06/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              55
     C     *IN55         IFEQ      '0'
     C     EABBLF        IFNE      'P'
     C                   Z-ADD     EABAMT        XXBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C*** BEGIN CHG *** 10/11/99 **************************************
     C**********           MOVE 'U'       XXFAHB
     C                   MOVE      'U'           XXFAEA
     C*** END   CHG *** 10/11/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'R'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     HBKEY         CHAIN     PHB01                              55
     C     *IN55         IFEQ      '0'
     C     HBBLFL        IFNE      'P'
     C                   Z-ADD     HB1AAM        XXBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ****************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ****************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'R'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   END                                                    ENDNE 'P'
     C                   END                                                    END IFEQ 55
     C*
     C                   ENDIF
     C*
     C*** END CHG *** 01/06/99 ***************************************
     C                   END                                                    END IFEQ 'H'
     C*
     C                   MOVE      EOSYS         XXSYS             1
     C                   Z-ADD     EOEOB#        XXEOB#
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C                   ADD       1             XXTCAS            4 0
     C                   ADD       XXBAMT        XXCAMT           10 2
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DETL
     C*
     C                   END                                                    ENDNE 'P'
     C                   END                                                    END IF 50
     C*
     C                   MOVE      '0'           *IN99
     C                   MOVE      '1'           *IN90                          RELEASE DONE
     C                   MOVE      '1'           *IN93                          RELEASE DONE
     C*
     C     SRRELE        ENDSR
     C*
     O**
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
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
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1994, 1995'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE               8 '  /  /  '
     O                       DSTIME             127 '  :  :  '
     O                       XXCONM              84
     O          E            HEAD           2
     O                       ORPNAM              10
     O                       ORTITL              94
     O                       PAGE               127
     O                                          121 'PAGE:'
     O*
     O*
     O          E            HEAD           2
     O                                           67 'RELEASED PRIOR TO :'
     O                       X1RDAT        Y     77
     O*
     O*
     O          E            HEAD           2
     O                                           12 'SUBSCRIBER #'
     O                       XXSUB#              19
     O                                           21 '-'
     O                       XXSBNM              62
     O          E            HEAD           1
     O                                           87 'TRANS'
     O          E            HEAD           2
     O                                            8 'SYS'
     O                                           16 'GRP'
     O                                           24 'CASE'
     O                                           46 'CLAIMANT NAME'
     O                                           74 'EOB #'
     O                                           86 'DATE'
     O                                          112 'REASON FOR RELEASE'
     O          E            DETL           1
     O                       XXSYS                7
     O                       XF#                 27
     O                       XXCNAM              63
     O                       XXEOB#        Z     74
     O                       XXTRDT              88 '  /  /  '
     O                       DA                 125
     O                       XXBAMT        M    139
     O          E            CASTOT      1  2
     O                                           11 'CASE TOTAL:'
     O                                           33 '# RELEASED:'
     O                       XXTCAS        ZB    39
     O                       XXCAMT        M    139
     O          E            GRPTOT      1  3
     O                                            5 'GROUP'
     O                       XXFND                9
     O                                           16 'TOTAL:'
     O                                           33 '# RELEASED:'
     O                       XXTGRP        ZB    39
     O                       XXGAMT        M    139
     O          E            SUBTOT      1
     O                                           10 'SUBSCRIBER'
     O                       XXSUB#              16
     O                                           23 'TOTAL:'
     O                                           38 '# RELEASED:'
     O                       XXTSUB        ZB    44
     O                       XXSAMT        M    139
     O          E            MSG         3
     O                                           19 '*** NO RECORDS'
     O                                           34 'TO RELEASE ***'
     O**
     O          E            ERRMSG      3
     O                                           20 '*** RELEASE CRITERIA'
     O                                           38 'WAS NOT FOUND FOR'
     O                                           44 'FUND:'
     O                       XXFND               48
     O                                           53 '***'
** (DS)
TRANS DT GREATER THAN     DAYS
MORE THAN       BILLS FOR CASE
