     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H* PROGRAM TITLE: N7722R - NO LOST TIME AUTO RELEASE            **
     H*                                                              **
     H* PROGRAM DESCRIPTION:                                         **
     H*       - DRIVER FILE: FAEOL                                   **
     H*       - ONE/ALL SUBSCRIBER PROCESSING (*IN80)                **
     H*       - IF THERE IS ANY "NO LOST TIME" EOMB FOR A CASE,      **
     H*            THEN, ALL BILLS FOR THAT CASE ARE RELEASED.       **
     H*                                                              **
     H* FILES:                                                       **
     H* FAEOL (OPEN QUERY): EOHRFL EQ 'H' (HOLD/RELEASE STATUS)      **
     H*                     EOBLFL NE 'P' (BILLING FLAG)             **
     H*                     EONLFL EQ 'N' (NO LOST TIME FLAG)        **
     H*                     EOTRDT LT PROMPT DATE                    **
     H*                                                              **
     H*** CHANGED 02/01/89 BY CONNIE TERRY - IF THE NO LOST TIME FLAG
     H***  IS EQUAL TO N - NO TIME HAS BEEN LOST AND THE EOB SHOULD BE
     H*    AUTOMATICALLY RELEASED REGARDLESS OF OTHER CRITERIA ...   **
     H*                                                              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** REVISIONS:                                                  **
     H**                                                             **
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H** 03/18/99 C:94 PROJECT: 25MKL
     H**               ADD RECON#
     H**
     H** 01/06/99 C:94  PROJECT: MC152                               **
     H**                CHANGE PROGRAM TO USE API (AA001R)           **
     H**                TO UPDATE FAHBP AND FAEAP                    **
     H**                                                             **
     H** 7/26/89  CONNIE TERRY - CORRECT LOOPING ERROR.              **
     H**                                                             **
     H** 7/26/89  CST - ADDED PROMPT DATE TO PRINT ON HEADINGS       **
     H**                PROJECT : I0011                              **
     H**                                                             **
     H** 9/07/89  C:05- CHGD HEADING "PROMPT" DATE FROM DAILY BATCH  **
     H** PROJ #: A9C    PROMPT SCREEN DATE TO NEW PROMPT SCREEN DATE **
     H**                                                             **
     H** 10/16/89 C:05- RMVD PROCESSING OF FAEOL4 BECAUSE WE ONLY    **
     H** PROJ #: A9C    WANT TO RELEASE THOSE RECORDS THAT FALL      **
     H**                BEFORE THE PROMPTED BEFORE DATE              **
     H*                                                              **
     H*  08/26/92 C:05  PROJ: 99AAC                                  **
     H*                 Changed all instances of EOFLG2 to EOHRFL    **
     H*                 Changed all instances of EOFLG4 to EOBLFL    **
     H*                 Changed all instances of EOFLG5 to EONLFL    **
     H*                 Changed all instances of HBFLG2 to HBHRFL    **
     H*                 Changed all instances of HBPFLG to HBBLFL    **
     H*                                                              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATOR USAGE:                                            **
     H**  80       ALL SUBSCRIBERS (READ)                            **
     H** N80       ONE SUBSCRIBER  (READE)                           **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEG ADD *** 01/06/99 ***************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/06/99 ***************************************
     FFMCDL     IF   E           K DISK
     FFAEOL     UF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEG CHG *** 01/06/98 ***************************************
     FFAHBL     IF   E           K DISK
     F********** FAHBL   UF  E           K        DISK
     F*** END CHG *** 01/06/99 ***************************************
     FFMSML     IF   E           K DISK
     F*** BEG ADD *** 01/06/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/06/99 ***************************************
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
     F*

     D DS              S             30    DIM(3) CTDATA PERRCD(1)              RELEASE DESC.
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
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 01/06/99 ***************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0

     I*** END ADD *** 01/06/99 ***************************************
     I*
     I*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
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
     C*   KEY LIST FOR THE HBA TRANS FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEG ADD *** 01/06/99 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 01/06/99 ***************************************
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     SMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQDATE            8
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
     C                   MOVE      QQDATE        X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*  DETERMINE IF ONE SUBSC OR ALL SUBSCRIBERS
     C*
     C     XXSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN80                          ALL SUBSCR.
     C                   ELSE
     C                   MOVE      '0'           *IN80                          ONE SUBSCR.
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
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********     *IN60         IFEQ      '0'
     C*********          EXCEPT    ROUTE
     C*********          END                                                    END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*  READ EOB OPEN QUERY FILE (ALL HELD RECORDS/NOT PRINTED YET)
     C*
     C   80              READ      PEO02                                  60     ALL SUBSCR
     C  N80EOKEY1        CHAIN     PEO02                              60         ONE SUBSCR
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
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSC NAME
     C*
     C     SMKEY         CHAIN     PSM01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBM
     C                   END
     C*
     C*   LOAD THE RELEASE CRITERIA FROM THE FUND MASTER FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FN#DAY        XX#DAY            3 0
     C                   MOVE      FN#BLL        XX#BLL            5 0
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXSR      SRCINF                                       NW CASE INFO
     C*
     C                   ELSE
     C                   GOTO      TERM
     C                   END
     C*********************************
     C*   MAIN LINE PROGRAM LOOP
     C*********************************
     C*
     C     *IN60         DOWEQ     '0'
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
     C*** IF SUBSCR. CHANGES, PRINT SUBSCR. TOTALS, PAGE BREAK HEADINGS
     C*
     C     EOSUB#        IFNE      XXSUB#
     C     XXSUB#        ANDNE     *ZEROS
     C     *IN93         IFEQ      '0'
     C                   EXCEPT    MSG                                          NO RELEASES
     C                   END                                                    END *IN93
     C                   EXSR      SRSUB                                        SUB  TOTALS
     C                   END                                                    ENDNE XXSUB#
     C*
     C*  DETERMINE IF ANY TIME WAS LOST - IF NOT --  RELEASE EOBS.
     C*
     C     EONLFL        IFEQ      'N'
     C                   MOVE      '1'           *IN98                          NO TIME LOST
     C                   EXSR      SRREL
     C     EOKEY         SETGT     PEO02
     C                   GOTO      NXTRD
     C                   END                                                    END NO TIME LOST
     C*
     C*  DETERMINE # OF DAYS SINCE TRANSACTION DATE
     C*
     C*  READ NEXT RECORD
     C*
     C     NXTRD         TAG
     C   80              READ      PEO02                                  60     ALL COMPANY
     C  N80EOKEY1        READE     PEO02                                  60     ONE COMPANY
     C*
     C                   END                                                    END DOWEQ 60
     C*
     C*
     C****  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C                   EXSR      SRCASE
     C                   EXSR      SRGRP
     C                   EXSR      SRSUB
     C*
     C     *IN93         IFEQ      '0'
     C                   EXCEPT    MSG
     C                   END
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
     C*
     C     *IN90         IFEQ      '1'                                          SRREL DONE
     C                   ADD       XXTCAS        XXTGRP            4 0          GRP # REL
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    CASTOT
     C*
     C                   MOVE      '1'           *IN91                          SRCASE DONE
     C*
     C     *INLR         IFEQ      '0'
     C     EOFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   END
     C*
     C                   END                                                    END IF *IN90
     C*
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOFYR         XXYR
     C                   Z-ADD     EOCASE        XXCASE
     C*
     C                   Z-ADD     EOCO#         XXCO#
     C                   Z-ADD     EOSUB#        XXSUB#
     C                   Z-ADD     EOFND         XXFND
     C*
     C                   Z-ADD     0             CASCNT
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
     C*  PERFORM SUBSCRIBER TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRSUB         BEGSR
     C*
     C     *IN92         IFEQ      '1'                                          SRGRP DONE
     C*
     C                   ADD       XXTSUB        XXTALL            4 0          TOTAL # REL
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
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSC NAME
     C*
     C     SMKEY         CHAIN     PSM01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBM
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
     C                   Z-ADD     DSDATE        XQTO              8 0
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
     C*
     C     *IN91         IFEQ      '1'                                          SRCASE DONE
     C*
     C                   ADD       XXTGRP        XXTSUB            4 0          SUBS # REL
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    GRPTOT
     C                   MOVE      '1'           *IN92                          SRGRP DONE
     C*
     C     *INLR         IFEQ      '0'
     C     EOSUB#        ANDNE     XXSUB#
     C     XXSUB#        ANDNE     *ZEROS
     C                   EXSR      SRSUB
     C                   END
     C*
     C                   END                                                    END IF *IN91
     C*
     C                   Z-ADD     EOFND         XXFND
     C                   MOVE      '0'           *IN91
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  RELEASE SUBROUTINE
     C*****************************************************************
     C*
     C     SRREL         BEGSR
     C*
     C     *IN98         IFEQ      '1'
     C                   MOVEA     DS(3)         DA
     C                   END
     C*
     C*  RELEASE EOB RECORDS
     C*
     C     EOKEY         CHAIN     PEO02                              50         10/16/89 ADD
B1   C     *IN50         DOWEQ     '0'
     C*
B2   C     EOBLFL        IFNE      'P'
     C                   MOVE      'R'           EOHRFL
     C                   MOVEL     'RELEASE'     EOPGM
     C                   MOVE      DSTIME        EOTIME
     C                   MOVE      DSDATE        EODT
     C                   UPDATE    PEO02                                         10/16/89 ADD
     C*
B3   C     EOSYS         IFEQ      'H'
     C*
     C*** BEG CHG *** 01/06/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              55
     C     *IN55         IFEQ      '0'
     C     EABBLF        IFEQ      'P'
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
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
E5   C                   ENDIF                                                  ENDNE 'P'
E4   C                   ENDIF                                                  END IFEQ 55
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     HBKEY         CHAIN     PHB01                              55
B4   C     *IN55         IFEQ      '0'
B5   C     HBBLFL        IFNE      'P'
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
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
E5   C                   ENDIF                                                  ENDNE 'P'
E4   C                   ENDIF                                                  END IFEQ 55
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/06/99 ***************************************
E3   C                   END                                                    END IFEQ 'H'
     C*
     C                   MOVE      EOSYS         XXSYS             1
     C                   Z-ADD     EOEOB#        XXEOB#
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C                   ADD       1             XXTCAS            4 0
B3   C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
E3   C                   END
     C*
     C                   EXCEPT    DETL
     C*
E2   C                   END                                                    ENDNE 'P'
     C     EOKEY         READE     PEO02                                  50     10/16/89 ADD
E1   C                   END                                                    END DOWEQ 50
     C*
     C                   MOVE      '0'           *IN98                          TIME LOST *IN
     C*
     C                   MOVE      '1'           *IN90                          RELEASE DONE
     C                   MOVE      '1'           *IN93                          RELEASE DONE
     C*
     C                   ENDSR
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
     O****                               57 'INC. 1997'
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
     O                                          121 'PAGE:'
     O                       PAGE               127
     O          E            HEAD           2
     O                                           67 'RELEASED PRIOR TO :'
     O                       XXDATE        Y     77
     O*
     O          E            HEAD           2
     O                                           13 'SUBSCRIBER #:'
     O                       XXSUB#              20
     O                                           22 '-'
     O                       XXSUBM              64
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
     O          E            CASTOT      1  3
     O                                           11 'CASE TOTAL:'
     O                                           33 '# RELEASED:'
     O                       XXTCAS        ZB    39
     O          E            GRPTOT      1  3
     O                                            5 'GROUP'
     O                       XXFND                9
     O                                           16 'TOTAL:'
     O                                           33 '# RELEASED:'
     O                       XXTGRP        ZB    39
     O          E            SUBTOT      1
     O                                           10 'SUBSCRIBER'
     O                       XXSUB#              17
     O                                           26 'TOTAL:'
     O                                           38 '# RELEASED:'
     O                       XXTSUB        ZB    44
     O          E            MSG         3
     O                                           19 '*** NO RECORDS'
     O                                           34 'TO RELEASE ***'
     O**
** (DS)
TRANS DT GREATER THAN     DAYS
MORE THAN       BILLS FOR CASE
CASE FLAGGED AS NO TIME LOST
