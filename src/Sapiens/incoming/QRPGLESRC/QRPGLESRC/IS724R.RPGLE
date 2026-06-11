     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS724R   UPDATE THE DOCUMENT AND FOLDER INFO FROM VALID SCAN
     H*** AUTO SCAN!!!
     H*****************************************************************
     H*                                                               *
     H*                                                               *
     H*** * VERIFY THE INDEX !!!!                                     *
     H*                                                               *
     H*** * CREATE THE DARTS RECORD FOR INDEX                         *
     H*                                                               *
     H* * * WRITE A DO RECORD                                         *
     H*                                                               *
     H* * * UPDATE THE DI RECORD WITH THE NEXT AVAILABLE DOC ID IDX   *
     H*       AND THE NEXT BATCH ID IF NEEDED.                        *
     H*                                                               *
     H* * * UPDATE THE FB RECORD WITH THE CUR # OF DOCS IN SUBFLR     *
     H*       AND PROVIDE THE CREATE OF NEW SUBFLR IF MAX HAS BEEN    *
     H*       REACHED                                                 *
     H*
     H* * * ASSIGN TO INDEX QUEUE BASED ON DOC TYPE IF NOT ALREADY    *
     H* * * INDEXED OR A SINGLE SCAN!!!
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 10/17/11 C:279 Project: SY195                                 *
     H*               - Add CALL to API ISA700R to remove direct      *
     H*                 access to IMDOP.                              *
     H*                                                               *
     H* 03/28/11 C:62 PROJECT 74834                                   *
     H*               ASSIGN CMCDAT.                                  *
     H*                                                               *
     H* 01/06/08 C:62 PROJECT 114IRZ                                  *
     H*               ADD AUTO INDEX PROCESSING                       *
     H*                                                               *
     H* 05/30/08 C:62 PROJECT DR257                                   *
     H*               ADD ROLLOVER PROCESSING                         *
      *
     F* 07/09/07 C:191 PROJECT: 10IJI                                *
     F*               -Write to IMIEP & IMDOP the Rcvd Date that     *
     F*                has been captured in IS610R.                  *
     F*                                                              *
      * 12/23/03 c:128 Proj: 129IEW
      *                Added comment date to screen to allow dates
      *                other than today's date.
      *                Added date parm to *Entry Parms to allow the
      *                entered date to be written to the file as
      *                CMCDAT. Do not diary if remark code diary type
      *                flag = 'N'.                                    *
     H*                                                               *
     H*  10/16/00 C:128 SUPPORT: FX081 #24490                         *
     H*                 Changed logical file from SMKTL1 to SMKTL3    *
     H*                 to use correct key.                           *
     H*                                                               *
     H*  01/27/00 C:113 PROJECT: DR095                                *
     H*                 Changed program to use SMKTP instead of IMITP.*
     H*                                                               *
     H*  01/04/00 C:113 PROJECT: SUPP                                 *
     H*                 Corrected date conversion for storage.        *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
      * DoCument INDEX CONTROL FILE
     FIMDIL     UF   E           K DISK
     F*
     F*
      * FOLDER NAMING CONTROL FILE
     FIMFBL1    UF   E           K DISK
     F*
     F*
      * Document TRANSACTION FILE
     F***** BEG  DEL ***  10/17/11  **********************************
     F*****IMDOP   O   E                    DISK
      * Document INDEX TRANSACTION FILE
     F*****IMDOP1  O   E                    DISK
     F***** END  DEL ***  10/17/11  **********************************
     F*
     F*
      * INDEX QUEUE FILE
     FIMIEP     O    E             DISK
     F*
     F*
      * Document TYPE
     FIMDTL     IF   E           K DISK
     F*
     F*
      * INDEX TYPE FILE
     F*** BEGIN CHG *** 01/27/00 ***********************************************
     F***IMITL1  IF  E           K        DISK
     F*** END   CHG *** 01/27/00 ***********************************************
     F*** BEGIN CHG *** 10/16/00 ***********************************************
     F***SMKTL1  IF  E           K        DISK
     FSMKTL3    IF   E           K DISK
     F*** END   CHG *** 10/16/00 ***********************************************
     F*
     FSDCML     O    E             DISK
     FSDCMLI    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0I)
      * DOC   STATUS TABLE FILE
     FITDSL     IF   E           K DISK
      * DOCUMENT NOTES FILE
     FIMDNL     IF A E           K DISK
     F*
     F*** Begin Chg *** 12/23/03 ***********************************************
      * Remark Code File
     FSTCTL1    IF   E           K DISK
     F*** End   Chg *** 12/23/03 ***********************************************
     F*** BEGIN ADD *** 01/06/08 **************************************
     FIMRAP     O    E             DISK
     F*** END   ADD *** 01/06/08 **************************************

     D FC              S             75    DIM(5) CTDATA PERRCD(1)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XCURDT                 1      8  0
     D  XCURYR                 1      4  0
     D  XCURMD                 5      8  0
     D*
     D                 DS
     D  D2DTTM                 1     14  0
     D  D2TIME                 1      6  0
     D  D2DATE                 7     14  0
     D*
     D                 DS
     D  XXA75                  1     75
     D  XXBSTS                24     25
     D  XXBM                  24     25
     D  XXSLH1                26     26
     D  XXBD                  27     28
     D  XXHYPH                28     28
     D  XXSLH2                29     29
     D  XXBYR                 30     33
     D  XXBTMH                39     40
     D  XXCOL1                41     41
     D  XXBTMM                42     43
     D  XXCOL2                44     44
     D  XXBTMS                45     46
     D  XXLDSC                31     70
     D  XXBUSR                24     33
     D  XXBERR                24     75
     D*
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      ***** END ADD ***** 10/17/11 ***********************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPCM0I
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#

     C*
     C*
     C*** DOCUMENT INDEX CTL FILE
     C*
     C     DIKY1         KLIST
     C                   KFLD                    QQWSID
     C*
     C*** FOLDER BALANCE FILE
     C*
     C     FBKY1         KLIST
     C                   KFLD                    QQCIF
     C*
     C*** DOCUMENT TYPE FILE
     C*
     C     DTKY1         KLIST
     C                   KFLD                    IEDOCT
     C*
     C     DTKY2         KLIST
     C                   KFLD                    QQDOCT
     C*
     C*
     C*** INDEX TYPE FILE
     C*
     C*** BEGIN CHG *** 01/27/00 ***********************************************
     C********** ITKY1     KLIST
     C     KTKY1         KLIST
     C*** END   CHG *** 01/27/00 ***********************************************
     C                   KFLD                    XXIDXP
     C*
     C*
     C     CMKYA         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    ##SEQ#            5 0
     C*
     C*
     C*
     C*** BATCH STATUS TABLE FILE
     C*
     C     DSKY1         KLIST
     C                   KFLD                    QQBSTS
     C*
     C*
     C*** DOC   NOTES (HISTORY) FILE
     C*
     C     DNKY1         KLIST
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                KFLD           DODCID
     C                   KFLD                    QQDCID
     C***** END  CHG ***  10/17/11  **********************************
     C                   KFLD                    XXASQ#            5 0
     C*
     C*
     C     DNKY2         KLIST
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                KFLD           DODCID
     C                   KFLD                    QQDCID
     C***** END  CHG ***  10/17/11  **********************************
     C*** Begin Chg *** 12/23/03 ***********************************************
     C*
     C*** Remark Code File
     C*
     C     CTKY1         KLIST
     C                   KFLD                    DTRMCD
     C*
     C*** End   Chg *** 12/23/03 ***********************************************
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQWSID            2              SCAN STA ID     1
     C                   PARM                    QQDSQ#            6              DOC SEQ#        2
     C                   PARM                    QQBCHS            4              BATCH SEQ #     3
     C                   PARM                    QQCIF             8              CURR IMAGE FLR  4
     C                   PARM                    QQCISF           12              CURR IMAGE SFLR 5
     C                   PARM                    QQ#DCS            5              # OF DOCS PRCSD 6
     C                   PARM                    QQDOCT            8              DOC TYPE        7
     C*
     C                   PARM                    QQDCID           12              DOC ID          8
     C                   PARM                    QQBYTD            9              BYTES IN DOC    9
     C                   PARM                    QQSDAT            8              SCAN DATE      10
     C                   PARM                    QQSTIM            6              SCAN TIME      11
     C                   PARM                    QQBCHI           12              BATCH ID   12
     C                   PARM                    QQPGS             5              # OF PAGES 13
     C                   PARM                    QQKEY            40              DARTS KEY  14
     C                   PARM                    QQSEQ#            5 0            DARTS SEQ# 15
     C                   PARM                    QQRTCD            1                         16
      *** Begin Add *** 12/23/03 ***********************************************
     C                   PARM                    QQCDAT            6              mmddyy(in) 17
      *** Bnd   Add *** 12/23/03 ***********************************************
     *** BEG ADD ********** 07/09/07 ********************************
     C                   PARM                    QQRCVD            8             RECEIVED DATE
     *** END ADD ********** 07/09/07 ********************************
     C*
      *** BEGIN ADD *** 01/04/00 ***********************************************
      *
      ** Date check/conversion parameters
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *** END   ADD *** 01/04/00 ***********************************************
     C***** BEG  ADD ***  10/17/11  **********************************
     C     ISAKEY        PLIST
     C                   PARM                    QQDCID
     C                   PARM                    QQCIF
     C                   PARM                    QQCISF
     C                   PARM                    QQOSYS            1
     C                   PARM                    QQLBN            10
     C                   PARM                    QQOFLD            8
     C                   PARM                    QQOSFL           12
     C                   PARM                    QQODCI           12
     C                   PARM                    QQARQS            4 0
     C                   PARM                    QQDOCT
     C                   PARM                    QQBYT#            5 1
     C                   PARM                    QQWSID
     C                   PARM                    QQSDT             8 0
     C                   PARM                    QQSTMD            6 0
     C                   PARM                    QQSREP            8
     C                   PARM                    QQBCHI
     C                   PARM                    QQPGS#            4 0
     C                   PARM                    QQAIFL            1
     C                   PARM                    QQOPTF            1
     C                   PARM                    QQIDFX            1
     C                   PARM                    QQVOL1           10
     C                   PARM                    QQVOL2           10
     C                   PARM                    QQVOL3           10
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ#
     C                   PARM                    QQFL01            1
     C                   PARM                    QQFL02            1
     C                   PARM                    QQFL03            1
     C                   PARM                    QQFL04            1
     C                   PARM                    QQFL05            1
     C                   PARM                    QQCHR5            5
     C                   PARM                    QQDSTS            2
     C                   PARM                    QQRCV#            8 0
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQINDX            1
     C                   PARM                    QQRTRN            1
     C***** END  ADD ***  10/17/11  **********************************
      ***** BEG ADD ***** 10/17/11 ***********************************
      * GET ROOT
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
      ***** END ADD ***** 10/17/11 ***********************************
     C                   TIME                    DSDTTM
      *** BEGIN DEL *** 01/04/00 ***********************************************
     C********** DSDATE    MULT 10000.01  XXDT    60       YY/MM/DD
     C**********           MOVE DSTIME    XXTM    60
     C**********
     C**********           MOVE XXDT      ##DATE  6
      *** END   DEL *** 01/04/00 ***********************************************
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
     C     QQCIF         CAT       '/'           ##9A              9
     C     ##9A          CAT(P)    QQCISF        QQPTH            60
     C******
     C*** GET THE DOC TYPE RCD, TO GET THE INDEX PROGRAM TO CHAIN TO THE
     C*** IDX TYPE FILE (IMITP), TO GET THE PGM TO CALL FOR VERIFICATION
     C*** ITVPGM?
     C*
     C*    VERIFY THE INDEX PASSED IN, IF VALID CREATE THE DARTS RECORD,
     C***  IF NOT, PROCESS LIKE THE HIGH SPEED OPTION.
     C******
     C*
     C     DTKY2         CHAIN     PDT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     '4'           QQRTCD
     C                   GOTO      EXIT
     C                   ENDIF
     C*
     C                   MOVEL     DTIDXP        XXIDXP           10
     C*
     C*** GET THE VERIFY PROGRAM TO CALL!!!!
     C*
     C*** BEGIN CHG *** 01/27/00 ***********************************************
     C********** ITKY1     CHAINPIT01                60
     C********** *IN60     IFEQ *OFF
     C**********           CALL ITVPGM
     C*
     C     KTKY1         CHAIN     PKT01                              60
     C     *IN60         IFEQ      *OFF
     C                   CALL      KTVPGM
     C*** END   CHG *** 01/27/00 ***********************************************
     C                   PARM                    QQKEY
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         IFNE      *BLANK
     C                   MOVEL     *BLANKS       QQKEY
     C                   ELSE
     C*** BEGIN ADD *** 01/06/08 **************************************
     C*
     C* DO WE RESTRICT AUTO INDEX?
     C*
     C     DTFL01        IFNE      'Y'
     C*
     C* AUTO INDEX
     C*
     C*** END   ADD *** 01/06/08 **************************************
     C                   EXSR      WCOM
     C*** BEGIN ADD *** 01/06/08 **************************************
     C                   ELSE
     C*
     C* DO NOT AUTO INDEX
     C*
     C                   EXSR      BARIDX
     C                   MOVEL     *BLANKS       QQKEY
     C                   MOVE      0             QQSEQ#
     C                   ENDIF
     C*** END   ADD *** 01/06/08 **************************************
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL     *BLANKS       QQKEY
     C*
      *
     C                   ENDIF
     C*
     C*
      *================================================================
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVELQQDCID    DODCID
     C*****                MOVELQQCIF     DOFLR
     C*****                MOVELQQCISF    DOSFLR
     C*****                MOVEL*BLANKS   DOOSYS
     C*****                MOVEL*BLANKS   DOLIBN
     C*****                MOVEL*BLANKS   DOOFLD
     C*****                MOVEL*BLANKS   DOOSFL
     C*****                MOVEL*BLANKS   DOODCI
     C*****                MOVE *ZEROS    DOARQS
     C*****                MOVELQQDOCT    DODTYP
     C*****                MOVE *ZEROS    DOARQS
      *
     C*****                MOVE QQBYTD    ##BYTD  90
     C*****      ##BYTD    DIV  1000      DOBYTD
      *
      *
      *
     C*********************MOVE QQBYTD    DOBYTD
     C*****                MOVELQQWSID    DOWSID
     C*****                MOVE QQSDAT    DOSDT
     C*****                MOVE QQSTIM    DOSTMD
     C*****                MOVELQ1USER    DOSREP
     C*****                MOVELQQBCHI    DOBCHI
     C*****                MOVE QQPGS     DOPGS
     C*****                MOVEL*BLANKS   DOOPTF
     C*
     C*****      QQKEY     IFNE *BLANKS
     C*****                MOVEL'Y'       DOAIFL
     C*****                MOVEL'Y'       DOIDFX
     C*****                ELSE
     C*****                MOVEL'N'       DOIDFX
     C*****                MOVEL*BLANKS   DOAIFL
     C*****                ENDIF
     C*
     C*****                MOVEL*BLANKS   DOVOL1
     C*****                MOVEL*BLANKS   DOVOL2
     C*****                MOVEL*BLANKS   DOVOL3
     C*****                MOVELQQKEY     DODKEY
     C*****                MOVE QQSEQ#    DODSQ#
     C*****                MOVEL*BLANKS   DOLPGM
     C*****                MOVEL*BLANKS   DOLUSR
     C*****                MOVE *ZEROS    DOLDT
     C*****                MOVE *ZEROS    DOLTIM
     C*
     C*****                MOVELQ1USER    DOIUSR
     C*****                MOVELQ1PGM     DOIPGM
     C*****                TIME           DSDTTM
     C*****      DSDATE    MULT 10000.01  DOIDT
     C*****                Z-ADDDSTIME    DOITIM
     C*
     C*****      DOIDFX    IFEQ 'N'
     C*****                MOVEL'SC'      DODSTS
     C*****                ELSE
     C*****                MOVEL'CM'      DODSTS
     C*****                MOVEL'CM'      QQBSTS  2
     C*****                ENDIF
     *** BEG ADD ********** 07/09/07 ********************************
     C*****      Q1PRMS    IFGT 17
     C*****                MOVE QQRCVD    DORCVD
     C*****                ENDIF
     *** END ADD ********** 07/09/07 ********************************
     C*
     C*****                WRITEPDO01
     C*
     C*****      QQKEY     IFNE *BLANKS
     C*****                WRITEPDO011
     C*****                ENDIF
     C*
     C*****      DODSTS    CASEQ'CM'      BNOTES
     C*****                ENDCS
     C*
     C                   MOVE      QQSDAT        QQSDT
     C                   MOVE      QQSTIM        QQSTMD
     C                   MOVEL     Q1USER        QQSREP
     C                   MOVE      QQPGS         QQPGS#
     C                   MOVEL     *BLANKS       QQOPTF
     C                   MOVEL     *BLANKS       QQOSYS
     C                   MOVEL     *BLANKS       QQLBN
     C                   MOVEL     *BLANKS       QQOFLD
     C                   MOVEL     *BLANKS       QQOSFL
     C                   MOVEL     *BLANKS       QQODCI
     C                   MOVE      QQBYTD        ##BYTD            9 0
     C     ##BYTD        DIV       1000          QQBYT#
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     'Y'           QQAIFL
     C                   MOVEL     'Y'           QQIDFX
     C                   ELSE
     C                   MOVEL     'N'           QQIDFX
     C                   MOVEL     *BLANKS       QQAIFL
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       QQVOL1
     C                   MOVEL     *BLANKS       QQVOL2
     C                   MOVEL     *BLANKS       QQVOL3
     C                   MOVE      'Y'           QQINDX
     C*
     C     QQIDFX        IFEQ      'N'
     C                   MOVEL     'SC'          QQDSTS
     C                   ELSE
     C                   MOVEL     'CM'          QQDSTS
     C                   MOVEL     'CM'          QQBSTS            2
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      17
     C                   MOVE      QQRCVD        QQRCV#
     C                   ENDIF
     C*
     C* Call API to write Document and Folder Info
     C                   CALL      'ISA700R'     ISAKEY
     C*
     C     QQDSTS        CASEQ     'CM'          BNOTES
     C                   ENDCS
     C***** END  CHG ***  10/17/11  **********************************
     C*
      * * * ====================================================== * * *
     C*
      * * * IF QQKEY IS = *BLANKS, WE MUST SET THIS FOR INDEXING QUEUE
     C*
     C     QQKEY         IFEQ      *BLANKS
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVELDODCID    IEDCID
     C*****                MOVELDODTYP    IEDOCT
     C                   MOVEL     QQDCID        IEDCID
     C                   MOVEL     QQDOCT        IEDOCT
     C***** END  CHG ***  10/17/11  **********************************
     C*
     C     DTKY1         CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DTDQUE        IEQID
     C                   ELSE
     C                   MOVEL     '4'           QQRTCD
     C                   GOTO      EXIT
     C                   ENDIF
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVE DOSDT     IESDT
     C*****                MOVE DOSTMD    IESTM
     C                   MOVE      QQSDT         IESDT
     C                   MOVE      QQSTMD        IESTM
     C***** END  CHG ***  10/17/11  **********************************
     C                   MOVEL     Q1USER        IESREP
     C                   MOVEL     'A'           IESTAT
     C*
     C                   MOVEL     Q1USER        IEIUSR
     C                   MOVEL     Q1PGM         IEIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      IEIDT
     C                   Z-ADD     DSTIME        IEITIM
     C                   MOVEL     *BLANKS       IELPGM
     C                   MOVEL     *BLANKS       IELUSR
     C                   MOVE      *ZEROS        IELDT
     C                   MOVE      *ZEROS        IELTIM
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVELDODSTS    IEDSTS
     C                   MOVEL     QQDSTS        IEDSTS
     C***** END  CHG ***  10/17/11  **********************************
     *** BEG ADD ********** 07/09/07 ********************************
     C                   MOVE      QQRCVD        IERCVD
     *** END ADD ********** 07/09/07 ********************************
     C*
     C                   WRITE     PIE01
     C*
     C*
     C                   ENDIF
     C*
      * * * ====================================================== * * *
     C*
     C     DIKY1         CHAIN     PDI01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      QQDSQ#        ##DSQ#            6 0
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     DISEQ#        IFEQ      999999
     C                   Z-SUB     1             DISEQ#
     C                   ELSE
     C     DISEQ#        IFEQ      -1
     C                   MOVEL     'S'           QQRTCD
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   ADD       1             DISEQ#
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C     QQBCHS        IFNE      *BLANKS
     C                   MOVE      QQBCHS        ##BCHS            4 0
     C     ##BCHS        ADD       1             DIBCHS
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        DILUSR
     C                   MOVEL     Q1PGM         DILPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      DILDT
     C                   Z-ADD     DSTIME        DILTIM
     C*
     C                   UPDATE    PDI01
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     QQRTCD        IFEQ      'S'
     C                   GOTO      EXIT
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      EXIT
      * * * ERROR - DOCUMENT IDX  RCD NOT FOUND
     C                   ENDIF
     C*
     C* * * FOLDER BALANCE ROUTINE
     C*
     C     FBKY1         CHAIN     PFB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      QQ#DCS        ###DCS            5 0
     C*
     C*** BECAUSE WE ARE PROCESSING ONE DOC AT A TIME, WE MUST ONLY ADD
     C*** 1 TO THE CURRENT # OF DOCS IN THE SUBFOLDER, IF IT WAS A BATCH
     C*** UPDATE, THEN WE COULD ADD THE TOTAL # OF DOCS TO THE C#DS FLD
     C*** AND DO IT ONLY ONCE PER BATCH!
     C**
     C***** DEL 10/13/97 **ADD  ###DCS    FBC#DS
     C** ADD NEXT LINE *** 10/13/97 **********************************
     C                   ADD       1             FBC#DS
     C*
     C     FBC#DS        IFGE      FBM#DS
     C                   EXSR      NEWSFL
     C*
     C     QQRTNC        IFNE      *BLANKS
     C                   MOVE      '3'           QQRTCD
     C                   GOTO      EXIT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PFB01
     C*
     C                   ELSE
     C*
     C                   MOVE      '2'           QQRTCD
     C                   GOTO      EXIT
      * * * ERROR - FOLDER BALANCE RCD NOT EXIST
     C                   ENDIF
     C*
     C*
      *
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*** BEGIN ADD *** 01/06/08 **************************************
     C*================================================================
     C* BARIDX - BARCODE INDEX PROCESSING
     C*================================================================
     C     BARIDX        BEGSR
     C*
     C* WRITE IMRAP RECORD
     C*
     C                   CLEAR                   PRA01
     C*
     C                   MOVEL(P)  QQDCID        RADCID
     C                   MOVEL(P)  QQKEY         RADKEY
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        RALUSR
     C                   MOVEL(P)  Q1PGM         RALPGM
     C                   Z-ADD     DSTIME        RALTIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RALDT
     C                   MOVEL(P)  RALUSR        RAIUSR
     C                   MOVEL(P)  RALPGM        RAIPGM
     C                   Z-ADD     RALTIM        RAITIM
     C                   Z-ADD     RALDT         RAIDT
     C*
     C                   WRITE     PRA01
     C*
     C                   ENDSR
     C*** END   ADD *** 01/06/08 **************************************
      *** =========================================================== ***
     C     NEWSFL        BEGSR
      *** =========================================================== ***
     C*
     C                   MOVE      FBCISF        XXSUFX            3 0
     C                   ADD       1             XXSUFX
     C                   MOVEL     FBCISF        XXPRE             9
     C                   MOVE      XXSUFX        XXSUF             3
     C     XXPRE         CAT       XXSUF         FBCISF
     C*
     C*** CALL CL PGM TO CREATE NEW SUBFOLDER
     C*
     C                   CALL      'ISADDF'
     C                   PARM                    FBCIF
     C                   PARM                    FBCISF
     C                   PARM                    QQRTNC            1
     C*
     C                   Z-ADD     0             FBC#DS
     C*
     C*
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   WCOM  SUBROUTINE - WRITE REMARK RECORD           W C O M   ***
      *====================================================================*
     C*
     C     WCOM          BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXTIME            6 0
     C                   Z-ADD     0             XXDATE            6 0
     C                   Z-ADD     0             XXSEQ#            5 0
     C                   Z-ADD     *HIVAL        ##SEQ#            5 0
     C                   Z-ADD     0             ##SECL            3 0
     C                   MOVE      'N'           ##SBYL            1
     C*
      *** BEGIN DEL *** 01/04/99 ***********************************************
     C********** DSDATE    MULT 10000.01  HHDT    60
     C**********           Z-ADDDSTIME    HHTIME  60
      *** END   DEL *** 01/04/99 ***********************************************
     C*
     C*** GET LAST SEQ# USED
     C*
     C* * * WRITE REMARKS  TO FILE * * *
     C*
     C                   MOVE      ##SECL        CMSECL
     C                   MOVE      ##SBYL        CMSBYL
     C*
     C                   MOVEL     QQKEY         CMKEY
     C                   MOVEL     *BLANKS       CMCSID                         ANY LONGER
     C*** BEGIN CHG *** 01/27/00 ***********************************************
     C**********           MOVELITDPGM    CMPNAM
     C                   MOVEL     KTPNAM        CMPNAM
     C*** BEGIN CHG *** 01/27/00 ***********************************************
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVEL     DTRMCD        CMRMCD
     C                   MOVEL     '***IMAGE'    CMDCID
     C                   MOVE      '*** '        CMDCID
     C                   MOVEL     DTDESC        CMCOMM
     C                   MOVE      ' '           CMFL01
     C****   LOAD FAX FOLDER HERE *********************
     C                   MOVEL     QQPTH         CMFOLD
      ***** BEG ADD ***** 10/17/11 ***********************************
      *
     C                   MOVEL     QQDCID        CMIDOC                             HIDDEN
     C     CMFOLD        IFNE      ' '
     C     CMDCID        ORNE      ' '
     C                   MOVEL     '/'           CMUNCN
     C     CMUNCN        CAT       ROOT:0        CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMFOLD:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMIDOC:0      CMUNCN
     C                   MOVEL     ENV           CMIFSP
     C                   ELSE
     C                   MOVE      *BLANKS       CMUNCN
     C                   MOVEL     *BLANKS       CMIFSP
     C                   ENDIF                                                  CMFOLD IFNE ' '
      ***** END ADD ***** 10/17/11 ***********************************
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C**********           Z-ADDHHDT      CMCDAT
     C**********           Z-ADDHHDT      CMTRDT
     C**********           Z-ADDHHTIME    CMTRTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      *** Begin Chg *** 12/23/03 ***********************************************
     C     Q1PRMS        IFLE      16
     C                   MOVEL     SRDTO         CMCDAT
     C                   ENDIF
      *** End   Chg *** 12/23/03 ***********************************************
     C                   MOVEL     SRDTO         CMTRDT
     C                   Z-ADD     DSTIME        CMTRTM
      *** END   CHG *** 01/04/00 ***********************************************
      *** Begin Chg *** 12/23/03 ***********************************************
      ***
     C     Q1PRMS        IFGT      16
      * Cvt input date parm from mmddyy to cyymmdd for output to file.
     C                   MOVE      *ZEROS        SRDTI
     C*** BEGIN ADD *** 03/28/11 **************************************
     C     QQCDAT        IFEQ      '000000'
     C     QQCDAT        OREQ      *BLANKS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C                   ELSE
     C*** END   ADD *** 03/28/11 **************************************
     C                   MOVE      QQCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C*** BEGIN ADD *** 03/28/11 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/28/11 **************************************
     C                   ENDIF
      *** End   Chg *** 12/23/03 ***********************************************
     C*
     C*
     C*
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANKS       CMIQUE                             HIDDEN
     C                   MOVEL     QQPTH         CMIPTH                             HIDDEN
     C                   MOVE      *ZERO         CMPIDX                             HIDDEN
     C                   MOVEL     *BLANKS       CMAPID                             HIDDEN
     C                   MOVEL     QQDCID        CMIDOC                             HIDDEN
     C                   MOVEL     *BLANKS       CMQPTY                             HIDDEN
     C                   MOVEL     *BLANKS       CMDCTP                             HIDDEN
     C                   MOVEL     'Y'           CMIMGF                             HIDDEN
     C*
     C*
     C                   MOVEL     *BLANK        CMSTAT
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C*** ELSE DEFAULT TO CURRENT USER
     C                   MOVEL     Q1USER        CMUSER
     C*
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C********** DSDATE    MULT 10000.01  CMDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C                   Z-ADD     DSTIME        CMTIME
      *** Begin Chg *** 12/23/03 ***********************************************
     C*******              MOVE 'Y'       CMDIFL
     C*
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        CMDIFL
     C                   ENDIF
     C*                                                    DFT LOC=DASD
      *** End   Chg *** 12/23/03 ***********************************************
     C                   MOVEL     'D'           CMFL05
     C*
     C                   MOVE      'N'           CMFL02
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C********** DSDATE    MULT 10000.01  CMIDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C*
     C                   Z-ADD     *ZERO         CMDCDT
     C                   MOVEL     Q1PGM         CMIPGM
     C*
     C                   MOVE      *BLANKS       CMSMCD
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C*
     C*** GET LAST SEQ# USED
     C*
     C****  *****   ****
     C*
     C     CMKYA         SETGT     PCM0I
     C                   READP     PCM0I                                  60
     C     *IN60         IFEQ      '0'
     C     QQKEY         ANDEQ     PIKEY
     C     PISEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     0             CMSEQ#
     C                   END
     C*
     C                   WRITE     PCM01
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   MOVE      CMSEQ#        QQSEQ#
     C*
     C*
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*
     C*
      *===================================================================
      ***   BNOTES   -     CREATE BATCH NOTE                         *****
      *===================================================================
     C*
     C     BNOTES        BEGSR
     C*
     C                   TIME                    D2DTTM
     C                   MOVE      D2DATE        XCURYR
     C                   MOVEL     D2DATE        XCURMD
     C*
     C                   Z-ADD     *HIVAL        XXASQ#
     C     DNKY1         SETGT     PDN01
     C     DNKY2         READPE    PDN01                                  60
     C     *IN60         IFEQ      *OFF
     C     1             ADD       DNASQ#        XXASQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXASQ#
     C                   ENDIF
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVELDODCID    DNDCID
     C                   MOVEL     QQDCID        DNDCID
     C***** END  CHG ***  10/17/11  **********************************
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C                   MOVE      XCURDT        DNCDAT
     C*
     C                   MOVEL     Q1PGM         DNPNAM                         12/31/91 CHG
     C                   MOVEL     XCURDT        DNTRDT
     C                   MOVE      D2TIME        DNTRTM
     C*** SECURITY VALUES NOT CURRENTLY USED ******
     C                   MOVE      *ZEROS        DNSECL
     C                   MOVEL     *BLANKS       DNSBYL
     C*** PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C                   MOVEL     'H'           DNPRFL
     C*
     C                   MOVEL     *BLANKS       DNSTAT
     C                   MOVEL     Q1USER        DNIUSR
     C                   MOVEL     Q1PGM         DNIPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        DNITIM
     C     DSDATE        MULT      10000.01      DNIDT
     C*
     C                   MOVEL     *BLANKS       DNLUSR
     C                   MOVEL     *BLANKS       DNLPGM
     C                   MOVE      *ZEROS        DNLTIM
     C                   MOVE      *ZEROS        DNLDT
     C*
     C**** FORMAT THE COMMENT ****
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(1)         XXA75            75
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(2)         XXA75            75
     C*
     C*** GET THE BATCH STATUS TABLE RCD. FOR THE DESCRIPTION....
     C*
     C     DSKY1         CHAIN     PDS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     DSLDSC        XXLDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXLDSC
     C                   ENDIF
     C*
     C                   MOVEL     '-'           XXHYPH            1
     C                   MOVEL     QQBSTS        XXBSTS            2
     C*
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(3)         XXA75            75
     C*
     C*
     C                   MOVEL     D2DATE        XXBM              2
     C                   MOVE      D2DATE        XXBYR             4
     C                   MOVE      D2DATE        XXCHR8            8
     C     2             SUBST     XXCHR8:3      XXBD              2
     C*
     C                   MOVEL     D2TIME        XXBTMH            2
     C                   MOVE      D2TIME        XXBTMS            2
     C                   MOVE      D2TIME        XXCHR6            6
     C     2             SUBST     XXCHR6:3      XXBTMM            2
     C*
     C                   MOVEL     '/'           XXSLH1            1
     C                   MOVEL     '/'           XXSLH2            1
     C                   MOVEL     ':'           XXCOL1            1
     C                   MOVEL     ':'           XXCOL2            1
     C*
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C*
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(4)         XXA75            75
     C*
     C*
     C*
     C                   MOVEL     Q1USER        XXBUSR           10
     C*
     C*
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C*
     C*
     C*
     ***********************************************************
     C     ENDBN         ENDSR
     C*
**   RMRK ENTRIES      45 7  01       90 23 56
Doc.  History:
   Mass Chg Status   : xx  -  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
            Date/Time: xx/xx/xxxx     xx:xx:xx
            User     : xxxxxxxxxx
            Error    : xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xx
