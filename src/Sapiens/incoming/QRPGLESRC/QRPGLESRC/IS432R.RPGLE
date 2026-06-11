     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS432R   UPDATE THE DOCUMENT AND FOLDER INFO FROM SPLIT RQST
     H*****************************************************************
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
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 10/17/11 C:279 Project: SY195                                *
     H*               - Add CALL to API ISA700R to remove direct     *
     H*                 access to IMDOP.                             *
     H*                                                               *
     H* 05/30/08 C:62 PROJECT DR257                                   *
     H*               ADD ROLLOVER PROCESSING                         *
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
     F*
      * Document INDEX TRANSACITON FILE
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

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C                   KFLD                    QQDOCT
     C*
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQWSID            2              SCAN STA ID
     C                   PARM                    QQDSQ#            6              DOC SEQ#
     C                   PARM                    QQBCHS            4              BATCH SEQ #
     C                   PARM                    QQCIF             8              CURR IMAGE FLR
     C                   PARM                    QQCISF           12              CURR IMAGE SFLR
     C                   PARM                    QQDOCT            8              DOC TYPE
     C                   PARM                    QQQID            10              QUEUE ID
     C*
     C                   PARM                    QQDCID           12              DOC ID
     C                   PARM                    QQBYTD            9              BYTES IN DOC
     C                   PARM                    QQSDAT            8              SCAN DATE
     C                   PARM                    QQSTIM            6              SCAN TIME
     C                   PARM                    QQBCHI           12              BATCH ID
     C                   PARM                    QQPGS             5              # OF PAGES
     C                   PARM                    QQKEY            40              DARTS KEY
     C                   PARM                    QQSEQ#            5 0            DARTS SEQ#
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQFQID           10
     C                   PARM                    QQSUSR           10
     C*
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
     C                   PARM                    QQRCVD            8 0
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQINDX            1
     C                   PARM                    QQRTRN            1
     C***** END  ADD ***  10/17/11  **********************************
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
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
      *
     C*****                MOVELQQWSID    DOWSID
     C*****                MOVE QQSDAT    DOSDT
     C*****                MOVE QQSTIM    DOSTMD
     C*****                MOVELQ1USER    DOSREP
     C*****                MOVELQQBCHI    DOBCHI
     C*****                MOVE QQPGS     DOPGS
     C*****                MOVEL*BLANKS   DOAIFL
     C*****                MOVEL*BLANKS   DOOPTF
     C*
     C*****      QQKEY     IFNE *BLANKS
     C*****                MOVEL'Y'       DOIDFX
     C*****                ELSE
     C*****                MOVEL'N'       DOIDFX
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
     C*****                MOVEL'RL'      DODSTS
     C*
     C*****                WRITEPDO01
     C*
     C*****      QQKEY     IFNE *BLANKS
     C*****                WRITEPDO011
     C*****                ENDIF
     C*
     C                   MOVEL     *BLANKS       QQOSYS
     C                   MOVEL     *BLANKS       QQLBN
     C                   MOVEL     *BLANKS       QQOFLD
     C                   MOVEL     *BLANKS       QQOSFL
     C                   MOVEL     *BLANKS       QQODCI
     C                   MOVE      *ZEROS        QQARQS
     C                   MOVE      QQSDAT        QQSDT
     C                   MOVE      QQSTIM        QQSTMD
     C                   MOVEL     Q1USER        QQSREP
     C                   MOVE      QQPGS         QQPGS#
     C                   MOVEL     *BLANKS       QQAIFL
     C                   MOVEL     *BLANKS       QQOPTF
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     'Y'           QQIDFX
     C                   ELSE
     C                   MOVEL     'N'           QQIDFX
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       QQVOL1
     C                   MOVEL     *BLANKS       QQVOL2
     C                   MOVEL     *BLANKS       QQVOL3
     C                   MOVEL     'RL'          QQDSTS
     C                   MOVE      'Y'           QQINDX
     C*
     C* Call API to write Document and Folder Info
     C                   CALL      'ISA700R'     ISAKEY
     C*
     C***** END  CHG ***  10/17/11  **********************************
      * * * ====================================================== * * *
     C*
      * * * IF QQQID IS = *BLANKS, WE MUST SET THIS FOR INDEXING QUEUE
     C*
     C     QQQID         IFNE      *BLANKS
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVELDODCID    IEDCID
     C*****                MOVELDODTYP    IEDOCT
     C                   MOVEL     QQDCID        IEDCID
     C                   MOVEL     QQDOCT        IEDOCT
     C***** END  CHG ***  10/17/11  **********************************
     C*
     C                   MOVEL     QQQID         IEQID
     C     DTKY1         CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C*********************MOVELDTDQUE    IEQID
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
     C                   MOVEL     'RL'          IEDSTS
     C*
     C                   WRITE     PIE01
     C*
     C*** DOC WAS QUEUED, SO WE CHECK THE STUFF HERE !!!
     C*
      *////////////////////////////////////////////////////////////////
      ** **  1/27/99 *** PUT IN EXIT PROGRAM CALL HERE ** ** ** ** ** *
      *////////////////////////////////////////////////////////////////
     C*
     C* * * CALL EXIT PROGRAM FOR REASSIGNS
     C*
     C                   CALL      'ISE001C'                            99      NO ERR
     C                   PARM      QQFQID        V@FQID           10            FROM QUEUE
     C                   PARM      QQQID         V@TQID           10            TO   QUEUE
     C                   PARM      QQSUSR        V@USER           10            FROM USER
     C                   PARM      IEDCID        V@DCID           12            DOC ID
     C                   PARM                    V@RTRN            6            XIT RTN CD
     C*
      *////////////////////////////////////////////////////////////////
     C*
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
     C*****      QQBCHS    IFNE *BLANKS
     C**                   MOVE QQBCHS    ##BCHS  40
     C**         ##BCHS    ADD  1         DIBCHS
     C*****                ENDIF
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
     C*
     C*** BECAUSE WE ARE PROCESSING ONE DOC AT A TIME, WE MUST ONLY ADD
     C*** 1 TO THE CURRENT # OF DOCS IN THE SUBFOLDER, IF IT WAS A BATCH
     C*** UPDATE, THEN WE COULD ADD THE TOTAL # OF DOCS TO THE C#DS FLD
     C*** AND DO IT ONLY ONCE PER BATCH!
     C**
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
