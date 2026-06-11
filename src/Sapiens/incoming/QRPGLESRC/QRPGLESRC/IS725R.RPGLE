     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS725R   UPDATE THE DOCUMENT AND FOLDER INFO FROM VALID RESCN
     H*****************************************************************
     H*                                                               *
     H* * * UPDATE THE DO RECORD                                      *
     H*                                                               *
     H*
     H* * * ASSIGN TO INDEX QUEUE BASED ON DOC TYPE                   *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H***                                                             *
     H* 04/15/14 C:300 Project: SY195                                *
     H*               - Correct program name to retrieve IFS ROOT    *
     H*                                                              *
     H* 10/17/11 C:279 Project: SY195                                *
     H*               - Modified to update IMDOP with UNC path and   *
     H*                 IFS flag.                                    *
     H*                                                               *
     H*** 10/13/97 - MODIFIED TO ONLY PROCESS (ADD) 1 DOC FOR FOLDER  *
     H***            BALANCE RECORDS                                  *
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
      * Document TRANSACTION FILE
     FIMDOL1    UF   E           K DISK
     F*
     F*
      * INDEX QUEUE FILE
     FIMIEL5    UF   E           K DISK
     F*

     D*
      ***** BEG ADD ***** 10/17/11 *********************************************
     D RPGM            C                   CONST('RTVDOCENV')
     D RPGM1           C                   CONST('RTVDOCROOT')
      ***** END ADD ***** 10/17/11 *********************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DOCUMENT  FILE
     C*
     C     DOKY1         KLIST
     C                   KFLD                    QQDCID
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
     C                   PARM                    QQ#DCS            5              # OF DOCS PRCSD
     C                   PARM                    QQDOCT            8              DOC TYPE
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
     C***** BEG  ADD ***  10/17/11  **********************************
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
     C***** END  ADD ***  10/17/11  **********************************
     C*
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
     C     DOKY1         CHAIN     PDO01                              60
     C                   MOVEL     QQCISF        DOSFLR
     C                   MOVEL     *BLANKS       DOOSYS
     C                   MOVEL     *BLANKS       DOLIBN
     C                   MOVEL     *BLANKS       DOOFLD
     C                   MOVEL     *BLANKS       DOOSFL
     C                   MOVEL     *BLANKS       DOODCI
     C                   MOVE      *ZEROS        DOARQS
      *
     C                   MOVE      QQBYTD        ##BYTD            9 0
     C     ##BYTD        DIV       1000          DOBYTD
      *
      *
      *
     C*********************MOVE QQBYTD    DOBYTD
     C                   MOVEL     QQWSID        DOWSID
     C                   MOVE      QQSDAT        DOSDT
     C                   MOVE      QQSTIM        DOSTMD
     C                   MOVEL     Q1USER        DOSREP
     C                   MOVE      QQPGS         DOPGS
     C                   MOVEL     *BLANKS       DOAIFL
     C                   MOVEL     *BLANKS       DOOPTF
     C*
     C                   MOVEL     *BLANKS       DOVOL1
     C                   MOVEL     *BLANKS       DOVOL2
     C                   MOVEL     *BLANKS       DOVOL3
     C                   MOVEL     *BLANKS       DOLPGM
     C                   MOVEL     *BLANKS       DOLUSR
     C                   MOVE      *ZEROS        DOLDT
     C                   MOVE      *ZEROS        DOLTIM
     C*
     C                   MOVEL     'SC'          DODSTS
     C*
     C***** BEG  ADD ***  10/17/11  **********************************
     C     Q1PRMS        IFGE      18
     C     QQIFSP        ANDEQ     ' '
      * Get environment
     C                   CALL      RPGM
     C                   PARM                    ENV               1
      *
      * If IFS then get the Path
     C     ENV           IFEQ      'I'
     C     QQUNCN        ANDEQ     ' '
      * Get Root
     C***** beg chg ****** 04/15/14 **********************************
     C*****              CALL      RPGM
     C                   CALL      RPGM1
     C***** end chg ****** 04/15/14 **********************************
     C                   PARM                    ROOT             10
     C*
     C     DOFLR         IFNE      ' '
     C     DOSFLR        ORNE      ' '
     C     DODCID        ORNE      ' '
     C                   CAT(P)    '/':0         QQUNCN
     C                   CAT(P)    ROOT:0        QQUNCN
     C                   CAT(P)    '/':0         QQUNCN
     C                   CAT(P)    DOFLR:0       QQUNCN
     C                   CAT(P)    '/':0         QQUNCN
     C                   CAT(P)    DOSFLR:0      QQUNCN
     C                   CAT(P)    '/':0         QQUNCN
     C                   CAT(P)    DODCID:0      QQUNCN
     C*
     C                   MOVE      ENV           DOIFSP
     C                   MOVE      QQUNCN        DOUNCN
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C***** END  ADD ***  10/17/11  **********************************
     C*
     C  N60              UPDATE    PDO01
     C*
      * * * ====================================================== * * *
     C*
      * * * IF QQKEY IS = *BLANKS, WE MUST SET THIS FOR INDEXING QUEUE
     C*
     C     QQKEY         IFEQ      *BLANKS
     C     DOKY1         CHAIN     PIE01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      DOSDT         IESDT
     C                   MOVE      DOSTMD        IESTM
     C                   MOVEL     Q1USER        IESREP
     C                   MOVEL     'A'           IESTAT
     C*
     C                   MOVEL     Q1USER        IELUSR
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      IELDT
     C                   Z-ADD     DSTIME        IELTIM
     C*
     C                   MOVEL     'SC'          IEDSTS
     C*
     C  N60              UPDATE    PIE01
     C     DOKY1         READE     PIE01                                  60
     C                   ENDDO
     C*
     C*
     C                   ENDIF
     C*
      *
     C*
     C     TERM          TAG
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
