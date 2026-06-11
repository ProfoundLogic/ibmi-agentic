     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* PROGRAM: F0349R01
     H* DESCRIPTION: WORK WITH EOMBS
     H* PROJECT: FL021   C:117
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 05/05/00  C:079  PROJ: FLMNT
     H*                  1. Add display of init date and add
     H*                     carrier received date field to entry screen
     H*                  2. Condition update of FDHCP.
     H*
     H* 12/16/99  C:117  PROJ: Y2K
     H*                  HAD TO CONVERT THE DATE BEFORE SAVING IT
     H*                  TO THE DATABASE.
     H*
     H*****************************************************************
     FF0349F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEOP     UF A E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     FFDHCL     UF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     F*** BEG ADD *** 05/05/00 ****************************************
     FFAE1L     UF   E           K DISK
     F*** END ADD *** 05/05/00 ****************************************
      /COPY SCOPYMT01D
     D LT              S              1    DIM(26) CTDATA PERRCD(26)            ALPHABET
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMBs')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*  USED TO UPDATE THE 5-6 POSITION IN FIELD HC10A1 IN FILE FDHCP
     D*
     D                 DS
     D  XX10A1                 1     10
     D  X110A1                 1      3
     D  X210A1                 4      4
     D  X310A1                 5      6
     D*
     D*   Data structure of FL License # to make sure first two
     D*     characters of license # are alpha
     D                 DS
     D  SCLIC#                 1      9
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*   KEY LIST FOR PAYMENT PLAN UPDATE
     C*
     C     PTKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*** BEG ADD *** 05/05/00 ****************************************
     C*
     C*   KEY LIST FOR EOB AUXILIARY FILE
     C*
     C     E1KY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 05/05/00 ****************************************
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CACO#         QQCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    CASS#         KKSS#
     C     *LIKE         DEFINE    EODIA1        XXDIA
     C     *LIKE         DEFINE    EODIA1        XXDIA1
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Make sure 1st Diag Code is populated & make sure that it is
     C*    a valid code; if 2nd, 3rd, & 4th Diag Codes are filled,
     C*    make sure they are valid.
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        7090
     C                   ELSE
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7190
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDS1         SCDS1
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7790
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DIDS1         SCDS2
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7890
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DIDS1         SCDS3
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7990
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DIDS1         SCDS4
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD *** 05/05/00 ****************************************
     C*  Carr rcvd date added to screen; required
     C*
     C     SCCRDT        IFEQ      *ZEROS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCRDT EQ ZEROS
     C*
     C*  Date must be valid
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCCRDT
     C                   ENDIF                                                  SRERR EQ 1
     C*
     C*  Date must not be greater than init proc date
     C                   Z-ADD     SCIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOIDT
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1CRDT
     C*
     C     E1CRDT        IFGT      EOIDT
     C     EOIDT         ANDNE     0
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  E1IDT GT EOIDT
     C*
     C*** END ADD *** 05/05/00 ****************************************
     C*
     C*  Make sure Liscense# isn't blank
     C*
     C     SCFLIC        IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C     SCFLIC        IFNE      *BLANKS
     C                   MOVEL     SCFLIC        SCLIC#
     C                   Z-ADD     1             X                 2 0
     C     SCLIC1        LOOKUP    LT(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN66
     C                   SETON                                        90
     C                   END
     C*
     C                   Z-ADD     1             X
     C     SCLIC2        LOOKUP    LT(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN66
     C                   SETON                                        90
     C                   END
     C                   END                                                    END EQ 'FL'
     C*
     C*  Make sure FL Payment Plan isn't blank & that it is valid
     C*
     C     SCPMNT        IFEQ      *BLANKS
     C                   SETON                                        7390
     C                   ELSE
     C                   SETOFF                                       73
     C                   ENDIF
     C     SCPMNT        IFNE      *BLANKS
     C     SCPMNT        ANDNE     'CP'
     C     SCPMNT        ANDNE     'CH'
     C     SCPMNT        ANDNE     'CI'
     C     SCPMNT        ANDNE     'MP'
     C     SCPMNT        ANDNE     'MH'
     C     SCPMNT        ANDNE     'MI'
     C     SCPMNT        ANDNE     'NP'
     C     SCPMNT        ANDNE     'NH'
     C     SCPMNT        ANDNE     'NC'
     C     SCPMNT        ANDNE     'FS'
     C                   SETON                                        7490
     C                   ELSE
     C                   SETOFF                                       74
     C                   ENDIF
     C*
     C*  Make sure that Data Subm flag is either "Y" or "N" & not
     C*    blank
     C*
     C     SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        7590
     C                   ELSE
     C     SCTAPE        IFEQ      'Y'
     C     SCTAPE        OREQ      'N'
     C                   SETOFF                                       76
     C                   ELSE
     C                   SETON                                        7690
     C                   ENDIF
     C                   ENDIF
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KBDO - F2=Process
     C*================================================================
     C     KBDO          BEGSR
     C*
     C                   MOVE      SCCNTL        QQCNTL
     C                   MOVEL     SCCONM        XXCONM           40
     C                   MOVEL     SCUSER        XXUSER           10
     C                   MOVEL     SCEOMB        XXEOB#            9 0
     C                   MOVEL     CACO#         QQCO#
     C*
     C                   CALL      'F0347R'
     C                   PARM                    QQCO#
     C                   PARM                    QQCALL            1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQCNTL
     C                   PARM                    XXCONM           40
     C                   PARM                    XXUSER           10
     C                   PARM                    XXEOB#            9 0
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C                   MOVEL     XXICD9        SCDIA1
     C                   MOVEL     XXLOST        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C  N60              MOVEL     DIDS1         SCDS1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C                   MOVEL     XXICD9        SCDIA2
     C                   MOVEL     XXLOST        SCLOS2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC2
     C  N60              MOVEL     DIDS1         SCDS2
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C                   MOVEL     XXICD9        SCDIA3
     C                   MOVEL     XXLOST        SCLOS3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC3
     C  N60              MOVEL     DIDS1         SCDS3
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C                   MOVEL     XXICD9        SCDIA4
     C                   MOVEL     XXLOST        SCLOS4
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC4
     C  N60              MOVEL     DIDS1         SCDS4
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCPMNT'
     C                   SETON                                        9067
     C                   SETOFF                                       6869
     C*
     C     HFLD          WHENEQ    'SCFLIC'
     C                   SETON                                        9069
     C                   SETOFF                                       6867
     C*
     C     HFLD          WHENEQ    'SCTAPE'
     C                   SETON                                        9068
     C                   SETOFF                                       6967
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EOCO#         KKCO#
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVEL     EOFYR         KKFYR
     C                   MOVEL     EOCASE        KKCASE
     C*
     C                   MOVEL(P)  EOSUB#        XXSUB#            5
     C                   MOVEL(P)  EOFND         XXFND             3
     C                   MOVEL(P)  EOFYR         XXFYR             3
     C                   MOVEL(P)  EOCASE        XXCASE            7
     C*
     C     CAKY          CHAIN     PCA01                              60
     C                   MOVEL     CASS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** Get Claimant name
     C                   MOVE      *BLANKS       SCCNAM
     C*
     C                   MOVEL(P)  CDLSN         ##LAST           56
     C                   MOVEL(P)  CDFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCCNAM
     C*
     C                   ENDIF                                                  *IN99
     C*
     C* CONCAT FOR CASE NUMBER
     C*
     C     XXSUB#        CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXFND:0       SCCASE
     C                   CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXFYR:0       SCCASE
     C                   CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXCASE:0      SCCASE
     C*
     C                   MOVE      EOFEIN        SCFEIN
     C                   MOVE      EOMOD         SCMOD
     C                   MOVE      EOPNAM        SCPNAM
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*
     C                   MOVE      CASS#         SCSS#
     C                   MOVE      CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      EOTAPE        SCTAPE
     C                   MOVE      EOFLIC        SCFLIC
     C*
     C     PTKEY         CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      HC10A1        XX10A1
     C                   MOVEL     X310A1        SCPMNT
     C                   ENDIF
     C*
     C     EODIA1        IFNE      *BLANKS
     C                   MOVEL     EODIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     EODIA1        SCDIA1
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCDS1
     C                   MOVE      *BLANKS       SCDIA1
     C                   ENDIF
     C*
     C     EODIA2        IFNE      *BLANKS
     C                   MOVEL     EODIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDS1         SCDS2
     C                   MOVEL     EODIA2        SCDIA2
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVE      *BLANKS       SCLOS2
     C                   MOVE      *BLANKS       SCDS2
     C                   MOVE      *BLANKS       SCDIA2
     C                   ENDIF
     C*
     C     EODIA3        IFNE      *BLANKS
     C                   MOVEL     EODIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDS1         SCDS3
     C                   MOVEL     EODIA3        SCDIA3
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC3
     C                   MOVE      *BLANKS       SCLOS3
     C                   MOVE      *BLANKS       SCDS3
     C                   MOVE      *BLANKS       SCDIA3
     C                   ENDIF
     C*
     C     EODIA4        IFNE      *BLANKS
     C                   MOVEL     EODIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDS1         SCDS4
     C                   MOVEL     EODIA4        SCDIA4
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC4
     C                   MOVE      *BLANKS       SCLOS4
     C                   MOVE      *BLANKS       SCDS4
     C                   MOVE      *BLANKS       SCDIA4
     C                   ENDIF
     C*** BEG ADD *** 05/05/00 ****************************************
     C     E1KY          CHAIN(N)  PE101                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     E1IDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIDT
     C*
     C                   Z-ADD     E1CRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
     C*
     C                   ENDIF
     C*** END ADD *** 05/05/00 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCDIA1        EODIA1
     C                   MOVEL     SCDIA2        EODIA2
     C                   MOVEL     SCDIA3        EODIA3
     C                   MOVEL     SCDIA4        EODIA4
     C*
     C                   MOVEL     SCFLIC        EOFLIC
     C                   MOVEL     SCTAPE        EOTAPE
     C                   MOVEL     SCPMNT        X310A1
     C*** BEG DEL *** 05/05/00 ****************************************
     C******************** MOVE XX10A1    HC10A1
     C*** END DEL *** 05/05/00 ****************************************
     C*** END DEL *** 05/05/00 ****************************************
     C*
     C*** BEG DEL *** 05/05/00 ****************************************
     C******************** MOVE Q1USER    EOUSER
     C*** END DEL *** 05/05/00 ****************************************
     C                   MOVE      Q1PGM         EOPGM
     C                   MOVE      DSTIME        EOTIME
     C*** BEG CHG *** 12/16/99 ****************************************
     C***********          MOVE DSDATE    EODT
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*** END CHG *** 12/16/99 ****************************************
     C*** BEG ADD *** 05/05/00 ****************************************
     C     PTKEY         CHAIN     PHC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      XX10A1        HC10A1
     C                   Z-ADD     EODT          HCDT
     C                   Z-ADD     EOTIME        HCTIME
     C                   MOVEL     Q1USER        HCUSER
     C                   UPDATE    PHC01
     C                   ENDIF
     C*** END ADD *** 05/05/00 ****************************************
     C     E1KY          CHAIN     PE101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SRDTO         E1DT
     C                   MOVE      Q1PGM         E1PGM
     C                   MOVE      DSTIME        E1TIME
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1CRDT
     C*
     C                   UPDATE    PE101
     C*
     C                   ENDIF
     C*** END ADD *** 05/05/00 ****************************************
     C*
     C                   MOVE      Q1USER        HCUSER
     C                   MOVE      Q1PGM         HCPGM
     C                   MOVE      DSTIME        HCTIME
     C*** BEG CHG *** 12/16/99 ****************************************
     C***********          MOVE DSDATE    HCDT
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C*** END CHG *** 12/16/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKB         CASEQ     *ON           KBDO
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        IFEQ      'C'
     C     EOST          ANDEQ     'FL'
     C     EOCNTL        ANDNE     *BLANKS
     C                   SETOFF                                       85
     C                   ELSE
     C                   SETON                                        85
     C                   EXSR      PGMEND
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C                   MOVE      SCCNTL        QQCNTL           20
     C*
     C*** BEG DEL *** 05/05/00 ****************************************
     C********** PTKEY     CHAINPHC01                60
     C*** END DEL *** 05/05/00 ****************************************
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C*** BEG DEL *** 05/05/00 ****************************************
     C******************** UPDATPHC01
     C*** END DEL *** 05/05/00 ****************************************
E01  C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     *INKB         ANDEQ     *OFF
     C                   CALL      'F0348R'
     C                   PARM      ' '           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQRRN             9 0
     C                   PARM      @@RRN         QQRRN2            9 0
     C                   PARM                    QQCNTL           20
     C                   END
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAEOP
     C                   ENDSR
**  TABLE LT  - ALPHABET
ABCDEFGHIJKLMNOPQRSTUVWXYZ
