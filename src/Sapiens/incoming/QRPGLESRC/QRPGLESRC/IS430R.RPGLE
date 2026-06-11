     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS430R - PROCESS ALL SPLIT REQUESTS                      *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*  60    GENERAL CHAIN INDICATORE                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/15/13 C:062 Redmine: 2212 CSI                              *
     H*                Change split server processing                 *
     H*                                                               *
     H* 03/01/13 C:191 Redmine: 2218 CSI                              *
     H*                  - Clear CMUNCN before loading new value      *
     H*                                                               *
     H* 10/17/11 C:279 Project: SY195                                 *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H* 05/30/08 C:62 PROJECT DR257                                   *
     H*               ADD ROLLOVER PROCESSING                         *
     H*                                                               *
     H*****************************************************************
     FIASRL     UF   E           K DISK
     FSDCML     O    E             DISK
     FSDCMLI    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0I)
     FIMDTL     IF   E           K DISK
     FIMSSL     IF   E           K DISK
     FIMDOL1    IF   E           K DISK
     F*

     D*
     D DN              S             75    DIM(15)                              DOCN's
     D*
     D NOLNK           C                   CONST('No queues available ')
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('SPLIT AL')
     D DHBR            C                   CONST('has been reassigned')
     D DTOQ            C                   CONST('to Queue:')
     D CSPL            C                   CONST('** SPLIT **')
     D CIDX            C                   CONST('** INDEXED **')
     D CQUE            C                   CONST('** QUEUED **')
     D CTKY            C                   CONST('To Key:')
     D CBYU            C                   CONST('By User:')
     D*
     D CTDC            C                   CONST('This document was cr-
     D                                     eated by the split s-
     D                                     erver, From DOCID:')
     D*
     D*
     D MP1             C                   CONST('Split request for Do-
     D                                     c ID: ')
     D MP2             C                   CONST('from Queue:')
     D MP3             C                   CONST('for pages:')
     D MP4             C                   CONST('Did NOT complete nor-
     D                                     mally.')
     D ORIG            C                   CONST('Original ')
     D*
     D                 DS
     D  SCMSG1                 1     64
     D  SM                     1     64
     D                                     DIM(64)                              SCMSG
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
      ***** END ADD ***** 10/17/11 ***********************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPCM0I
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#
     I*

     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8               MDY(YY)
     C                   PARM      *BLANKS       SRTFMT            8               MDYY
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8            YYYYMMDD
     C                   PARM      '*MDYY'       SRTFMT            8            MMDDYYYY
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*
     C     SSKY          KLIST
     C                   KFLD                    QQSRVI
     C*
     C     DTKY          KLIST
     C                   KFLD                    Q@DCTP
     C*
     C     DTKY1         KLIST
     C                   KFLD                    Q@DCTP
     C*
     C     CMKYA         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    ##SEQ#            5 0
     C*
     C     DOKY          KLIST
     C                   KFLD                    SRFDCI
     C*
     C*
     C*   KEY LIST FOR THE IMAGE INDEXING FILE
     C*
     C*
     C*****************************************************************
     C*
     C     *ENTRY        PLIST
     C*** Begin Del *** 04/15/13 *******************************************
     C**********         PARM                    QQFDCI           12
     C**********         PARM                    QQJOB#            6
     C**********         PARM                    QQUSRN           10
     C**********         PARM                    QQJOBN           10
     C*** End   Del *** 04/15/13 *******************************************
     C                   PARM                    QQSRVI            2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   PARM                    QQRTNC            1
     C*** END   ADD *** 05/30/08 **************************************
      ***** BEG ADD ***** 10/17/11 ***********************************
      * GET ROOT
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     *HIVAL        ##SEQ#
     C     *LOVAL        SETLL     PSR01
     C                   READ      PSR01                                  63
     C     *IN63         DOWEQ     *OFF
     C*
     C*** FOR EVERY SPLIT  RECORD, PERFORM SPLIT, UPDATE SR DB RCD, CREATE
     C*** INDEX IF NECESSARY, QUEUE THE ITEM IF NECESSARY, ETC.
     C* * * PERFORM ANY ERROR RECOVERY, ETC.
     C*
      *
      * * * GET THE NEXT AVAIL DOC ID ETC. FOR THIS NEW DOC TO BE CREATED.
     C*
     C                   CALL      'IS716R'
     C                   PARM                    QQSRVI                           "WS ID>.."
     C                   PARM                    Q1SEQ#            6              RETURN PARMS
     C                   PARM                    QQSUFX            3              DOC SUFFIX
     C                   PARM                    QQBCHS            4              BTCH ID
     C                   PARM                    QQDCID           12              NEW DOC ID
     C                   PARM                    QQBCHI           12              BUILT BTCH ID
     C                   PARM                    QQCIF             8              CURR IMAGE FLR
     C                   PARM                    QQCISF           12              CURR IMAGE SFLR
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 05/30/08 **************************************
     C*
     C* IF THE PROGRAM ENDED ABNORMALLY DUE TO ROLLOVER ERROR THEN
     C* PASS THAT ERROR CODE TO IS430C
     C*
     C     Q1PRMS        IFGT      5
     C     QQRTCD        IFEQ      'S'
     C                   MOVEL(P)  QQRTCD        QQRTNC
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C*
     C*** ERRORS THAT CAN BE RETURNED ARE "1" - INVALID DI RCD     =OR=
     C***                                 "2" - INVALID FB RCD
     C*
     C     QQCIF         CAT(P)    '/':0         Q@TFLR           40
     C                   CAT(P)    QQCISF:0      Q@TFLR
     C*
      *
      * * * PERFORM THE SPLIT REQUEST.... IF SUCCESSFUL...
      *
     C                   CALL      'IS207C'
     C                   PARM      SRFDCI        QQFRDC           12
     C                   PARM      SRFPTH        QQFPTH           80
     C                   PARM      QQDCID        QQTODC           12
     C                   PARM      Q@TFLR        QQTFLR           40
     C                   PARM      SRFRPG        QQFP              3
     C                   PARM      SRTOPG        QQTP              3
     C                   PARM      SRFTYP        QQTYP             1
     C                   PARM      *BLANKS       Q#RTC             6
     C*
     C* * * CHECK RETURN CODES HERE FOR ERRORS......
     C*
      * * ================================================================
     C*** Q#RTC -
      *           000001   = NO DOC INFO COULD BE RETRIEVED.
      *
      *           XXXXXX   = OTHER RETURNED FROM SPLIT SERVER.
      * * ================================================================
     C     Q#RTC         IFNE      *BLANKS
     C     Q#RTC         ANDNE     '000000'
     C     Q#RTC         ANDNE     '0     '
     C     Q#RTC         ANDNE     '     0'
     C*** Begin Chg *** 04/15/13 *******************************************
     C**********         MOVE      'E'           SRSPLC
     C*
     C**********         TIME                    DSDTTM
     C*
     C**********         Z-ADD     DSDATE        SRDTI
     C**********         CALL      'ISCVTDT'     DTCYMD
     C**********         MOVEL     SRDTO         SRSCDT
     C**********         MOVE      DSTIME        SRSCTM
     C*
     C**********         MOVEL     Q1JOB         SRJOBN
     C**********         MOVEL     Q1USER        SRUSRN
     C**********         MOVE      Q1JOBN        SRJOB#
     C*
     C**********         UPDATE    PSR01
     C                   Delete    PSR01
     C*** End   Chg *** 04/15/13 *******************************************
     C*** SEND ERROR TO REQUESTING USER THAT SPLIT DID NOT END NORMAL
     C*
     C                   MOVEL     *BLANKS       MSGDTA
     C*
     C     MP1           CAT(P)    SRFDCI:1      MSGDTA
     C                   CAT(P)    MP2:1         MSGDTA
     C                   CAT(P)    SRFQID:1      MSGDTA
     C                   CAT(P)    MP3:1         MSGDTA
     C                   CAT(P)    SRFRPG:1      MSGDTA
     C                   CAT(P)    '- ':1        MSGDTA
     C                   CAT(P)    SRTOPG:1      MSGDTA
     C                   CAT(P)    DTOQ:2        MSGDTA
     C                   CAT(P)    SRTQID:1      MSGDTA
     C                   CAT(P)    CTKY:2        MSGDTA
     C                   CAT(P)    SRKEY:1       MSGDTA
     C                   CAT(P)    MP4:2         MSGDTA
     C*
     C***
     C                   CALL      'ISSNDMSG'
     C                   PARM                    MSGDTA          200
     C                   PARM      SRSUSR        TOUSR            10
     C                   PARM      *BLANKS       MSGRTN            1
     C*
     C*
     C                   GOTO      TERM
     C                   ENDIF
      *
      *
      * *    DETERMING IF WE NEED TO INDEX THIS, IF SO ... DOIT
     C*
     C                   MOVE      *ZEROS        #SSEQ#
     C                   MOVEL     *BLANKS       #SKEY
     C     SRINDX        IFEQ      'Y'
     C                   EXSR      WCOM
     C                   ENDIF
     C*
      *
      * * * CREATE THE DO RCD, UPDATE THE SR RCD,
      *
     C     SSKY          CHAIN     PSS01                              68
     C     *IN68         IFEQ      *OFF
     C                   MOVEL     SSDCTP        Q@DCTP            8
     C                   END
     C*** IF DCTP ENTERED, USE IT
     C     SRDCTP        IFNE      *BLANKS
     C                   MOVEL     SRDCTP        Q@DCTP
     C                   ENDIF
     C*
     C     DTKY          CHAIN     PDT01                              68
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         ##CDAT            8
     C                   MOVE      DSTIME        ##CTIM            6
     C                   MOVE      SRFRPG        #FRPG             3 0
     C                   MOVE      SRTOPG        #TOPG             3 0
     C*
     C     #TOPG         SUB       #FRPG         #PGS              5 0
     C                   ADD       1             #PGS
     C                   MOVE      #PGS          @PGS              5
      *
     C                   CALL      'IS432R'
     C                   PARM                    QQSRVI            2              CURR IMAGE FLR
     C                   PARM                    Q1SEQ#            6
     C                   PARM      *BLANKS       Q1BCHS            4
     C                   PARM                    QQCIF             8              CURR IMAGE FLR
     C                   PARM                    QQCISF           12              CURR IMAGE SFLR
     C                   PARM      Q@DCTP        QQDOCT            8              DOC TYPE
     C                   PARM      SRTQID        QQQID            10
     C                   PARM      QQTODC        QQDCID           12              DOC ID
     C                   PARM      *ALL'0'       QQBYTD            9              BYTES IN DOC
     C                   PARM      ##CDAT        QQSDAT            8              SCAN DATE
     C                   PARM      ##CTIM        QQSTIM            6              SCAN TIME
     C                   PARM                    QQBCHI           12              BATCH ID
     C                   PARM      @PGS          QQPGS             5              # OF PAGES
     C                   PARM      #SKEY         QQKEY            40              DARTS KEY
     C                   PARM      #SSEQ#        QQSEQ#            5 0            DARTS SEQ#
     C                   PARM      *BLANKS       QQRTCD            1
     C                   PARM      SRFQID        QQFQID           10
     C                   PARM      SRSUSR        QQSUSR           10
     C*
     C*** BEGIN ADD *** 05/30/08 **************************************
     C*
     C* IF THE PROGRAM ENDED ABNORMALLY DUE TO ROLLOVER ERROR THEN
     C* PASS THAT ERROR CODE TO IS430C
     C*
     C     Q1PRMS        IFGT      5
     C     QQRTCD        IFEQ      'S'
     C                   MOVEL(P)  QQRTCD        QQRTNC
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C*
     C* * *  UPDATE THE SR RECORD AS SUCCESSFUL
      *
     C                   MOVEL     Q@DCTP        SRDCTP
     C                   MOVEL     QQTODC        SRTDCI
     C                   MOVEL     QQCIF         SRTFLR
     C                   MOVEL     QQCISF        SRTSFL
     C                   MOVEL     'C'           SRSPLC
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTCYMD
     C                   MOVEL     SRDTO         SRSCDT
     C                   MOVE      DSTIME        SRSCTM
     C*
     C                   MOVEL     Q1JOB         SRJOBN
     C                   MOVEL     Q1USER        SRUSRN
     C                   MOVE      Q1JOBN        SRJOB#
     C*
     C                   MOVE      #SSEQ#        SRSEQ#
     C                   MOVE      #SKEY         SRKEY
     C*
     C*
     C*
     C                   UPDATE    PSR01
     C*
     C*** CREATE DOC NOTES ***********************************************
     C*
     C     DOKY          CHAIN     PDO01                              69
     C*
     C*
     C                   EXSR      DNOT
     C*
      *** LOOP TO GET ANOTHER, IT THERE, IF NOT, GO BACK AND WAIT FOR DQ
      *** ENTRY.
      *
     C                   READ      PSR01                                  63
     C                   ENDDO
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *====================================================================*
     C     DNOT          BEGSR
     C*
     C                   Z-ADD     1             G                 2 0
     C                   MOVEL     *BLANKS       DN
      * * *
     C*
     C                   Z-ADD     DOSDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         QQSDT             8
     C                   MOVE      DOSTMD        Q$STM             6
      *
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     QQSDT         XXM               2
     C                   MOVE      QQSDT         XXY               4
     C     2             SUBST     QQSDT:3       XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXSDT            10
     C*
     C                   MOVEL     Q$STM         XXH               2
     C                   MOVE      Q$STM         XXS               2
     C     2             SUBST     Q$STM:3       XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXSTM             8
      *
      * * *
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQCDAT            8
     C                   MOVE      DSTIME        Q$TIME            6
      *
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     QQCDAT        XXM               2
     C                   MOVE      QQCDAT        XXY               4
     C     2             SUBST     QQCDAT:3      XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     Q$TIME        XXH               2
     C                   MOVE      Q$TIME        XXS               2
     C     2             SUBST     Q$TIME:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    CSPL:5        XX75
     C*
     C                   MOVEL     XX75          DN(G)
     C*
     C                   ADD       1             G
     C     CTDC          CAT(P)    SRFDCI:1      XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C*
     C                   ADD       1             G
     C     CBYU          CAT(P)    SRSUSR:1      XX75
     C                   CAT(P)    ORIG:4        XX75
     C                   CAT(P)    DDAT:1        XX75
     C                   CAT(P)    XXSDT:1       XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXSTM:1       XX75
     C*
     C                   MOVEL     XX75          DN(G)
     C*
     C                   ADD       1             G
     C                   MOVEL(P)  *BLANKS       XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C     SRTQID        IFNE      *BLANKS
     C                   ADD       1             G
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    CQUE:5        XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C                   ADD       1             G
     C     DTOQ          CAT(P)    SRTQID:1      XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C                   ADD       1             G
     C                   MOVEL(P)  *BLANKS       XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C                   ENDIF
     C*
     C*
     C     SRKEY         IFNE      *BLANKS
     C                   ADD       1             G
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    CIDX:5        XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C                   MOVE      SRSEQ#        $@SEQ             5
     C                   ADD       1             G
     C     CTKY          CAT(P)    SRKEY:1       XX75
     C                   CAT(P)    $@SEQ:3       XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C                   ADD       1             G
     C                   MOVEL(P)  *BLANKS       XX75
     C                   MOVEL     XX75          DN(G)
     C*
     C                   ENDIF
      *
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
     C*
     C*
     C                   DO        G             H                 3 0
     C*
     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(H)         QQCM75
     C*
     C     H             IFGE      G
     C                   MOVE      '1'           Q$RTC             1
     C                   ELSE
     C                   MOVE      ' '           Q$RTC
     C                   ENDIF
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM      SRTDCI        R$DCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   ENDDO
     C*
     C*
     C                   ENDSR
      *====================================================================*
     C     WCOM          BEGSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT              7 0
     C                   Z-ADD     DSTIME        HHTIME            6 0
     C*
     C*** GET LAST SEQ# USED
     C*
     C* * * WRITE REMARKS  TO FILE * * *
     C*
     C                   MOVE      *BLANK        CMSECL
     C                   MOVE      *BLANK        CMSBYL
     C*
     C                   MOVEL     SRKEY         CMKEY
     C                   MOVEL     *BLANKS       CMCSID                         ANY LONGER
     C                   MOVEL     SRPNAM        CMPNAM
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVEL     SRRMCD        CMRMCD
     C                   MOVEL     QQDCID        CMDCID
     C                   MOVEL     SRCOMM        CMCOMM
     C                   MOVE      ' '           CMFL01
     C****   LOAD FAX FOLDER HERE *********************
     C                   MOVEL     Q@TFLR        CMFOLD
      ***** BEG ADD ***** 10/17/11 ***********************************
      ***** BEG ADD ***** 03/01/13 ***********************************
     C                   EVAL      CMUNCN = *Blanks
      ***** END ADD ***** 03/01/13 ***********************************
     C     CMFOLD        IFNE      ' '
     C     CMDCID        ORNE      ' '
     C                   MOVEL     '/'           CMUNCN
     C     CMUNCN        CAT       ROOT:0        CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMFOLD:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMDCID:0      CMUNCN
     C                   MOVEL     ENV           CMIFSP
     C                   ELSE
     C                   MOVE      *BLANKS       CMUNCN
     C                   MOVE      *BLANKS       CMIFSP
     C                   ENDIF                                                  CMFOLD IFNE ' '
      ***** END ADD ***** 10/17/11 ***********************************
     C                   Z-ADD     SRCDAT        CMCDAT
     C                   Z-ADD     HHDT          CMTRDT
     C                   Z-ADD     HHTIME        CMTRTM
     C*
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     SRTQID        CMIQUE                             HIDDEN
     C                   MOVEL     Q@TFLR        CMIPTH                             HIDDEN
     C                   MOVE      *ZEROS        CMPIDX                             HIDDEN
     C                   MOVEL     *BLANK        CMAPID                             HIDDEN
     C                   MOVEL     QQDCID        CMIDOC                             HIDDEN
     C                   MOVEL     *BLANK        CMQPTY                             HIDDEN
     C                   MOVEL     Q@DCTP        CMDCTP                             HIDDEN
     C*** NO FAX... THIS IS AN IMAGE ****
     C                   MOVEL     'Y'           CMIMGF                             HIDDEN
     C*
     C                   MOVEL     *BLANK        CMSTAT
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C*** IF DIARY USER ENTERED, WRITE TO CMUSER
     C     SRDUSR        IFNE      *BLANKS
     C                   MOVEL     SRDUSR        CMUSER
     C                   ELSE                                                   ELSE NE *BLANKS
     C*** ELSE DEFAULT TO CURRENT USER
     C                   MOVEL     Q1USER        CMUSER
     C                   END                                                    END NE *BLANKS
     C*
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C     SRDUSR        IFNE      *BLANKS
     C                   MOVE      'Y'           CMDIFL
     C                   ELSE
     C                   MOVE      ' '           CMDIFL
     C                   END
     C*                                                    DFT LOC=DASD
     C                   MOVEL     'D'           CMFL05                         ADD 05/11/93
     C*
     C                   MOVE      *BLANK        CMFL02                         ADD 03/20/92
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
     C*
     C                   Z-ADD     *ZERO         CMDCDT
     C                   MOVEL     Q1PGM         CMIPGM                         ADD 10/14/91
     C*
     C                   MOVE      *BLANKS       CMSMCD
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C*
     C*** GET LAST SEQ# USED
     C*
     C                   MOVEL     SRKEY         QQKEY            40
     C*
     C     CMKYA         SETGT     PCM0I
     C                   READP     PCM0I                                  60
     C     *IN60         IFEQ      '0'
     C     QQKEY         ANDEQ     PIKEY
     C     PISEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     1             CMSEQ#
     C                   END
     C*
     C                   WRITE     PCM01
     C*
     C***** EXECUTE THE USER INDEX EXIT HERE ********
     C*                     ONLY FOR DIARY TYPES....
     C     CMDIFL        IFEQ      'Y'
     C*
     C                   MOVE      CMSEQ#        V@SEQ#
     C                   CALL      'ISE002C'                            99
     C                   PARM      SRFQID        FQ               10
     C                   PARM      SRDUSR        TU               10
     C                   PARM      SRSUSR        UID              10
     C                   PARM      CMDCID        DCID             12
     C                   PARM      CMKEY         V@KEY            40
     C                   PARM                    V@SEQ#            5
     C                   PARM      *BLANKS       RTRN              6
     C*
     C                   ENDIF
     C*
     C                   MOVEL     CMKEY         #SKEY            40
     C                   MOVE      CMSEQ#        #SSEQ#            5 0
     C*
     C                   ENDSR
