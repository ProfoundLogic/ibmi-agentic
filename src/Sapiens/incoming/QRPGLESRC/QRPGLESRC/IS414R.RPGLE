     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* * *  QUEUE THE INCOMING FAX ************************* * * *
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 07/09/07 C:191 PROJECT: 10IJI                                 *
     F*               -WRITE RECEIVED DATE TO IMIEP                   *
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
     FIMIEP     O    E             DISK
     FIMDOP1    O    E             DISK
     FIMDOL1    UF   E           K DISK
     F*
     FSDCMP     O    E             DISK
     FSDCMLI    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0I)
     FSMKTL     IF   E           K DISK
     FIMDTL2    IF   E           K DISK
      *
     F*
     F*
     F*

     D DN              S             75    DIM(5)                               DOC NOTES
     D CMA             C                   CONST('Fax Received from:')
     D CMB             C                   CONST('FaxERRORPartial:')
     D*
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('** Auto-Index **')
     D DHBR            C                   CONST('Key:')
     D DHB1            C                   CONST('Seq#:')
     D DTOQ            C                   CONST('for TSI:')
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D FIDS02          DS
  O00D  Q3FILE                 1      8
  O00D  Q3OPEN                 9      9
  O00D  Q3EOF                 10     10
  O00D  Q3STCD                11     15  0
  O00D  Q3OPCD                16     21
  O00D  Q3RTN                 22     29
O0000D  Q3SSEQ                30     37
O0000D  Q3RCPR                38     45
O0000D  Q3EXN1                46     52
O0000D  Q3MIO#                53     56
  O00D  Q3FL02                57     76
  O00D  Q3ODPT                77     78
  O00D  Q3FL03                79    122
  O00D  Q3SPL#               123    124B 0
  O00D  Q3FL07               125    151
  O00D  Q3LNRW               152    153B 0
  O00D  Q3CLCH               154    155B 0
  O00D  Q3NREC               156    159B 0
  O00D  Q3FL04               160    187
  O00D  Q3OVLN               188    189B 0
  O00D  Q3FL05               190    260
  O00D  Q3RECF               261    270
  O00D  Q3FL06               271    368
  O00D  Q3IAID               369    369
  O00D  Q3CPOS               370    371B 0
  O00D  Q3FL08               372    377
  O00D  Q3LSFR               378    379B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D FC6             C                   CONST('                    -
     D                                        ')
     D CM              C                   CONST('XXXXXXXXXXXXXXXXX:')

     I*
     IPCM0I
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#

     C*
     C*
     C*
     C*** KEY LIST FOR KEY TYPE FILE
     C     KTKY          KLIST
     C                   KFLD                    QQKYTP            2
     C*
     C*   KEY LIST FOR DO FILE
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     C*
     C*   KEY LIST FOR DT FILE
     C     DTKY          KLIST
     C                   KFLD                    QQRMCD
     C*
     C* * * KEY LIST FOR SDCMP
     C     CMKYA         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    ##SEQ#            5 0
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSID           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM                    QQPART            1            PERFORM PART LKUP
     C                   PARM                    QQPPGM           10            PARTIAL PGM
     C                   PARM                    QQPLIB           10            PARTIAL PGMLIB
     C                   PARM                    QQDFTQ           10            DFT QUEUE
     C                   PARM                    QQKEY            40            **KEY ********
     C                   PARM                    QQKYTP            2            **KEY TYPE ***
     C                   PARM                    QQCOMM          256            **COMMENT ****
     C                   PARM                    QQDUSR           10            **DIARY USER**
     C                   PARM                    QQRMCD            5            **RMK CODE ***
     C                   PARM                    QQRTCD            1            RTNCODE ********
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*
     C                   MOVE      QQ#PGS        ###PGS            5 0
     C                   MOVE      QQFDTE        ##FDTE            6 0
     C                   MOVE      QQFTIM        ##FTM             6 0
     C                   MOVE      QQDT8         ##DT8             8 0
     C                   MOVEL     QQRTCD        ##RTN             1
     C                   MOVEL     *BLANKS       QQRTCD
     C                   MOVEL     QQTSID        ##TSID           20
     C                   MOVE      *BLANKS       BOOM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *HIVAL        ##SEQ#
     C*
     C     KTKY          CHAIN     PKT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     KTPNAM        XXPNAM           10
     C                   ELSE
     C                   MOVEL     *BLANKS       XXPNAM
     C                   ENDIF
     C*
     C*** CREATE DART RCD ***
     C*
     C                   EXSR      WCOM
     C*
     C                   EXSR      WIE
     C                   EXSR      UDO
     C     BOOM          IFNE      '1'
     C                   EXSR      WDO1
     C                   ELSE
     C                   MOVE      'N'           QQRTCD
     C                   ENDIF
     C*
     C***                  CREATE DOC NOTES
     C                   EXSR      CRTNOT
     C                   MOVEL     *BLANKS       QQRTCD
     C                   GOTO      TERM
     C*
     C*
     C     TERM          TAG
     C     ##RTN         CABNE     *BLANKS       ENDPGM                           SETON LR
     C                   RETURN
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*
      ********************************************************************
      * * * CREATE DOC NOTES *********************************************
      ********************************************************************
     C     CRTNOT        BEGSR
      *
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   MOVE      CMSEQ#        XXSEQ#            5
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     QQDT8         XXY               4
     C                   MOVE      QQDT8         XXD               2
     C     2             SUBST     QQDT8:5       XXM               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     QQFTIM        XXH               2
     C                   MOVE      QQFTIM        XXS               2
     C     2             SUBST     QQFTIM:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75
     C*
     C                   MOVEL     XX75          DN(1)
     C*
     C     MDOC          CAT(P)    QQDCID:1      XX75
     C                   CAT(P)    DTOQ:3        XX75
     C                   CAT(P)    QQTSID:1      XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C     DHBR          CAT(P)    QQKEY:1       XX75
     C                   CAT(P)    DHB1:3        XX75
     C                   CAT(P)    XXSEQ#:1      XX75
     C*
     C                   MOVEL     XX75          DN(3)
     C*
     C                   MOVEL     *BLANKS       XX75
     C                   MOVEL     XX75          DN(4)
     C*
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
     C*
     C                   MOVE      *BLANKS       Q$RTC             1
     C*
     C                   DO        4             G                 3 0
     C*
     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(G)         QQCM75
     C*
     C*
     C     G             IFEQ      4
     C                   MOVE      '1'           Q$RTC             1
     C                   ENDIF
     C*
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM      QQDCID        QQDCID           12              DOCID
     C                   PARM      QQDT8         QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C     BPDOC         TAG
     C*
     C                   ENDDO
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
     C                   MOVE      *ZERO         CMSECL
     C                   MOVE      *BLANKS       CMSBYL
     C*
     C                   MOVEL     QQKEY         CMKEY
     C                   MOVEL     *BLANKS       CMCSID                         ANY LONGER
      *
      *
     C*** GET THE PNAM FOR THIS DART RCD
      *
      *
      *
     C                   MOVEL     XXPNAM        CMPNAM
      *
      *
      *
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVEL     QQRMCD        CMRMCD
     C                   MOVEL     QQDCID        CMDCID
     C                   MOVEL     QQCOMM        CMCOMM
     C                   MOVE      ' '           CMFL01
     C****   LOAD FAX FOLDER HERE *********************
     C                   MOVEL     QQFOLD        CMFOLD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C                   Z-ADD     HHDT          CMTRDT
     C                   Z-ADD     HHTIME        CMTRTM
     C*
     C*
     C*
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        CMIQUE                             HIDDEN
     C                   MOVEL     *BLANK        CMIPTH                             HIDDEN
     C                   MOVE      *BLANK        CMPIDX                             HIDDEN
     C                   MOVEL     *BLANK        CMAPID                             HIDDEN
     C                   MOVEL     *BLANK        CMIDOC                             HIDDEN
     C                   MOVEL     *BLANK        CMQPTY                             HIDDEN
     C                   MOVEL     *BLANKS       CMDCTP                             HIDDEN
     C*** FAX FLAG
     C                   MOVEL     'F'           CMIMGF
     C                   MOVEL     QQDCID        CMDCID
     C                   MOVE      *BLANKS       CMIDOC
     C*
     C*
     C*
     C                   MOVEL     *BLANK        CMSTAT
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C*** IF DIARY USER ENTERED, WRITE TO CMUSER
     C     QQDUSR        IFNE      *BLANKS
     C                   MOVEL     QQDUSR        CMUSER
     C                   MOVE      'Y'           CMDIFL
     C                   ELSE                                                   ELSE NE *BLANKS
     C*** ELSE DEFAULT TO CURRENT USER
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVE      'N'           CMDIFL
     C                   END                                                    END NE *BLANKS
     C*
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*                                                    DFT LOC=DASD
     C                   MOVEL     'D'           CMFL05                         ADD 05/11/93
     C*
     C                   MOVE      'N'           CMFL02                         ADD 03/20/92
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
     C                   MOVEL     QQKEY         QQKEY            40
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
     C                   ENDSR
     C*
      * * * ======================================================== * * *
      * * * WRITE IE RECORD **********************************************
      * * * ======================================================== * * *
     C     WIE           BEGSR
     C*
     C*
     C                   MOVEL     QQDCID        IEDCID
     C*
     C     DTKY          CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DTDOCT        IEDOCT
     C                   MOVEL     DTDQUE        IEQID
     C                   ELSE
     C                   MOVEL     *BLANKS       IEDOCT
     C                   MOVEL     *BLANKS       IEQID
     C                   ENDIF
     C*
     C                   MOVE      QQDT8         IESDT
     C                   MOVE      QQFTIM        IESTM
     C                   MOVEL     Q1USER        IESREP
     C                   MOVEL     'D'           IESTAT
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
     C                   MOVEL     'CM'          IEDSTS
     *** BEG ADD ********** 07/09/07 ********************************
     C     DORCVD        IFNE      0
     C                   MOVE      DORCVD        IERCVD
     C                   ELSE
     C                   MOVE      IESDT         IERCVD
     C                   ENDIF                                                  DORCVD
     *** END ADD ********** 07/09/07 ********************************
     C*
     C                   WRITE     PIE01
     C*
     C*
     C*
     C                   ENDSR
     C*
      * * * ======================================================== * * *
      * * * WRITE DO1 RECORD *********************************************
      * * * ======================================================== * * *
     C     WDO1          BEGSR
     C*
      *================================================================
     C*
      *
     C                   MOVEL     CMKEY         DODKEY
     C                   MOVE      CMSEQ#        DODSQ#
      *
     C*
     C                   MOVEL     Q1USER        DOLUSR
     C                   MOVEL     Q1PGM         DOLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      DOLDT
     C                   Z-ADD     DSTIME        DOLTIM
     C*
     C                   MOVEL     'CM'          DODSTS
     C*
     C                   WRITE     PDO011
     C*
     C*
     C*
     C                   ENDSR
     C*
      * * * ======================================================== * * *
      * * * UPDAT DO RECORD **********************************************
      * * * ======================================================== * * *
     C     UDO           BEGSR
     C*
      *================================================================
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     IEDOCT        DODTYP
      *
     C*
     C                   MOVEL     'Y'           DOAIFL
     C                   MOVEL     'Y'           DOIDFX
     C*
     C                   MOVEL     CMKEY         DODKEY
     C                   MOVE      CMSEQ#        DODSQ#
     C*
     C                   MOVEL     Q1USER        DOLUSR
     C                   MOVEL     Q1PGM         DOLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      DOLDT
     C                   Z-ADD     DSTIME        DOLTIM
     C*
     C*
     C                   MOVEL     'CM'          DODSTS
     C*
     C                   UPDATE    PDO01
     C*
     C                   ELSE
     C                   MOVE      '1'           BOOM              1
     C                   ENDIF
     C*
     C*
     C                   ENDSR
