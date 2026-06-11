     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE W8042R - BULK REREATING ENDORSEMENT PROGRAM
     H*****************************************************************
     H*    PROGRAM DESCRIPTION:                                       *
     H*                                                               *
     H*    THIS PROGRAM IS USED TO ATTACH RATE CHANGE ENDORSEMENTS    *
     H*    TO THE EMPLOYER PASSED INTO THE PROGRAM.                   *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    07/16/99            PROJECT: WC850                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/14/21   C:180  Project WC2786
     H*                   Rename field WMENP1/ENDSC2 as it was giving
     h*                   an error on compile.
     H*                                                               *
     H* 02/08/11 C:190 SEQ#: 75631                                    *
     H*               WRITE THE DARTS REMARKS THRU THE DA700R API AND *
     H*               NOT DIRECTLY                                    *
     H*                                                               *
     H* 08/17/99 C:65 PROJECT: WC850                                  *
     H*               CHANGE THE FIELDS BEING USED FOR ENDORSE ADDRESS*
     H*                                                               *
     H*****************************************************************
     F*
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*** BEGIN DEL *** 02/08/11 **************************************
     F**********SDCMLA  IF  E           K        DISK                      A
     F*** END DEL *** 02/08/11 **************************************
     FSTCTL1    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWTENL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMMUL1    IF A E           K DISK
     F                                     RENAME(PMU01:PMU02)
     FWXFMP     O    E             DISK    USROPN
     *** Begin Change *** 01/14/21 *******************************
     F*WMENP1    O    E             DISK    USROPN
     FWMENP1    O    E             DISK    USROPN PREFIX(x)
     *** End   Change *** 01/14/21 *******************************
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     D*
     D*
     D XXFORM          C                   CONST('WC000407  ')
     D XXDESC          C                   CONST('Rate Change Endo-
     D                                     rsement')
     D*
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP1                 1      5
     D  XXZIP2                 6      9
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*
     D                 DS
     D  DSDAT7                 1      7  0
     D  DSCENT                 1      1
     D  DSYY                   2      3
     D  DSMM                   4      5
     D  DSDD                   6      7
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  BF                     1   1000
     D                                     DIM(20)
     D  ENBLN1                 1     50
     D  ENBLN2                51    100
     D  ENBLN3               101    150
     D  ENBLN4               151    200
     D  ENBLN5               201    250
     D  ENBLN6               251    300
     D  ENBLN7               301    350
     D  ENBLN8               351    400
     D  ENBLN9               401    450
     D  ENBLNA               451    500
     D  ENBLNB               501    550
     D  ENBLNC               551    600
     D  ENBLND               601    650
     D  ENBLNE               651    700
     D  ENBLNF               701    750
     D  ENBLNG               751    800
     D  ENBLNH               801    850
     D  ENBLNI               851    900
     D  ENBLNJ               901    950
     D  ENBLNK               951   1000
     D                 DS
     D  AF                     1   1000
     D                                     DIM(20)
     D  ENALN1                 1     50
     D  ENALN2                51    100
     D  ENALN3               101    150
     D  ENALN4               151    200
     D  ENALN5               201    250
     D  ENALN6               251    300
     D  ENALN7               301    350
     D  ENALN8               351    400
     D  ENALN9               401    450
     D  ENALNA               451    500
     D  ENALNB               501    550
     D  ENALNC               551    600
     D  ENALND               601    650
     D  ENALNE               651    700
     D  ENALNF               701    750
     D  ENALNG               751    800
     D  ENALNH               801    850
     D  ENALNI               851    900
     D  ENALNJ               901    950
     D  ENALNK               951   1000
     D                 DS
     D  XXPOL1                 1     15
     D  ENFND                  1      3  0
     D  ENEMP#                 4     12  0
     D  ENFYR                 13     15  0
     D                 DS
     D  XXWK5A                 1      5
     D  XXEM1                  1      1
     D  XXEM2                  2      5
     D                 DS
     D  XXWK9A                 1      9
     D  XXPR1                  1      2
     D  XXSF1                  3      9
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C****************************************************************
     C*
     C*  FUND MASTER KLIST
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  DARTS REMARKS FILE
     C*
     C*** BEGIN DEL *** 02/08/11 **************************************
     C********   CMKY      KLIST
     C********             KFLD           QCKEY
     C********             KFLD           CMSEQ#
     C*** END   DEL *** 02/08/11 **************************************
     C*
     C*  DARTS REMARKS CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMRK            5
     C*
     C*  EMPLOYER MASTER KLIST
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  EMPLOYER DETAIL KLIST
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFDT
     C*
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  ENDORSEMENT FILE
     C*
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XKFORM
     C*
     C     MUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER DETAIL KLIST (PRIOR YEAR)
     C*
     C     ELKYP         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XPFYR
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  ENDORSEMENT FORM TABLE FILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
     C*
     C*  ENDORSEMENT FORM STATISTICAL INFO FILE KLIST
     C*
     C     NXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMST
     C                   KFLD                    ELPFDT
     C*
     C     NXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMST
     C*
     C*  FUND AUXILIARY KLIST
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CARRIER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELFYR         XPFYR
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C*** BEGIN DEL *** 02/08/11 **************************************
     C********   *LIKE     DEFN CMSEQ#    XXSEQ#
     C*** END   DEL *** 02/08/11 **************************************
     C     *LIKE         DEFINE    ENFORM        XKFORM
     C     *LIKE         DEFINE    FMEMST        XXEMST
     C     *LIKE         DEFINE    FMEAD1        XXEAD1
     C     *LIKE         DEFINE    FMEAD2        XXEAD2
     C     *LIKE         DEFINE    FMEAD3        XXEAD3
     C     *LIKE         DEFINE    FNPST         XXFNST
     C     *LIKE         DEFINE    FMCARR        XXCARR
     C     *LIKE         DEFINE    FMCPL#        XXCPL#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C     *LIKE         DEFINE    ELPTDT        XXEFDT
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    EMATTN        XXATTN
     C     *LIKE         DEFINE    EMCNTY        XXCNTY
     C     *LIKE         DEFINE    EMCNT3        XXCNT3
     C     *LIKE         DEFINE    EMCONT        XXCONT
     C     *LIKE         DEFINE    ELEMD1        XXEXD1
     C     *LIKE         DEFINE    ELEMD2        XXEXD2
     C     *LIKE         DEFINE    ELEMD3        XXEXD3
     C     *LIKE         DEFINE    ELEXM1        XXEXM1
     C     *LIKE         DEFINE    ELEXM2        XXEXM2
     C     *LIKE         DEFINE    ELEXM3        XXEXM3
     C     *LIKE         DEFINE    EMFEIN        SVFEIN
     C     *LIKE         DEFINE    EMMAD1        XXMAD1
     C     *LIKE         DEFINE    EMMAD2        XXMAD2
     C     *LIKE         DEFINE    EMNAME        XXENAM
     C     *LIKE         DEFINE    EMNAOB        XXNAOB
     C     *LIKE         DEFINE    EMPAD1        XXPAD1
     C     *LIKE         DEFINE    EMPAD2        XXPAD2
     C     *LIKE         DEFINE    AAFEIN        XXFEIN
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C     *LIKE         DEFINE    ELPFDT        X1PFDT
     C     *LIKE         DEFINE    ELPTDT        X1PTDT
     C     *LIKE         DEFINE    EMRSK#        XXRSK#
     C     *LIKE         DEFINE    EMTPOB        XXTPOB
     C*** 01/14/21     *LIKE         DEFINE    ENMAD3        XXMCSZ
     C*** 01/14/21    *LIKE         DEFINE    ENPAD3        XXPCSZ
     C     *LIKE         DEFINE    xENMAD3       XXMCSZ
     C     *LIKE         DEFINE    xENPAD3       XXPCSZ
     C*
     C*  DATE CHECK/CONVERSION PARM LISTS
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
     C*  ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQQUE            10
     C                   PARM                    QQPCT             5
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQDART            1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQPCT         XXPCT             5 2
     C*
     C                   MOVEL     XXFORM        XKFORM
     C                   MOVEL(P)  'PSBR'        XXRMRK
     C*
     C********************************************************************
     C*  MAINLINE
     C********************************************************************
     C*
     C                   Z-ADD     XXFND         ENFND
     C                   Z-ADD     XXEMP#        ENEMP#
     C*** 01/14/21       Z-ADD     XXDIV         ENDIV
     C                   Z-ADD     XXDIV         xENDIV
     C                   Z-ADD     XXFYR         ENFYR
     C                   EXSR      BULDIT
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C********************************************************************
     C*  GATHER INFO TO MERGE TO THE FORM
     C********************************************************************
     C     BULDIT        BEGSR
     C*
     C*  OPEN THE ENDORSEMENT FORM WORKFILE
     C*
     C                   OPEN      WXFMP
     C                   OPEN      WMENP1
     C*
     C*  GET THE BASIC INFO
     C*
     C                   EXSR      GETFND
     C                   EXSR      GETEMP
     C                   EXSR      GETAGT
     C                   EXSR      GETFRM
     C                   EXSR      GETCAR
     C*
     C*  CLEAR OUT THE RECORD
     C*
     C                   CLEAR                   PFM01
     C*
     C*  LOAD BASIC WXFMP FIELDS
     C*
     C                   EXSR      BASINF
     C*
     C*  LOAD FORM SPECIFIC INFO
     C*
     C                   EXSR      OTHINF
     C*
     C*  CLOSE THE ENDORSEMENT FORM WORKFILE
     C*
     C                   CLOSE     WXFMP
     C                   CLOSE     WMENP1
     C*
     C* WRITE THE DARTS REMARK
     C*
     C     QQDART        IFEQ      'Y'
     C*
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       Q2FLR            80
     C                   PARM      *BLANKS       Q2DCID           12
     C                   PARM      'EN'          Q2PRE             2
     C                   PARM      QQCO#         Q2CO#             3
     C                   PARM      QQFND         Q2FND             3
     C                   PARM      *BLANKS       Q2RTCD            1
     C*
     C     Q2FLR         IFNE      *BLANKS
     C     Q2DCID        ANDNE     *BLANKS
     C*
     C                   CALL      'W8043C'
     C                   PARM                    Q2DCID
     C                   PARM                    Q2FLR
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQFYR             3
     C*
     C*
     C*  GET THE NEXT AVAILABLE SEQUENCE NUMBER
     C*
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C*** BEGIN CHG *** 02/08/11 **************************************
     C*
     C*********  QCKEY     SETGTPCM01
     C*********  QCKEY     REDPEPCM01                    60
     C*********  *IN60     IFEQ *OFF
     C*********  CMSEQ#    ADD  1         XXSEQ#
     C*********            ELSE
     C*********            Z-ADD1         XXSEQ#
     C*********            ENDIF
     C*********
     C*********            TIME           DSDTTM
     C*********
     C*********            CLEARPCM01
     C*********            MOVE QCKEY     CMKEY
     C*********            MOVELCTDESC    CMCOMM
     C*
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     CMCDAT
     C*********
     C*********            MOVEL'W0050R'  CMPNAM
     C*********
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     CMTRDT
     C*********
     C*********            Z-ADDDSTIME    CMTRTM
     C*********            MOVE XXSEQ#    CMSEQ#
     C*********            MOVE *ZEROS    CMSECL
     C*********            MOVE 'N'       CMSBYL
     C*********            MOVE 'A'       CMSTAT
     C*********            MOVELQ1USER    CMUSER
     C*********            MOVELQ1PGM     CMPGM
     C*********            Z-ADDDSTIME    CMTIME
     C*********
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     CMDT
     C*********
     C*********            MOVE Q1USER    CMIUSR
     C*********            MOVE 'N'       CMDIFL
     C*********            MOVELQ1PGM     CMIPGM
     C*********  CTDPRG    IFGT *ZERO
     C*********            MOVE 'Y'       CMFL02
     C*********            ELSE
     C*********            MOVE *BLANKS   CMFL02
     C*********            END
     C*********            MOVE CTFL02    CMFL03
     C*********            MOVELXXRMRK    CMRMCD
     C*********
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     CMIDT
     C*********
     C*********            MOVELQ2FLR     CMFOLD    P
     C*********            MOVELQ2DCID    CMDCID
     C*********
     C*  RE-VALIDATE THE SEQUENCE NUMBER
     C*****
     C*********  *IN60     DOUEQ*OFF
     C*********  CMKY      SETLLPCM01                    60
     C** 60 ******         ADD  1         CMSEQ#
     C*********            ENDDO
     C*
     C*  WRITE THE DARTS COMMENT
     C*
     C*********            WRITEPCM01
     C*
     C*********            FEOD SDCMLA
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY
     C                   MOVEL(P)  CTDESC        Q#COMM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         Q#CDAT
     C*
     C                   MOVE(P)   Q1USER        Q#USER
     C                   MOVE(P)   'N'           Q#DIFL
     C                   MOVE(P)   XXRMRK        Q#RMCD
     C                   MOVE(P)   Q2DCID        Q#DCID
     C                   MOVEL(P)  'W0050R'      Q#PNAM
     C                   MOVE(P)   'N'           Q#SBYL
     C                   MOVE(P)   Q1PGM         Q#PGM
     C                   MOVE      Q2FLR         Q#FOLD
     C                   MOVE      Q1PGM         Q#IPGM
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           Q#FL02
     C                   ELSE
     C                   MOVE      *BLANKS       Q#FL02
     C                   END
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE      'Y'           Q#FL01
     C                   ELSE
     C                   MOVE      *BLANKS       Q#FL01
     C                   ENDIF
     C*
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT            8
     C                   PARM                    Q#DCID           12
     C                   PARM                    Q#PNAM           10
     C                   PARM      *ZEROS        Q#SECL            3
     C                   PARM                    Q#USER           10
     C                   PARM                    Q#PGM            10
     C                   PARM                    Q#FOLD           80
     C                   PARM                    Q#DIFL            1
     C                   PARM                    Q#IPGM           10
     C                   PARM                    Q#FL01            1
     C                   PARM                    Q#FL02            1
     C                   PARM                    Q#RMCD            5
     C                   PARM      '00000'       Q#SEQ#            5
     C                   PARM                    Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      *BLANK        Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *ZEROS        Q#PIDX            5
     C                   PARM      *BLANKS       Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C*** END   CHG *** 02/08/11 **************************************
     C*
     C*
     C                   ENDIF                                                  Q2FLR IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  QQDART IFEQ
     C*
     C*  PRINT THE ENDORSEMENTS FOR THIS EMPLOYER
     C*
     C                   CALL      'W8042C'
     C                   PARM                    QQQUE
     C*
     C                   EXSR      WRITEM
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFND - GET FUND INFORMATION
     C********************************************************************
     C     GETFND        BEGSR
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNPST         IFNE      *BLANK
     C                   MOVEL     FNPST         XXFNST
     C                   ELSE
     C                   MOVEL     FNMST         XXFNST
     C                   ENDIF
     C*
     C*  GET THE FUND AUXILIARY RECORD
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVE      F2CAR#        XXCARR
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETEMP - GET EMPLOYER INFORMATION
     C********************************************************************
     C     GETEMP        BEGSR
     C*
     C                   MOVE      *ZERO         XXPFDT
     C*
     C*  GET THE EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  EMATTN        XXATTN
     C                   MOVEL     EMCNTY        XXCNTY
     C                   MOVEL     EMCNT3        XXCNT3
     C                   MOVEL     EMCONT        XXCONT
     C                   MOVE      EMFEIN        SVFEIN
     C                   MOVEL     EMMAD1        XXMAD1
     C                   MOVEL     EMMAD2        XXMAD2
     C                   MOVEL     EMNAME        XXENAM
     C                   MOVEL     EMNAOB        XXNAOB
     C                   MOVE      EMRSK#        XXRSK#
     C                   MOVEL     EMTPOB        XXTPOB
     C*
     C*  BUILD UP ADDRESS LINES USING MAILING ADDRESS DATA
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL(P)  EMMAD1        XXEAD1
     C                   MOVEL     EMMST         XXEMST
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXCSZ            40
     C                   CAT       EMMST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
     C*
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
     C*
     C     EMMAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMMAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMMAD2 <> ' '
     C*
     C                   ELSE
     C*
     C*  BUILD UP ADDRESS LINES USING PHYSICAL ADDRESS DATA
     C*
     C                   MOVEL(P)  EMPAD1        XXEAD1
     C                   MOVEL     EMPST         XXEMST
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXCSZ
     C                   CAT       EMPST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
     C*
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
     C*
     C     EMPAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMPAD2 <> ' '
     C*
     C                   ENDIF                                                  EMMPFL = 'M'
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  GET THE EMPLOYER DETAIL RECORD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   Z-ADD     ELEMD1        XXEXD1
     C                   Z-ADD     ELEMD2        XXEXD2
     C                   Z-ADD     ELEMD3        XXEXD3
     C                   Z-ADD     ELEXM1        XXEXM1
     C                   Z-ADD     ELEXM2        XXEXM2
     C                   Z-ADD     ELEXM3        XXEXM3
     C                   Z-ADD     ELPFDT        X1PFDT
     C                   Z-ADD     ELPTDT        X1PTDT
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETAGT - GET AGENT INFORMATION
     C********************************************************************
     C     GETAGT        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXAGT#
     C*
     C*  GET THE CURRENT AGENT HISTORY RECORD
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AHAGT#        XXAGT#
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C  N60AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG01                              60
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFRM - GET ENDORSMENT FORM TABLE RECORD
     C********************************************************************
     C     GETFRM        BEGSR
     C*
     C*  GET THE ENDORSMENT FORM TABLE RECORD
     C*
     C     ENKY          CHAIN     PEN01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ENSTAT        ANDEQ     'I'
     C     ENKY          READE     PEN01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETCAR - GET CARRIER INFORMATION
     C********************************************************************
     C     GETCAR        BEGSR
     C*
     C*  GET THE CARRIER INFO RECORD
     C*
     C     RPKY          CHAIN     PRP01                              60
     C*
     C                   MOVEL     RPCAP#        XXCPL#
     C*
     C                   ENDSR
     C********************************************************************
     C*  BASINF - LOAD BASIC WXFMP FIELDS
     C********************************************************************
     C     BASINF        BEGSR
     C*
     C     MUKY1         SETGT     PMU02
     C     MUKY1         READPE    PMU02                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             MUEND#
     C                   ENDIF
     C*
     C     MUEND#        ADD       1             XXEND#            5 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      XXCO#         FMCO#
     C                   MOVE      XXFND         FMFND
     C                   MOVE      XXFYR         FMFYR
     C                   MOVE      XXEMP#        FMEMP#
     C                   MOVE      XXDIV         FMDIV
     C                   MOVEL     XXFORM        FMFORM
     C*
     C                   MOVE      ENEDDT        DSDAT7
     C                   MOVEL     DSMM          FMEDDT
     C                   MOVE      DSYY          FMEDDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C*
     C                   Z-ADD     XXEND#        FMEND#
     C*
     C                   MOVE      XXEMP#        QQEMP#            9
     C                   MOVE      XXFYR         QQFYR             3
     C*
     C     QQFND         CAT(P)    QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#
     C*
     C                   MOVEL     EMTPOB        FMTOBC
     C                   MOVEL     EMNAME        FMEMNM
     C                   MOVEL     EMFDNM        FMFENM
     C                   MOVEL     XXFNST        FMFNST
     C                   MOVEL     FNNAME        FMFNNM
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP
     C     ELKY2         CHAIN     PEL02                              61
     C     *IN61         DOWEQ     '0'
     C                   ADD       ELENMP        XXENMP
     C     ELKY2         READE     PEL02                                  61
     C                   ENDDO
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     XXENMP        FMPREM
     C                   MOVE      XXAGT#        FMAGT#
     C                   MOVEL     AGNAME        FMAGNM
     C                   MOVEL     AGPST         FMAGST
     C                   MOVEL     XXCPL#        FMCPL#
     C                   MOVE      XXCARR        FMCARR
     C                   MOVEL     XXEAD1        FMEAD1
     C                   MOVEL     XXEAD2        FMEAD2
     C                   MOVEL     XXEAD3        FMEAD3
     C                   MOVEL     XXEMST        FMEMST
     C*
     C                   MOVE      ELANDT        DSDAT7
     C                   MOVE      DSDD          FMANDA
     C                   MOVE      DSMM          FMANMO
     C*
     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMDT
     C*
     C                   MOVEL     DSTIME        FMTIME
     C                   MOVEL     'A'           FMSTAT
     C                   MOVEL     Q1USER        FMIUSR
     C                   MOVEL     Q1PGM         FMIPGM
     C                   MOVE      FMDT          FMIDT
     C*
     C                   ENDSR
     C********************************************************************
     C*  OTHINF - LOAD FORM SPECIFIC INFORMATION
     C********************************************************************
     C     OTHINF        BEGSR
     C*   MOVE THE INPUT & WORK FIELDS TO THE OUTPUT RECORD HERE
     C*
     C**** Begin Delete *** 01/14/21 *************************************
     C*                  MOVE      XXPFDT        ENPDT8
     C*                  MOVE      XXPFDT        ENEED8
     C*                  MOVEL     FNNAME        ENFNNM
     C*                  MOVEL     XXENAM        ENNAME
     C*                  MOVEL     XXCONT        ENCONT
     C*                  MOVEL(P)  XXATTN        ENATTN
     C*                  MOVEL     XXCNT3        ENCNT3
     C*                  MOVE      SVFEIN        ENFEID
     C*                  MOVEL     XXTPOB        ENTPOB
     C*
     C*                  MOVEL     XXEAD1        ENMAD1
     C*                  MOVEL     XXEAD2        ENMAD2
     C*                  MOVEL     XXEAD3        ENMAD3
     C*
     C*                  MOVEL     XXEAD1        ENPAD1
     C*                  MOVEL     XXEAD2        ENPAD2
     C*                  MOVEL     XXEAD3        ENPAD3
     C*
     C*                  MOVEL     XXCNTY        ENCNTY
     C*                  MOVE      *BLANKS       ENOFID
     C*                  MOVE      *BLANKS       ENOFNM
     C*                  MOVE      *BLANKS       ENOENM
     C*
     C*                  MOVE      *BLANKS       ENOMA1
     C*                  MOVE      *BLANKS       ENOMA2
     C*
     C*                  MOVE      *BLANKS       ENOMA3
     C*
     C*                  MOVE      *BLANKS       ENOPA1
     C*                  MOVE      *BLANKS       ENOPA2
     C*
     C*                  MOVE      *BLANKS       ENOPA3
     C*
     C*                  MOVE      *BLANK        ENOCNT
     C*                  MOVE      *BLANK        ENOC1N
     C*                  MOVE      *BLANK        ENOC2N
     C*                  MOVE      *BLANK        ENOC3N
     C*                  MOVE      *BLANK        ENILCD
     C*                  MOVE      *BLANK        ENILDS
     C*                  MOVE      *BLANK        ENINL2
     C*                  MOVE      *BLANK        ENI2DS
     C*                  MOVE      XXEXM1        ENEXM1
     C*                  MOVE      XXEXM2        ENEXM2
     C*                  MOVE      XXEXM3        ENEXM3
     C*                  MOVE      XXEXD1        ENEXD1
     C*                  MOVE      XXEXD2        ENEXD2
     C*                  MOVE      XXEXD3        ENEXD3
     C*                  MOVEL     XXNAOB        ENNAOB
     C*                  MOVEL     XXPOL1        ENPOLY
     C*                  MOVE      XXCARR        ENCARR
     C*                  MOVE      X1PFDT        ENPFDT
     C*                  MOVE      X1PTDT        ENPTDT
     C*                  MOVEL     XXRSK#        ENRSK#
     C*                  MOVEL(P)  XXDESC        ENDSC1
     C*
     C*  EMPLOYER INFORMATION THAT PRINTS AT THE TOP OF THE ENDORSEMENT
     C*  FORM SHOULD BE INFO FOR THE STATE REPORTING DIVISION
     C*
     C*                  MOVE      *BLANKS       ENNAME
     C*                  MOVE      *BLANKS       ENFDNM
     C*                  MOVE      *BLANKS       ENPAD1
     C*                  MOVE      *BLANKS       ENPAD2
     C*                  MOVE      *BLANKS       ENPAD3
     C*                  MOVE      *BLANKS       ENMAD1
     C*                  MOVE      *BLANKS       ENMAD2
     C*                  MOVE      *BLANKS       ENMAD3
     C*                  MOVE      *BLANKS       ENCPL#
     C*                  Z-ADD     *ZEROS        ENPFD8
     C*                  Z-ADD     *ZEROS        ENPTD8
     C*
     C*    EMKY1         CHAIN     PEM01                              50
     C*    *IN50         DOWEQ     '0'
     C*
     C*    EMSTRP        IFEQ      'Y'
     C*                  MOVEL     EMNAME        ENNAME
     C*                  MOVEL     EMFDNM        ENFDNM
     C*
     C*    EMMCTY        IFNE      *BLANKS
     C*    EMMST         ORNE      *BLANKS
     C*    EMMZIP        ORNE      *ZEROS
     C*                  MOVEL     EMMZIP        XXZIP5            5
     C*                  MOVE      EMMZIP        XXZIP4            4
     C*    EMMCTY        CAT(P)    ',':0         XXMCSZ
     C*                  CAT       EMMST:1       XXMCSZ
     C*                  CAT       XXZIP5:1      XXMCSZ
     C*    XXZIP4        IFNE      *BLANKS
     C*    XXZIP4        ANDNE     *ZEROS
     C*                  CAT       '-':0         XXMCSZ
     C*                  CAT       XXZIP4:0      XXMCSZ
     C*                  ENDIF
     C*                  ENDIF
     C*
     C*    EMPCTY        IFNE      *BLANKS
     C*    EMPST         ORNE      *BLANKS
     C*    EMPZIP        ORNE      *ZEROS
     C*                  MOVEL     EMPZIP        XXZIP5
     C*                  MOVE      EMPZIP        XXZIP4
     C*    EMPCTY        CAT(P)    ',':0         XXPCSZ
     C*                  CAT       EMPST:1       XXPCSZ
     C*                  CAT       XXZIP5:1      XXPCSZ
     C*    XXZIP4        IFNE      *BLANKS
     C*    XXZIP4        ANDNE     *ZEROS
     C*                  CAT       '-':0         XXPCSZ
     C*                  CAT       XXZIP4:0      XXPCSZ
     C*                  ENDIF
     C*                  ENDIF
     C*
     C*                  MOVEL     EMMAD1        ENMAD1
     C*                  MOVEL     EMMAD2        ENMAD2
     C*                  MOVEL     XXMCSZ        ENMAD3
     C*
     C*    EMMAD2        IFEQ      *BLANKS
     C*                  MOVEL     XXMCSZ        ENMAD2
     C*                  MOVEL     *BLANKS       ENMAD3
     C*                  ENDIF
     C*
     C*                  MOVEL     EMPAD1        ENPAD1
     C*                  MOVEL     EMPAD2        ENPAD2
     C*                  MOVEL     XXPCSZ        ENPAD3
     C*    EMPAD2        IFEQ      *BLANKS
     C*                  MOVEL     XXPCSZ        ENPAD2
     C*                  MOVEL     *BLANKS       ENPAD3
     C*                  ENDIF
     C*
     C*    ENPAD1        IFEQ      *BLANKS
     C*                  MOVEL     ENMAD1        ENPAD1
     C*                  MOVEL     ENMAD2        ENPAD2
     C*                  MOVEL     ENMAD3        ENPAD3
     C*                  ENDIF
     C*
     C*    RPKY          CHAIN     PRP01                              60
     C*    *IN60         IFEQ      '0'
     C*                  MOVEL     RPCAP#        ENCPL#
     C*                  END
     C*
     C*    ELKY          CHAIN     PEL01                              60
     C*    *IN60         IFEQ      '0'
     C*                  Z-ADD     ELPFDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         ENPFD8
     C*                  Z-ADD     ELPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         ENPTD8
     C*                  END
     C*
     C*                  END                                                    EMSTRP='Y'
     C*
     C*    EMKY1         READE     PEM01                                  50
     C*                  ENDDO
     C*
     C*                  MOVE      *ZEROS        ENAGT#
     C*                  MOVE      *BLANKS       ENAGNM
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C*    AHKY          SETGT     PAH01
     C*                  READP     PAH01                                  50
     C*    *IN50         DOWEQ     '0'
     C*    AHSTAT        ANDEQ     'I'
     C*                  READP     PAH01                                  50
     C*                  END
     C*
     C*    *IN50         IFEQ      '0'
     C*    ENCO#         ANDEQ     AHCO#
     C*    ENFND         ANDEQ     AHFND
     C*    ENEMP#        ANDEQ     AHEMP#
     C*    ENDIV         ANDEQ     AHDIV
     C*                  MOVEL     AHAGT#        ENAGT#
     C*
     C*    AAKY          CHAIN     PAA01                              50
     C*    *IN50         IFEQ      '0'
     C*                  MOVEL     AAFEIN        XXFEIN
     C*                  MOVEL     AAMOD         XXMOD
     C*                  ELSE
     C*                  MOVE      *ZEROS        XXFEIN
     C*                  MOVE      *ZEROS        XXMOD
     C*                  END
     C*
     C*    AGKY          CHAIN     PAG01                              50
     C*    *IN50         IFEQ      '0'
     C*                  MOVEL     AGNAME        ENAGNM
     C*                  END
     C*                  END
     C**** End   Delete *** 01/14/21 *************************************
     C**** Beg   Add    *** 01/14/21 *************************************
     C* New code below
     C                   MOVE      XXPFDT        xENPDT8
     C                   MOVE      XXPFDT        xENEED8
     C                   MOVEL     FNNAME        xENFNNM
     C                   MOVEL     XXENAM        xENNAME
     C                   MOVEL     XXCONT        xENCONT
     C                   MOVEL(P)  XXATTN        xENATTN
     C                   MOVEL     XXCNT3        xENCNT3
     C                   MOVE      SVFEIN        xENFEID
     C                   MOVEL     XXTPOB        xENTPOB
     C*
     C                   MOVEL     XXEAD1        xENMAD1
     C                   MOVEL     XXEAD2        xENMAD2
     C                   MOVEL     XXEAD3        xENMAD3
     C*
     C                   MOVEL     XXEAD1        xENPAD1
     C                   MOVEL     XXEAD2        xENPAD2
     C                   MOVEL     XXEAD3        xENPAD3
     C*
     C                   MOVEL     XXCNTY        xENCNTY
     C                   MOVE      *BLANKS       xENOFID
     C                   MOVE      *BLANKS       xENOFNM
     C                   MOVE      *BLANKS       xENOENM
     C*
     C                   MOVE      *BLANKS       xENOMA1
     C                   MOVE      *BLANKS       xENOMA2
     C*
     C                   MOVE      *BLANKS       xENOMA3
     C*
     C                   MOVE      *BLANKS       xENOPA1
     C                   MOVE      *BLANKS       xENOPA2
     C*
     C                   MOVE      *BLANKS       xENOPA3
     C*
     C                   MOVE      *BLANK        xENOCNT
     C                   MOVE      *BLANK        xENOC1N
     C                   MOVE      *BLANK        xENOC2N
     C                   MOVE      *BLANK        xENOC3N
     C                   MOVE      *BLANK        xENILCD
     C                   MOVE      *BLANK        xENILDS
     C                   MOVE      *BLANK        xENINL2
     C                   MOVE      *BLANK        xENI2DS
     C                   MOVE      XXEXM1        xENEXM1
     C                   MOVE      XXEXM2        xENEXM2
     C                   MOVE      XXEXM3        xENEXM3
     C                   MOVE      XXEXD1        xENEXD1
     C                   MOVE      XXEXD2        xENEXD2
     C                   MOVE      XXEXD3        xENEXD3
     C                   MOVEL     XXNAOB        xENNAOB
     C                   MOVEL     XXPOL1        xENPOLY
     C                   MOVE      XXCARR        xENCARR
     C                   MOVE      X1PFDT        xENPFDT
     C                   MOVE      X1PTDT        xENPTDT
     C                   MOVEL     XXRSK#        xENRSK#
     C                   MOVEL(P)  XXDESC        xENDSC1
     C*
     C*  EMPLOYER INFORMATION THAT PRINTS AT THE TOP OF THE ENDORSEMENT
     C*  FORM SHOULD BE INFO FOR THE STATE REPORTING DIVISION
     C*
     C                   MOVE      *BLANKS       xENNAME
     C                   MOVE      *BLANKS       xENFDNM
     C                   MOVE      *BLANKS       xENPAD1
     C                   MOVE      *BLANKS       xENPAD2
     C                   MOVE      *BLANKS       xENPAD3
     C                   MOVE      *BLANKS       xENMAD1
     C                   MOVE      *BLANKS       xENMAD2
     C                   MOVE      *BLANKS       xENMAD3
     C                   MOVE      *BLANKS       xENCPL#
     C                   Z-ADD     *ZEROS        xENPFD8
     C                   Z-ADD     *ZEROS        xENPTD8
     C*
     C     EMKY1         CHAIN     PEM01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVEL     EMNAME        xENNAME
     C                   MOVEL     EMFDNM        xENFDNM
     C*
     C     EMMCTY        IFNE      *BLANKS
     C     EMMST         ORNE      *BLANKS
     C     EMMZIP        ORNE      *ZEROS
     C                   MOVEL     EMMZIP        XXZIP5            5
     C                   MOVE      EMMZIP        XXZIP4            4
     C     EMMCTY        CAT(P)    ',':0         XXMCSZ
     C                   CAT       EMMST:1       XXMCSZ
     C                   CAT       XXZIP5:1      XXMCSZ
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXMCSZ
     C                   CAT       XXZIP4:0      XXMCSZ
     C                   ENDIF
     C                   ENDIF
     C*
     C     EMPCTY        IFNE      *BLANKS
     C     EMPST         ORNE      *BLANKS
     C     EMPZIP        ORNE      *ZEROS
     C                   MOVEL     EMPZIP        XXZIP5
     C                   MOVE      EMPZIP        XXZIP4
     C     EMPCTY        CAT(P)    ',':0         XXPCSZ
     C                   CAT       EMPST:1       XXPCSZ
     C                   CAT       XXZIP5:1      XXPCSZ
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXPCSZ
     C                   CAT       XXZIP4:0      XXPCSZ
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     EMMAD1        xENMAD1
     C                   MOVEL     EMMAD2        xENMAD2
     C                   MOVEL     XXMCSZ        xENMAD3
     C*
     C     EMMAD2        IFEQ      *BLANKS
     C                   MOVEL     XXMCSZ        xENMAD2
     C                   MOVEL     *BLANKS       xENMAD3
     C                   ENDIF
     C*
     C                   MOVEL     EMPAD1        xENPAD1
     C                   MOVEL     EMPAD2        xENPAD2
     C                   MOVEL     XXPCSZ        xENPAD3
     C     EMPAD2        IFEQ      *BLANKS
     C                   MOVEL     XXPCSZ        xENPAD2
     C                   MOVEL     *BLANKS       xENPAD3
     C                   ENDIF
     C*
     C     xENPAD1       IFEQ      *BLANKS
     C                   MOVEL     xENMAD1       xENPAD1
     C                   MOVEL     xENMAD2       xENPAD2
     C                   MOVEL     xENMAD3       xENPAD3
     C                   ENDIF
     C*
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     RPCAP#        xENCPL#
     C                   END
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         xENPFD8
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         xENPTD8
     C                   END
     C*
     C                   END                                                    EMSTRP='Y'
     C*
     C     EMKY1         READE     PEM01                                  50
     C                   ENDDO
     C*
     C                   MOVE      *ZEROS        xENAGT#
     C                   MOVE      *BLANKS       xENAGNM
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C     xENCO#        ANDEQ     AHCO#
     C     xENFND        ANDEQ     AHFND
     C     xENEMP#       ANDEQ     AHEMP#
     C     xENDIV        ANDEQ     AHDIV
     C                   MOVEL     AHAGT#        xENAGT#
     C*
     C     AAKY          CHAIN     PAA01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
     C                   END
     C*
     C     AGKY          CHAIN     PAG01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AGNAME        xENAGNM
     C                   END
     C                   END
     C**** End   Add    *** 01/14/21 *************************************

     C                   MOVE      QQEFDT        FMDT01
     C                   Z-ADD     XXPCT         FM5211
     C*
     C                   WRITE     PFM01
     C                   WRITE     PEN011
     C*
     C                   ENDSR
     C********************************************************************
     C*  WRITEM - WRITE THE WMMUP RECORD FOR THIS EMPLOYER
     C********************************************************************
     C     WRITEM        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   CLEAR                   PMU02
     C*
     C                   Z-ADD     XXCO#         MUCO#
     C                   Z-ADD     XXFND         MUFND
     C                   Z-ADD     XXEMP#        MUEMP#
     C                   Z-ADD     XXDIV         MUDIV
     C                   Z-ADD     XXFYR         MUFYR
     C                   MOVEL     XKFORM        MUFORM
     C                   Z-ADD     XXEND#        MUEND#
     C                   MOVE      *ZEROS        MUSEQ#
     C                   MOVEL     ENST          MUENST
     C                   MOVEL     ENDESC        MUDESC
     C                   MOVE      ELPFDT        MUEEDT
     C                   MOVEL     'I'           MUEXFL
     C                   MOVEL     ENMNAU        MUMNAU
     C                   MOVEL     ENPRNT        MUPRNT
     C                   MOVEL     'A'           MUSTAT
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     DSTIME        MUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C*
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      MUDT          MUIDT
     C*
     C                   WRITE     PMU02
     C*
     C                   ENDSR
     C****************************************************************
