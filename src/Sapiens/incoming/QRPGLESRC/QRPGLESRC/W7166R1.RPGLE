     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7166R1 - Find Certificate Letter Folder and Document
     H***************************************************************
     H**  GIVEN: CO#, FND, EMP#, DIV, FYR, PGM (CX / CF / RE)
     H**  RETURNS:
     H**    Client specified FOLDER and DOCUMENT from WTXCP
     H**    (WW groups, opt 19) if entered.  Otherwise returns
     H**    default values for certificate FOLDER/DOCUMENT.
     H**
     H**  CALLED FROM:
     H**     W7166C
     H**
     H**  CALLS TO:
     H**     NONE
     H**
     H***************************************************************
     H* REVISIONS:
     H*                                                               *
     H* 01/22/13 C:287 Seq#: SY266_OVR                                *
     H*                Added DFTACTGRP and ACTGRP compile options     *
     H*
     H* 09/02/05 C:61 PROJECT: WC1359  129WLT
     H*               Created.
     H*
     H***************************************************************
      *
      *  Employer Policy Year
     FWDELL     IF   E           K DISK
      *
      *  Rating State
     FWMRSL     IF   E           K DISK
      *
      *  Rating State
     FWTXCL     IF   E           K DISK
      *
      *
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *============================================================
      *  KLIST
      *============================================================
      *
      *  Employer Policy Year
      *

     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
      *
      * KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *  CX/REIN state
      *
     C     XCKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
      *
     C     XCKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
      *
      *
      *============================================================
      *  *LIKE DEFN
      *============================================================
     C     *LIKE         DEFINE    ELCO#         KKCO#
     C     *LIKE         DEFINE    ELFND         KKFND
     C     *LIKE         DEFINE    ELEMP#        KKEMP#
     C     *LIKE         DEFINE    ELDIV         KKDIV
     C     *LIKE         DEFINE    ELFYR         KKFYR
      *
      *
      *============================================================
      *  PLIST
      *============================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@FYR             3
     C                   PARM                    @@PGM             2            CX  CF  RE
     C                   PARM                    @@FLR            80
     C                   PARM                    @@DOC            12
      *
      *=============================================================
      * MAIN
      *=============================================================
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@EMP#        KKEMP#
     C                   MOVE      @@DIV         KKDIV
     C                   MOVE      @@FYR         KKFYR
      *
      *  Establish Default values
      *
     C                   SELECT
      *
     C     @@PGM         WHENEQ    'CF'
     C                   MOVEL(P)  'WCDOC'       @@FLR
     C                   MOVEL(P)  'CFCERT'      @@DOC
      *
     C     @@PGM         WHENEQ    'CX'
     C                   MOVEL(P)  'WCDOC'       @@FLR
     C                   MOVEL(P)  'CXCERT'      @@DOC
      *
     C     @@PGM         WHENEQ    'RE'
     C                   MOVEL(P)  'WCDOC'       @@FLR
     C                   MOVEL(P)  'RECERT'      @@DOC
      *
     C                   ENDSL
      *
      *  Check for Client Specified Values
      *
     C     ELKY          CHAIN     PEL01                              60
     C  N60RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     XCKY          SETGT     PXC01
     C     XCKY2         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     @@PGM         WHENEQ    'CF'
     C     XCCFLD        ANDNE     *BLANK
     C                   MOVEL(P)  XCCFLF        @@FLR
     C                   MOVEL(P)  XCCFLD        @@DOC
      *
     C     @@PGM         WHENEQ    'CX'
     C     XCCSCD        ANDNE     *BLANK
     C                   MOVEL(P)  XCCSCF        @@FLR
     C                   MOVEL(P)  XCCSCD        @@DOC
      *
     C     @@PGM         WHENEQ    'RE'
     C     XCCSRD        ANDNE     *BLANK
     C                   MOVEL(P)  XCCSRF        @@FLR
     C                   MOVEL(P)  XCCSRD        @@DOC
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  XCKY
      *
     C                   ENDIF                                                  RSKY
      *
      *  Prepend folder directory as necessary
      *
     C                   CALL      'SA039R'
     C                   PARM                    @@FLR            80
      *
     C                   SETON                                        LR
      *
