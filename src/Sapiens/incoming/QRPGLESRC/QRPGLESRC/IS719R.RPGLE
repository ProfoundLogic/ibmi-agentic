     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS719R   Get Image Info for requested document
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
      *  10/17/11 C:279 Project: SY195
      *                 Modify program to pass/return UNC path field
      *                 and IFS flag to called/calling programs.
      *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
      * PRINT SERVER ID FILE
     FIMPSL1    IF   E           K DISK
      * Document Object (transaction) File
     FIMDOL     IF   E           K DISK
      * Document Object (transaction) File
     FIMDOL1    UF   E           K DISK
     F                                     RENAME(PDO01:PDO01#)
      * Document Type File
     FIMDTL     IF   E           K DISK
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DOCUMENT TRANSACTION FILE
     C*
     C     DOKY1         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
     C*
     C*** DOCUMENT TRANSACTION FILE
     C*
     C     DOKY#         KLIST
     C                   KFLD                    DODCID
     C*
     C*
     C*** PRINT SERVER ID FILE
     C*
     C     WSKY1         KLIST
     C                   KFLD                    QQWSID
     C*
     C*
     C*** DOCUMENT TYPE FILE
     C*
     C     DTKY1         KLIST
     C                   KFLD                    Q1DTYP
     C*
     C*
     C     PL0130        PLIST
     C                   PARM                    QQWSID            2
     C                   PARM                    QQRTC             1
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5
     C                   PARM                    QQJBNM           10
     C                   PARM                    Q1DCID           12
     C                   PARM                    Q1FLR             8
     C                   PARM                    Q1SFLR           12
     C                   PARM                    Q1OPTF            1
     C                   PARM                    Q1IDRV            1
     C                   PARM                    Q1DTYP            8
     C                   PARM                    Q1DTDS           40
     C                   PARM                    Q1VQUE           10
     C                   PARM                    Q1VQLB           10
     C                   PARM                    QQRTCD            1
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
     C*** END ADD *** 10/17/11 ****************************************
     C*
     C*
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C*** CALL THE PRINTER SELECT PROGRAM
     C*
     C***** PRINTER SELECT - IS150R ***
     C                   CALL      'IS150R'      PL0130
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C     QQRTC         IFEQ      'B'
     C     QQWSID        OREQ      *BLANK
     C*
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*
      *================================================================
     C*
     C*
     C     WSKY1         CHAIN     PPS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PSPQUE        Q1VQUE
     C                   MOVEL     PSPQLB        Q1VQLB
     C                   MOVEL     PSIDRV        Q1IDRV            1
     C                   ELSE
     C                   MOVEL     *BLANKS       Q1VQUE
     C                   MOVEL     *BLANKS       Q1VQLB
     C                   MOVEL     *BLANKS       Q1IDRV            1
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C     DOKY1         CHAIN     PDO01                              60
     C  N60DOKY#         CHAIN     PDO01#                             60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DODCID        Q1DCID           12
     C                   MOVEL     DOFLR         Q1FLR             8
     C                   MOVEL     DOSFLR        Q1SFLR           12
     C                   MOVEL     DOOPTF        Q1OPTF            1
     C                   MOVEL     DODTYP        Q1DTYP            8
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   MOVE      DOUNCN        QQUNCN
     C                   MOVE      DOIFSP        QQIFSP
     C*** END ADD *** 10/17/11 ****************************************
     C*
     C     DOARQS        ADD       1             DOARQS
     C*
     C                   UPDATE    PDO01#
     C*
     C     DTKY1         CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DTDESC        Q1DTDS
     C                   ELSE
     C                   MOVEL     *BLANKS       Q1DTDS
     C                   ENDIF
      *
      *
     C*** IF DOCUMENT IS ON OPTICAL, WE MUST RETRIEVE IT AND STORE IT
     C*** IN A FOLDER TO ALLOW A NORMAL VIEW.
     C*** DO THAT HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     C***             REPLACE THE DOCID / FLR INFO AND RTRN TO CALLING
     C***             PGM !!!!!!!!!!!!!
      *
      *
      *
      *
      *
      *
      *
      *
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       Q1DCID           12
     C                   MOVEL     *BLANKS       Q1FLR             8
     C                   MOVEL     *BLANKS       Q1SFLR           12
     C                   MOVEL     *BLANKS       Q1OPTF            1
     C                   MOVEL     *BLANKS       Q1DTYP            8
     C                   MOVEL     *BLANKS       Q1DTDS           40
     C                   MOVEL     '1'           QQRTCD
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   MOVE      *BLANKS       QQUNCN
     C                   MOVE      *BLANKS       QQIFSP
     C*** END ADD *** 10/17/11 ****************************************
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C*
     C***
     C*
     C*
     C*
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
