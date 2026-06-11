     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS712R2  GET INFO  Info for requested document
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
      * SPLIT SERVER ID file
     FIMSSL     IF   E           K DISK
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
     C     DOKY#         KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C*** SPLIT SERVER FILE
     C*
     C     TSKY1         KLIST
     C                   KFLD                    QQTSID
     C*
     C*
     C*** DOCUMENT TYPE FILE
     C*
     C     DTKY1         KLIST
     C                   KFLD                    Q1DTYP
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5 0
     C                   PARM                    QQTSID            2
     C                   PARM                    Q1DCID           12
     C                   PARM                    Q1FLR             8
     C                   PARM                    Q1SFLR           12
     C                   PARM                    Q1OPTF            1
     C                   PARM                    Q1IDRV            1
     C                   PARM                    Q1DTYP            8
     C                   PARM                    Q1DTDS           40
     C                   PARM                    Q1SQUE           10
     C                   PARM                    Q1SQLB           10
     C                   PARM                    Q1RQUE           10
     C                   PARM                    Q1RQLB           10
     C                   PARM                    Q1TFLR           12
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
      *================================================================
     C*
     C*
     C     TSKY1         CHAIN     PSS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SSSSND        Q1SQUE
     C                   MOVEL     SSSSLB        Q1SQLB
     C                   MOVEL     SSSRCV        Q1RQUE
     C                   MOVEL     SSSRLB        Q1RQLB
     C                   MOVEL     SSTFLR        Q1TFLR
     C                   MOVEL     SSIDRV        Q1IDRV            1
     C                   ELSE
     C                   MOVEL     *BLANKS       Q1SQUE
     C                   MOVEL     *BLANKS       Q1SQLB
     C                   MOVEL     *BLANKS       Q1RQUE
     C                   MOVEL     *BLANKS       Q1RQLB
     C                   MOVEL     *BLANKS       Q1IDRV            1
     C                   MOVEL     *BLANKS       Q1TFLR
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C     DOKY#         CHAIN     PDO01#                             60
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
