     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS711R   Get Image Info for requested document
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
      *  12/11/12 C:33  Project: SY266  #1404
      *                 No need to check workstation information.
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
      * Work station ID file
     FIMWSL2    IF   E           K DISK
      * Document Object (transaction) File
     FIMDOL     IF   E           K DISK
     FIMDOL1    UF   E           K DISK
     F                                     RENAME(PDO01:PDO02)
     F*
      * Document Type File
     FIMDTL     IF   E           K DISK
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     d isSkipWS        s              1n

     C*
     C*** DOCUMENT TRANSACTION FILE
     C*
     C     DOKY1         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
     C*
     C     DOKY2         KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C     DOKY3         KLIST
     C                   KFLD                    DODCID
     C*
     C*** WORKSTATION FILE
     C*
     C     WSKY1         KLIST
     C                   KFLD                    QQJBNM
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
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
     C*
      /Free
       isSkipWS = qqRtCd = '*';
      /End-free
     C*
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C                   MOVEL     *BLANKS       QQRTCD
     C*
      *================================================================
     C*
     C*
      /Free
       Clear WSvQue;
       Clear WSvQLb;
       Clear WSiDrv;
       Chain(n) wsky1 pws01;
       q1VQue = WSvQue;
       q1VQLb = WSvQLb;
       q1IDrv = WSiDrv;
       Select;
       When isSkipWS;
          qqRtCd = ' ';
       When not %found;
          qqRtCd = '1';
       Endsl;
      /End-free
     C*
     C*
     C     QQKEY         IFEQ      *BLANKS
     C     DOKY2         CHAIN     PDO02                              60
     C                   ELSE
     C     DOKY1         CHAIN     PDO01                              60
     C  N60DOKY3         CHAIN     PDO02                              60         SHLD ALWS
     C                   ENDIF                                                   GET HIT
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DODCID        Q1DCID           12
     C                   MOVEL     DOFLR         Q1FLR             8
     C                   MOVEL     DOSFLR        Q1SFLR           12
     C                   MOVEL     DOOPTF        Q1OPTF            1
     C                   MOVEL     DODTYP        Q1DTYP            8
     C                   MOVE      DOUNCN        QQUNCN
     C                   MOVE      DOIFSP        QQIFSP
     C*
     C     DOARQS        ADD       1             DOARQS
     C*
     C     QQKEY         IFEQ      *BLANKS
     C                   UPDATE    PDO02
     C                   ELSE
     C                   UPDATE    PDO02
     C                   ENDIF
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
     C                   MOVE      *BLANKS       QQUNCN
     C                   MOVE      *BLANKS       QQIFSP
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
