     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXDEDF - POPULATE DEDUCTIBLE DETAILS INTO WDELP
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/21/09  C:65  PROJECT: 93WIB                               *
     H*                  UPDATE PROGRAM TO UPDATE INFORMATION WITHOUT *
     H*                  CHECKING ACTIVE/INACTIVE STATUS ON CODES.    *
     H*                                                               *
     H*****************************************************************
     FWDELP     UF   E           K DISK
     FWTDDL2    IF   E           K DISK
     FWMRSL     IF   E           K DISK

     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXRTCD
     C*
     C*   KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDT             7 0
     C*
     C     DDKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
      *
     C     *LIKE         DEFINE    DDCODE        XXDDCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
      *
     C                   READ      PEL01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   CLEAR                   ELDEDP
     C                   CLEAR                   ELAMPO
     C                   CLEAR                   ELAMAG
     C                   CLEAR                   EL2DDP
     C                   CLEAR                   EL2APO
     C                   CLEAR                   EL2AAG
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
      *
      * FIRST DEDUCTIBLE DETAILS
      *
     C     ELDCDE        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  ELDCDE        XXDDCD
     C                   MOVE      ELDCEF        Q1DATE
     C                   MOVE      ELCO#         Q1CO#
     C                   MOVE      ELFND         Q1FND
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1DATE            7
     C*
     C                   MOVE      Q1DATE        XXDDT
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN DEL ***  12/21/09  *********************************
     C*********  DDSTAT    ANDEQ'A'
     C*** END DEL *****  12/21/09  *********************************
     C                   MOVE      DDTYPE        ELDEDP
     C                   Z-ADD     DDAMPO        ELAMPO
     C                   Z-ADD     DDAMAG        ELAMAG
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELDCDE IFNE
      *
      * SECOND DEDUCTIBLE DETAILS
      *
     C     ELDCD2        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  ELDCD2        XXDDCD
     C                   MOVE      ELD2EF        Q1DATE
     C                   MOVE      ELCO#         Q1CO#
     C                   MOVE      ELFND         Q1FND
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1DATE            7
     C*
     C                   MOVE      Q1DATE        XXDDT
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN DEL ***  12/21/09  *********************************
     C*********  DDSTAT    ANDEQ'A'
     C*** END DEL *****  12/21/09  *********************************
     C                   MOVE      DDTYPE        EL2DDP
     C                   Z-ADD     DDAMPO        EL2APO
     C                   Z-ADD     DDAMAG        EL2AAG
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELDCD2 IFNE
      *
     C                   UPDATE    PEL01
      *
     C                   READ      PEL01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
