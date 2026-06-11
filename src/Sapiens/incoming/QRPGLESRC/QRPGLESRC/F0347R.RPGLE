     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H***sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H* PROGRAM: F0347R
     H* PROJECT: FL021   C:117
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 05/05/00  C:079  PROJ: FS211
     H*                  1. Condition update of PEO01
     H*                  2. Use new logical for Bill Entry
     H*****************************************************************
     F*
     FF0347F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*** BEG CHG *** 05/05/00 ****************************************
     F******FAEOL32 UF  E           K        DISK
     FFAEOL45   UF   E           K DISK
     F*** END CHG *** 05/05/00 ****************************************
     F*

     D OPTNM           C                   CONST('Reset EOMB for Data -
     D                                     Submission')

     I*

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@CALL            1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CNTL           20
     C                   PARM                    @@CONM           40
     C                   PARM                    @@USER           10
     C                   PARM                    @@EOB#            9 0
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCNTL
     C                   KFLD                    KKEOB#
     C*
     C     *LIKE         DEFINE    EOCO#         @@CO#
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOCNTL        KKCNTL
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*
     C                   MOVEL     @@CONM        SCCONM
     C                   MOVEL     @@USER        SCUSER
     C                   MOVEL     OPTNM         SCPRDS
     C*
     C                   EXFMT     SCR01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVEL     @@CO#         KKCO#
     C                   MOVEL(P)  @@CNTL        KKCNTL
     C                   Z-ADD     @@EOB#        KKEOB#
     C     EOKY          CHAIN     PEO01                              60
     C*** BEG ADD *** 05/05/00 ****************************************
     C     *IN60         IFEQ      *OFF
     C*** END ADD *** 05/05/00 ****************************************
     C     EOTAPE        IFEQ      'Y'
     C                   MOVE      *BLANKS       EOSBDT
     C                   ELSE
     C                   MOVE      *BLANKS       EOSBDT
     C                   MOVE      *BLANKS       EOCNTL
     C                   ENDIF                                                  EOTAPE=Y
     C*
     C                   UPDATE    PEO01
     C*** BEG ADD *** 05/05/00 ****************************************
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*** END ADD *** 05/05/00 ****************************************
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   SETON                                        LR
     C*
     C                   RETURN
     C*
