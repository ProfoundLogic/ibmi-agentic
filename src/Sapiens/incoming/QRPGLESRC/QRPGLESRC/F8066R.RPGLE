     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  12/02/11  C:277 Project: 114MCV                              *
     H*                                                               *
     H*  This program will extract EOMB Print and MICR Check Print    *
     H*  data to transmit to SIS.                                     *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FF8066F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     D*
     D*-------------------------------------------------------------------------
     D* Data Definition
     D*-------------------------------------------------------------------------
     D @@FLR           S             10A
     D XXFLR           S             10A
     D XXMBRSEQ        S              2P 0 INZ(00)
     D OPTNM           C                   CONST('SIS Bill Export')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@FLR
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C     UPKY          KLIST                                                      USER PROFILE
     C                   KFLD                    Q1USER
     C*
     C     UPKY          CHAIN     PUP01                              99
     C                   IF        *IN99 = *OFF
     C     COKY          CHAIN     PCO01
     C                   IF        *IN99 = *OFF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C                   EXFMT     SCR01
     C                   IF        *IN03 = *ON
     C                   EVAL      *INLR = *ON
     C                   ELSE
     C*
     C                   MOVEL     @@FLR         XXFLR
     C                   CALL      'F8066C'
     C                   PARM                    XXFLR
     C                   PARM                    XXMBRSEQ
     C                   ENDIF
     C* Program ends here
     C
     C                   EVAL      *INLR = *ON
     C                   RETURN
