     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC864R                                        **
     H* DESCRIPTION: FOLDER DELETION                               **
     H*                                                            **
     H* CREATED BY:                                                **
     H***************************************************************
     H* REVISIONS:                                                 **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     FWMFNL     IF   E           K DISK
     FSAFPP     IF   E             DISK
     FALLFLRL   UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C*
     C     ALLKEY        KLIST
     C                   KFLD                    XXFLR            63
     C*
     C*
     C     FPKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C*
     C     FNKY2         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQCO#         XXCO#
     C*
     C     QQFND         IFEQ      *BLANKS
     C     QQFND         OREQ      '000'
     C*
     C     FNKY2         CHAIN     PFN01                              60
     C                   ELSE
     C     FNKY1         CHAIN     PFN01                              60
      *
     C                   ENDIF
      *
     C     *IN60         DOWEQ     '0'
     C                   MOVE      FNFND         XXFND
     C                   MOVE      FNFND         ##FND             3
     C*
     C     1             SETLL     PFP01
     C                   READ      PFP01                                  61
     C     *IN61         DOWEQ     *OFF
     C*
     C*** INITIALIZE FLR PATH VARIABLE
     C                   MOVEL     *BLANKS       XXFLR
     C*
     C     ##FND         CAT       '/'           XXW4              4
     C     XXW4          CAT       FPAOS         XXW6              6
     C     XXW6          CAT       '/'           XXW7              7
     C     XXW7          CAT       FPSFLR        XXFLR            63
     C*
     C     ALLKEY        CHAIN     FLRDTL                             62
     C     *IN62         DOWEQ     *OFF
     C*
     C                   CALL      'SC864C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    FPAOS
     C                   PARM                    FPSFLR
     C                   PARM                    XXFLR
     C                   PARM                    FLRNAM
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C*** RTCD IF = 'N', DIDNT DELETE DO TO DOC OR FLR EXISTING IN PATH.
     C*
     C     QQRTCD        IFNE      'N'
     C                   MOVEL     *ALL'9'       CRTTIM
     C                   UPDATE    FLRDTL
     C                   ENDIF
     C*
     C*
     C     ALLKEY        READE     FLRDTL                                 62
     C                   ENDDO
     C*
     C                   READ      PFP01                                  61
     C                   ENDDO
     C*
     C*
     C     QQFND         IFEQ      *BLANKS
     C     QQFND         OREQ      '000'
     C*
     C     FNKY2         READE     PFN01                                  60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C*
     C                   ENDDO
     C*
      *
      *
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
