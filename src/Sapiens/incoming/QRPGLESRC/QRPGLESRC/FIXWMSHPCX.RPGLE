     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMSHPCX
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     CHANGE SUBMISSIN HISTORY FILE WITH STAT OF '4', TYPE 'C'  *
     H*      AND 'R' WITH A SHSBDT GREATER THAN THE V3 LIVE DATE TO   *
     H*      HAVE A STATUS OF '3', SO THEY WILL BE RE-SUBMITTED.      *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:87                                            *
     H*   CREATED:    08/24/98    PROJECT: SUPP WA091 #13352          *
     H*                                                               *
     H*****************************************************************
     FWMSHL5    UF   E           K DISK

     C*
     C     SHKY          KLIST
     C                   KFLD                    XXSTAT            1
     C                   KFLD                    XXTYPE            1
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQV3DT            7
     C                   PARM                    QQFXDT            7
      *
     C                   MOVE      QQV3DT        XXV3DT            7 0
     C                   MOVE      QQFXDT        XXFXDT            7 0
      *
      *  CHANGE SHSTAT = '4' TO SHSTAT = '3' FOR CANCELLATIONS
      *
     C                   MOVE      '4'           XXSTAT
     C                   MOVE      'C'           XXTYPE
      *
     C     SHKY          CHAIN     PSH01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     SHSBDT        IFGE      XXV3DT
     C     SHSBDT        ANDLT     XXFXDT
     C                   MOVE      '3'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF
      *
     C     SHKY          READE     PSH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
      *  CHANGE SHSTAT = '4' TO SHSTAT = '3' FOR REINSTATEMENTS
      *
     C                   MOVE      '4'           XXSTAT
     C                   MOVE      'R'           XXTYPE
      *
     C     SHKY          CHAIN     PSH01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     SHSBDT        IFGE      XXV3DT
     C     SHSBDT        ANDLT     XXFXDT
     C                   MOVE      '3'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF
      *
     C     SHKY          READE     PSH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      *ON           *INLR
      *
