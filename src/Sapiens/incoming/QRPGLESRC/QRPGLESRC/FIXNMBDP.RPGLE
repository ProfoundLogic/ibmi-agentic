     H* (C) StoneRiver, 2014
     H/TITLE FIXNMBDP                                                                       *
     H**************************************************************************************
     H*  Description:  Correct percentages in file NMBDP                                   *
     H*                                                                                    *
     H*  Created: 05/19/14   Programmer C:274   Project: 114MMP                            *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FNMBDP     UF   E             DISK
     D*
     D*----------------------------------------------------------------------
     D*  Define stand alone fields
     D*----------------------------------------------------------------------
     D wkBDPC01        S              4  3
     D wkBDPC02        S              4  3
     D wkBDPC03        S              4  3
     D XXCLT#          S              3A
     C*
     C*----------------------------------------------------------------------
     C*  Get Client ID
     C*----------------------------------------------------------------------
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#
     C*
     C*----------------------------------------------------------------------
     C*  Read through file
     C*----------------------------------------------------------------------
     C*
     C*****        Only for client 114
     C                   If        XXCLT# = '114'
     C                   Read      PBD01                                  99
     C*
     C                   Dow       (*in99 = *off)
     C                   If        BDTYPE = 'DH'
     C                   Eval      wkBDPC01 = BDPC01 * 10
     C                   Eval      wkBDPC02 = BDPC02 * 10
     C                   Eval      wkBDPC03 = BDPC03 * 10
     C                   Eval      BDPC01 = wkBDPC01
     C                   Eval      BDPC02 = wkBDPC02
     C                   Eval      BDPC03 = wkBDPC03
     C                   Update    PBD01
     C                   Endif                                                  *in99=*off
     C                   Read      PBD01                                  99
     C                   Enddo                                                  *in99=*off
     C                   Endif                                                  XXCLT = 114
     C*
     C                   Move      *on           *inlr
