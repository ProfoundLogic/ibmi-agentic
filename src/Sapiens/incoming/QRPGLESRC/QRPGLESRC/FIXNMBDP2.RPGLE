     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXNMBDP2                                                                      *
     H**************************************************************************************
     H*  Description:  Move percentages to bigger fields                                   *
     H*                                                                                    *
     H*  Created: 10/20/14   Programmer C:079   Project: 114MMP                            *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FNMBDP     UF   E             DISK
     D* Dspecs
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*----------------------------------------------------------------------
     D*  Define stand alone fields
     D*----------------------------------------------------------------------
     D wkBDPC01        S              9  2
     D wkBDPC02        S              9  2
     D wkBDPC03        S              9  2
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D/copy sPrDate
     C*
     C*----------------------------------------------------------------------
     C*  Read through file
     C*----------------------------------------------------------------------
     C*
     C                   Read      PBD01                                  99
     C*
     C                   Dow       (*in99 = *off)
     C                   If        BDTYPE = 'DH'
     C                   Eval      BDAm12 = BDPC01 * 100
     C                   Eval      BDAm13 = BDPC02 * 100
     C                   Eval      BDAm14 = BDPC03 * 100
     C                   Eval      BDPgm = Q1Pgm
     C                   Eval      BDPgm = Q1User
      /free
                         BDDt   = DateTo7(systemDate());
                         BDTime = TimeTo6(SystemTime);
      /end-free
     C                   Update    PBD01
     C                   Endif                                                  *in99=*off
     C                   Read      PBD01                                  99
     C                   Enddo                                                  *in99=*off
     C*
     C                   Move      *on           *inlr
