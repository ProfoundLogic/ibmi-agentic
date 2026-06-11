     H* (c) 2015 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H optimize(*none)
     H*****************************************************************
     H/TITLE FIXSTSTP2 - FIX STSTP IN-STATE ADJ FLAG
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program fixes the "IN-STATE ADJ FLAG" in State Code Table*
     H*                                                               *
     H*  Created By:                                                  *
     H*  12/14/15  C:309 Support: 38813                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     D*================================================================
     D* Stand alone fields
     D*================================================================
     D psds          esds                  extname(szq1p)
      *
     D CurTime         S              6  0
     D CurDate         S              7  0
      *

     C*========================================================================
     C* Fix STSTP : "IN-STATE ADJ FLAG"
     C*========================================================================
     C*
      /Free

        Exec Sql
          set Option commit = *none;

        CurTime = %Dec(%Char(%Time():*iso0):6:0);
        CurDate = %Dec(%Char(%Date():*cymd0):7:0);

        Exec SQL
          Update STSTP
          Set STINST = 'Y', STUSER=:q1user, STPGM=:q1pgm,
          STTIME=:curtime, STDT=:curdate
          Where STKEY = 'NY';

        *InLr=*On;
      /End-Free
