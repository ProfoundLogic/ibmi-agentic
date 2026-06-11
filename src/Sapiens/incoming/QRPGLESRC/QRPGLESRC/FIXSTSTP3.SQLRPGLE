     H* (c) 2015 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H optimize(*none)
     H*****************************************************************
     H/TITLE FIXSTSTP3 - FIX STSTP IN-STATE ADJ FLAG
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program fixes the non adjudication state in State Table  *
     H*                                                               *
     H*  Created By:                                                  *
     H*  03/11/16  C:307 Support: 37159                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     D*================================================================
     D* Stand alone fields
     D*================================================================

     C*========================================================================
     C* Fix STSTP : "Non Adjudication state Flag"
     C*========================================================================
     C*
      /Free

          Exec SQL
          set Option commit = *none;

          Exec SQL
          Update STSTP
          Set ST1A2 = 'Y'
          Where STKEY in ('CR','MC','CC','PC','NW');

          Exec SQL
          Update STSTP
          Set ST1A5 = 'Y'
          Where STKEY in ('UC','PC','CC','CR','MC','US','P1','P2','CK');
        *InLr=*On;
      /End-Free
