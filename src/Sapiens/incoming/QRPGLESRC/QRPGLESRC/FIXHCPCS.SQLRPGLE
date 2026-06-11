     H* (c) 2017 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H optimize(*none)
     H*****************************************************************
     H/TITLE FIXHCPCS  - Fix Flag for IL Category HCPCS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program read through FMF1PIL and updates flags for IL    *
     H* Category HCPCS                                                *
     H*                                                               *
     H*  Created By:                                                  *
     H*  01/20/16  C:309 PROJECT: IL009                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  02/27/17  C:309 Project: IL010                               *
     H*            Changed effective date to 1170101 and ran for this *
     H*            project.                                           *
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
     C* Fix FMF1PIL : Fix Flag for IL Category HCPCS
     C*========================================================================
     C*
      /Free

        Exec Sql
          set Option commit = *none;

        CurTime = %Dec(%Char(%Time():*iso0):6:0);
        CurDate = %Dec(%Char(%Date():*cymd0):7:0);

        Exec SQL
          Update FMF1PIL
          Set F1glob= 0       , F1mult= ' '     , F1bila= ' ' ,
              F1asst= ' '     , F1cosg= ' '     , F1tesg= ' ' ,
              F1USER= :q1user , F1PGM = :q1pgm  ,
              F1TIME= :curtime, F1DT  = :curdate
          Where (F1cat = 'HCPCS' or F1code >= '70000')
          // *** Beg Chg *** 02/27/17 **************************
          // *** and F1efDt ='1160101';
              and F1efDt ='1170101';
          // *** End Chg *** 02/27/17 **************************

        *InLr=*On;
      /End-Free
