     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: This program is used to execute a SQL statement
     h*
     h*  Called from:
     h*  Calls to:
     h*
     h*  Created on 01/14/16   by C:300 for 37399
     h*******************
     h* Revisions:
     h*
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium
      /copy sPrString
      * *ENTRY
     DRUNSQL           pr                  ExtPgm('RUNSQL')
     D                              600
      *
     DRUNSQL           pi
     D sqlstmt                      600

      /Free

          Exec Sql Execute IMMEDIATE :SqlStmt;


       Return;
      /End-Free
