     H* (c) 2014 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H optimize(*none)
     H*****************************************************************
     H/TITLE FIXMCOF - FIX MCO FEIN
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program fixes the MCO FEIN in the claim master file      *
     H*                                                               *
     H* CALLS TO:   NONE                                              *
     H*                                                               *
     H* CREATED:                                                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     H*

     c*========================================================================
     c* Fix MCO FEIN
     c*
     c*========================================================================
     c/exec SQL
     c+  update WMCMP
     c+    set CMMCOF = ' '
     c+    where CMMCOF = '000000000'
     c/end-exec
     c                   seton                                        LR
