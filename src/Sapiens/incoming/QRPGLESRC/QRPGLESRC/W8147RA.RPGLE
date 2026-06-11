     hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
    h/title W8147R - PA Security Fund Processing Program
     *====================================================================
    h*  Created by: C136 on 07/27/09 for project 114WUK                  *
     *====================================================================
    h* Revisions                                                         *
     *====================================================================
     c/exec sql
     c+ update WDPAP set PASTAT = ' '
     c+   where PASTAT = 'W'
     c/end-exec
     c                   return
