     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/TITLE CVTMQUEINE - Conversion pgm to update menu opt IS215R to IS215C
     h option(*nodebugio:*srcstmt)
     h**************************************************************************
     h*  DESCRIPTION:
     h*
     h*  Called from:  SYPOSTPGM2 - Postload conversion program - Processing Program
     h*  Calls      :  None
     h*
     h**************************************************************************
     h*  Created on 11/05/2012 by c:282 for Project xxxxxx
     h**************************************************************************
     h* Revisions:
     h*
     h**************************************************************************
      * Update MMOPP for each record that calls IS215R.
      *
     c/exec sql
     c+ update mmopp
     c+ set  OPEXCL = 'IS215C                 (''          ''  '' '' '' '')'
     c+ where
     c+  OPEXCL like '%IS215R                 (''          ''  '' '')%'
     c/end-exec
      *
     c/exec sql
     c+ update mmopp
     c+ set  OPEXCL = 'IS215C                 (''          ''  ''L'' '' '')'
     c+ where
     c+  OPEXCL like '%IS215R                 (''          ''  ''L'')%'
     c/end-exec
      *
     c/exec sql
     c+ update mmopp
     c+ set  OPEXCL = 'IS215C                 (''          ''  ''S'' '' '')'
     c+ where
     c+  OPEXCL like '%IS215R                 (''          ''  ''S'')%'
     c/end-exec
      *
     c                   eval      *inlr = *on
