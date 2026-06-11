     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/TITLE V31MMOPP - Conversion pgm to update menu opts
     h option(*nodebugio:*srcstmt)
     h**************************************************************************
     h*  DESCRIPTION:
     h*
     h*  Called from:  SYPOSTPGM2 - Postload conversion program - Processing Program
     h*  Calls      :  None
     h*
     h**************************************************************************
     h*  Created on 01/11/13     c:33      Project SY266
     h**************************************************************************
     h* Revisions:
     h*
     h**************************************************************************
      *
      /Free
       Exsr AuditTracking;
       *inlr = *on;

       //===================================================================
       // Update MMOPP for each record that calls W4700R
       Begsr AuditTracking;

       exec sql
        update mmopp
          set OPEXCL =
           'WTPOLTRACK             (''000000000'' ''C'' '' '' ''00000'' ''M'')',
           OPMNTT = 'Audit Tracking'
        where
         OPEXCL =
           'W4700R                 ('' '' '' '' '' '')';

       exec sql
        update mmopp
          set OPEXCL =
           'WTPOLTRACK             (''000000000'' ''D'' '' '' ''00000'' ''M'')',
           OPMNTT = 'Audit Tracking - Inquiry'
        where
         OPEXCL =
           'W4700R                 ('' '' '' '' ''1'')';

       Endsr;
      /End-free
