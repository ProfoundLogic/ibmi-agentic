     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h debug(*yes)

      /title SA043R - Retrieve WCMS Application Values
      *****************************************************************
      * PROGRAM DESCRIPTION:
      *
      *   Retrieve if "Claims Only" client.
      *
      *****************************************************************
      * Revised:
      *
      * 10/10/11 C:61  WA034 78195
      *                Created.
      *
      *****************************************************************


     Fswcpf     if   e           k disk


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm                    wcfl08            1

     c                   eval      wcfl08 = ' '
     C                   read      swcpf

     c                   eval      *inlr = *on


