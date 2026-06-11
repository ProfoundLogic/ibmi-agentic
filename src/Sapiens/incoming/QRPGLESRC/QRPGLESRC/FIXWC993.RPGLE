     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWC993 - SET TEST GROUP = Y FOR ALL FUND NOS = 999
     H*                 Created C: 58     PROJECT WC993
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*****************************************************************
     FWMFNL     UPE  E           K DISK
     F*  GROUP MASTER

     I*
     IPFN01         51

     C*
     C     *IN51         IFEQ      *ON
     C     FNFND         ANDEQ     999
     C                   MOVE      'Y'           FNTEST
     C                   UPDATE    PFN01
     C                   ENDIF
     C*
