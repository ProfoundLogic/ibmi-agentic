     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*
     F*  Physical file definition (for maintaining record)
     FSPLTEMP   IF   E             DISK
     F                                     RENAME(SPL01:DBRCDF)

     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Work with Object Loc-
     D                                     ks for VDARTS')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C   KL
     CANNKL              MOVE      *OFF          *INKL
     C   KC
     CANNKC              MOVE      *OFF          *INKC
     C***
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C                   CALL      'IWRKJOB'
     C                   PARM                    JOBN
     C                   PARM                    USRN
     C                   PARM                    JOB#
     C                   PARM                    RTCD              1
     C*
     C                   MOVE      *ON           *INLR
