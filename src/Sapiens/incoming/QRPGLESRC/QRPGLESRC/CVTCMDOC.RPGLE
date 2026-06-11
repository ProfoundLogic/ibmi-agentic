     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSDCMLH    UF   E             DISK
     F*

     D                 DS
     D  $D                     1     12
     D                                     DIM(12)
     D  $D1                    1      1
     D  $D2                    2      2
     D  $D3                    3      3
     D  $D4                    4      4
     D  $D5                    5      5
     D  $D6                    6      6
     D  $D7                    7      7
     D  $D8                    8      8
     D  $D9                    9      9
     D  $D10                  10     10
     D  $D11                  11     11
     D  $D12                  12     12

     C*
     C     1             SETLL     PCM01
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL     *BLANKS       $D
     C                   MOVEL     *BLANKS       $$FX
     C                   MOVEA     CMDCID        $D
     C*
     C                   Z-ADD     1             Z                 3 0
     C     '.'           LOOKUP    $D(Z)                                  61
     C*** IF THIS IS AN EMPLOYER BASED RECORD.
     C     *IN61         IFEQ      *ON
     C                   ADD       1             Z
     C     Z             IFLE      10
     C                   MOVEA     $D            $DA              12
     C*
     C     3             SUBST     $DA:Z         $$FX              3
     C     $$FX          IFEQ      'FAX'
     C                   MOVEL     'F'           CMIMGF
     C                   MOVEL     'CVTIT '      CMIPTH
     C                   UPDATE    PCM01
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READ      PCM01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
