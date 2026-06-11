     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H* PROGRAM: SA006R                                              *
     H* DESCRIPTION: SECURITY CHECK      C:62 PROJECT:MC145 09/09/98 *
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H****************************************************************

     C*
     C* DISPLAY ERROR MESSAGE IF CONTROL RECORD DOESN'T EXIST OR
     C* USERS SECURITY LEVEL WAS LESS THAN THE CONTROL RECORD.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@ULVL            3 0          USER LEVEL
     C                   PARM                    @@UTYP            1            USER TYPE
     C                   PARM                    @@FLVL            3 0          FILE LEVEL
     C                   PARM                    @@FTYP            1            FILE TYPE
     C                   PARM                    @@RTCD            1            RETURN CODE
     C*
     C* IF THE USERS SECURITY LEVEL EQUALS THE CONTROL RECORDS
     C* SECURITY LEVEL THEN NEED TO MOVE ON TO THE TYPE CHECK.
     C*
     C     @@ULVL        IFLT      @@FLVL
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ELSE                                                   @@ULVL LT @@FTYP
     C*
     C     @@FTYP        IFNE      *BLANKS
     C*
     C* IF THE USERS SECURITY TYPE EQUALS THE CONTROL RECORDS SECURITY
     C* TYPE THEN THE SECURITY CHECK HAS PASSED.
     C*
     C     @@UTYP        IFNE      @@FTYP
     C*
     C* THE REMAINING TESTS TO DETERMINE IF SECURITY IS APPROVED
     C* OR DISAPPROVED.
     C*
     C                   SELECT
     C*
     C     @@FTYP        WHENEQ    '&'                                          1
     C     @@UTYP        IFGE      '0'                                          1
     C     @@UTYP        ANDLE     '9'                                          1
     C     @@UTYP        OREQ      '%'                                          1
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@UTYP IFGE '0'
     C*
     C     @@FTYP        WHENEQ    '$'                                          2
     C     @@UTYP        IFGE      '0'                                          2
     C     @@UTYP        ANDLE     '9'                                          2
     C     @@UTYP        ORGE      'A'                                          2
     C     @@UTYP        ANDLE     'M'                                          2
     C     @@UTYP        OREQ      '%'                                          2
     C     @@UTYP        OREQ      '#'                                          2
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@UTYP IFGE '0'
     C*
     C     @@FTYP        WHENEQ    '%'                                          3
     C     @@UTYP        IFGE      'A'                                          3
     C     @@UTYP        ANDLE     'Z'                                          3
     C     @@UTYP        OREQ      '&'                                          3
     C     @@UTYP        OREQ      '$'                                          3
     C     @@UTYP        OREQ      '#'                                          3
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@UTYP IFGE 'A'
     C*
     C     @@FTYP        WHENEQ    '#'                                          4
     C     @@UTYP        IFGE      'N'                                          4
     C     @@UTYP        ANDLE     '9'                                          4
     C     @@UTYP        OREQ      '$'                                          4
     C     @@UTYP        OREQ      '%'                                          4
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@UTYP IFGE 'N'
     C*
     C     @@UTYP        WHENEQ    '&'                                          5
     C     @@FTYP        IFGE      '0'                                          5
     C     @@FTYP        ANDLE     '9'                                          5
     C     @@FTYP        OREQ      '%'                                          5
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@FTYP IFGE '0'
     C*
     C     @@UTYP        WHENEQ    '$'                                          6
     C     @@FTYP        IFGE      '0'                                          6
     C     @@FTYP        ANDLE     '9'                                          6
     C     @@FTYP        ORGE      'A'                                          6
     C     @@FTYP        ANDLE     'M'                                          6
     C     @@FTYP        OREQ      '%'                                          6
     C     @@FTYP        OREQ      '#'                                          6
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@FTYP IFGE '0'
     C*
     C     @@UTYP        WHENEQ    '%'                                          7
     C     @@FTYP        IFGE      'A'                                          7
     C     @@FTYP        ANDLE     'Z'                                          7
     C     @@FTYP        OREQ      '&'                                          7
     C     @@FTYP        OREQ      '$'                                          7
     C     @@FTYP        OREQ      '#'                                          7
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@FTYP IFGE 'A'
     C*
     C     @@UTYP        WHENEQ    '#'                                          8
     C     @@FTYP        IFGE      'N'                                          8
     C     @@FTYP        ANDLE     '9'                                          8
     C     @@FTYP        OREQ      '$'                                          8
     C     @@FTYP        OREQ      '%'                                          8
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  @@FTYP IFGE 'N'
     C*
     C     @@FTYP        WHENNE    '*'                                          9
     C     @@FTYP        ANDNE     '&'                                          9
     C     @@FTYP        ANDNE     '%'                                          9
     C     @@FTYP        ANDNE     '#'                                          9
     C     @@FTYP        ANDNE     '$'                                          9
     C     @@UTYP        ANDNE     '*'                                          9
     C     @@UTYP        ANDNE     '&'                                          9
     C     @@UTYP        ANDNE     '%'                                          9
     C     @@UTYP        ANDNE     '#'                                          9
     C     @@UTYP        ANDNE     '$'                                          9
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDSL                                                  @@FTYP WHEQ
     C*
     C                   ENDIF                                                  @@UTYP IFNE @@FTYP
     C*
     C                   ENDIF                                                  @@FTYP IFNE *BLANKS
     C*
     C                   ENDIF                                                  @@ULVL LT @@FLVL
     C*
     C                   RETURN
