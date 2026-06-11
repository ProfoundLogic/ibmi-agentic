     D* CHANGE THE CURRENT USER PROFILE
     D* USING HARD CODED USERNAME
     D*****************************************************************
     D NEWUSER         S             10A
     D NEWPASSWORD     S             10A
     D NEWHANDLE       S             12A
     D QUSEC           DS
     D   QUSBPRV               1      4B 0
     D   QUSBAVL               5      8B 0
     D   QUSEI                 9     15
     D   QUSERVED             16     16
     D   OUTERROR             17   1040

     C                   EVAL      NEWUSER     = 'PORTAL'
     C                   EVAL      NEWPASSWORD = '*NOPWD    '
     C                   EVAL      QUSBPRV = 1040

     C                   CALL      'QSYGETPH'
     C                   PARM                    NEWUSER
     C                   PARM                    NEWPASSWORD
     C                   PARM                    NEWHANDLE
     C                   PARM                    QUSEC

     C                   IF        QUSBAVL > 0
     C                   Else
     C                   CALL      'QWTSETP'
     C                   PARM                    NEWHANDLE
     C                   PARM                    QUSEC
     C                   EndIf

     C                   Seton                                        LR
