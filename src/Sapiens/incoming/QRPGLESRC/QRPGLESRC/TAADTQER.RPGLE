     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F* TAADTQER   - RETRIEVE DATA QUEUE DESCRP - CALLED BY TAADTQEC

     D DTQDS           DS
     D  LENRCV                 1      4B 0
     D  BYTAVL                 5      8B 0
     D  MAXLEN                 9     12B 0
     D  KEYLEN                13     16B 0
     D  SEQ                   17     17
     D  INCSND                18     18
     D  FORCE                 19     19
     D  TEXT                  20     69
     D BIN             DS
     D  DSLEN                  1      4B 0

     C* PARAMETER LIST
     C     *ENTRY        PLIST                                                  PARM LIST
     C                   PARM                    DTAQ             10            DATA QUEUE
     C                   PARM                    LIB              10            LIBRARY
     C                   PARM                    MAXVAL                         MAX LENGTH
     C                   PARM                    SEQVAL                         SEQ VALUE
     C                   PARM                    KEYVAL                         KEY LENGTH
     C                   PARM                    TXTVAL                         TEXT
     C                   PARM                    FRCVAL                         FORCE VALUE
     C                   PARM                    SNDVAL                         SENDER VALUE
     C* ASSEMBLE NAME AND INTIALIZE
     C                   MOVEL     DTAQ          QNAME            20            FULL DTAQ
     C                   MOVE      LIB           QNAME                          FULL DTAQ
     C                   Z-ADD     70            DSLEN                          DS LENGTH
     C* USE API TO ACCESS INFO
     C                   CALL      'QMHQRDQD'                                   API
     C                   PARM                    DTQDS                          DTAQ DS
     C                   PARM                    DSLEN                          DTAQ LEN
     C                   PARM      'RDQD0100'    FORMAT           10            FORMAT
     C                   PARM                    QNAME                          FULL NAME
     C* CONVERT SEQ VALUES TO WHAT IS ON THE COMMAND
     C     SEQ           IFEQ      'F'                                          IF F
     C                   MOVEL     '*FIFO '      SEQVAL            6            MOVE VALUE
     C                   END                                                    IF F
     C     SEQ           IFEQ      'L'                                          IF L
     C                   MOVEL     '*LIFO '      SEQVAL                         MOVE VALUE
     C                   END                                                    IF L
     C     SEQ           IFEQ      'K'                                          IF K
     C                   MOVEL     '*KEYED'      SEQVAL                         MOVE VALUE
     C                   END                                                    IF K
     C* CONVERT SENDERID VALUES TO WHAT IS ON THE COMMAND
     C     INCSND        IFEQ      'Y'                                          IF Y
     C                   MOVEL     '*YES'        SNDVAL            4            MOVE VALUE
     C                   END                                                    IF Y
     C     INCSND        IFEQ      'N'                                          IF N
     C                   MOVEL     '*NO '        SNDVAL                         MOVE VALUE
     C                   END                                                    IF N
     C* CONVERT FORCE VALUES TO WHAT IS ON THE COMMAND
     C     FORCE         IFEQ      'Y'                                          IF Y
     C                   MOVEL     '*YES'        FRCVAL            4            MOVE VALUE
     C                   END                                                    IF Y
     C     FORCE         IFEQ      'N'                                          IF N
     C                   MOVEL     '*NO '        FRCVAL                         MOVE VALUE
     C                   END                                                    IF N
     C* CONVERT TO DEC LEN(5 0) VALUES AND TEXT
     C                   Z-ADD     MAXLEN        MAXVAL            5 0          MAX LENGTH
     C                   Z-ADD     KEYLEN        KEYVAL            5 0          KEY VALUE
     C                   MOVE      TEXT          TXTVAL           50            TEXT VALUE
     C* END OF PROGRAM
     C     ENDPGM        TAG                                                    END OF PGM
     C                   SETON                                        LR        SET LR
     C                   RETURN                                                 RETURN
