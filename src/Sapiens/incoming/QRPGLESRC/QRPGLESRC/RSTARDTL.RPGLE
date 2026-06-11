     FWMXAP     UF   E           K DISK
     FWMEAP     O    E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C                   TIME                    DSDTTM
     C*
     C*      Read cancellation a/r detail file
     C*
     C                   READ      PXA01                                  99
     C                   DoW       *in99=*OFF
     C*
     C                   If        XAiDt>=1130501
     C                   Clear                   PEA01
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XAFND         EAFND
     C                   MOVE      XAFYR         EAFYR
     C                   MOVE      XAEMP#        EAEMP#
     C                   MOVE      XADIV         EADIV
     C                   MOVE      XAARCD        EAARCD
     C                   MOVE      XAARTY        EAARTY
     C                   MOVE      XADESC        EADESC
     C                   MOVE      XAAMT         EAAMT
     C                   MOVE      XAPODT        EAPODT
     C                   MOVE      XAEDIT        EAEDIT
     C                   MOVE      XASTAT        EASTAT
     C                   MOVE      XABUL1        EABUL1
     C                   MOVE      XACHK#        EACHK#
     C                   MOVE      XANINV        EANINV
     C                   MOVE      XALATE        EALATE
     C                   MOVE      XAFL01        EAFL01
     C                   MOVE      XAFL02        EAFL02
     C                   MOVE      XADT01        EADT01
     C                   MOVE      XADT02        EADT02
     C                   MOVE      XACTYP        EACTYP
     C                   MOVE      XARDIV        EARDIV
     C                   MOVE      XACCTP        EACCTP
     C                   MOVE      XACNUM        EACNUM
     C                   MOVE      XACEXP        EACEXP
     C                   MOVE      XACNAM        EACNAM
     C                   MOVE      XATRDT        EATRDT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   MOVE      DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Write     PEA01
     C                   Delete    PXA01
     C*
     C                   Else
     C                   Update    PXA01
     C                   EndIf
     C*
     C                   Read      PXA01                                  99
     C                   EndDo                                                  *IN60=*OFF
     C*
     C                   SetOn                                        lr
