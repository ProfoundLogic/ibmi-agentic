     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      * FIX 73168 - USE TO BRING WMXAP (DELETED AR) BACK TO
      *             WMEAP WHEN REINSTATEMENT MISSES IT
      *
      * HOW TO USE:
      *             1) SET UP USER LIBRARY (USERLIB)
      *             2) CREATE WMXAP/L* INTO IT
      *             3) COPY RECORDS YOU WANT TO RESTORE TO WMEAP
      *                FROM WMXAP INTO USERLIB/WMXAP
      *             4) ADD USERLIB TO TOP OF LIBRARY LIST
      *             5) CALL PGM - IT RUNS THROUGH USERLIB/WMXAP,
      *                COPIES EACH RECORD BACK TO WMEAP
      *             6) MANUALLY DELETE THE PRODUCTIONLIB/WMXAP
      *                RECORDS THAT WERE COPIED TO USERLIB/WMXAP
      *
      **************************************************************
      *  REVISIONS
      *
      *  08/21/10  C:61  SUPPORT SEQ73949
      *                  ADD FIELDS FOR V9.
      *
      **************************************************************
     FWMXAL     UF   E           K DISK
     FWMEAL14   O    E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C     *LIKE         DEFINE    EAPODT        SYSDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   TIME                    DSDTTM
     C                   READ      PXA01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      WRTREC
     C                   DELETE    PXA01
     C                   READ      PXA01                                  60
     C                   ENDDO
     C                   SETON                                        LR
     C*================================================================
     C*  WRTREC - writes record to a/r file exactly as they were when
     C*           they were deleted except for the maintenance fields
     C*           and the transaction date field
     C*================================================================
     C     WRTREC        BEGSR
     C*
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
      ******* BEG ADD **** 08/21/10 ****************************
     C                   MOVE      XACTYP        EACTYP
     C                   MOVE      XARDIV        EARDIV
      ******* END ADD **** 08/21/10 ****************************
     C                   Z-ADD     SYSDT         EATRDT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   MOVE      DSTIME        EATIME
     C                   Z-ADD     SYSDT         EADT
     C                   WRITE     PEA01
     C                   ENDSR                                                  WRTREC
