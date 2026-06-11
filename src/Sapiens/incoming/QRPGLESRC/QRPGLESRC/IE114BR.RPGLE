     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IE114BR - Assign Queue Logic Program
     H*****************************************************************
     H* Description: This program will determine if the assign queue  *
     H*              program should be called                         *
     H*                                                               *
     H* Called from: IS215R1  - Index a queue entry                   *
     H*              IS210R   - Document Image Index entry            *
     H* Calls to   : IS115R   - Reassign - selection screen           *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/12/2004 by c:62 for Project 114MEX              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FIMIFL2    IF   E           K DISK
     FIMDOL1    IF   E           K DISK
     FIMDTL     IF   E           K DISK

     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D FFRM            C                   CONST('Fax From:')
     D*
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*===============================================================
     C* Parameter Lists
     C*===============================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@QID            10            10A Q ID
     C                   PARM                    @@USER           10            10A USER
     C                   PARM                    @@SDT             8 0           80 SCNDTMDY
     C                   PARM                    @@STM             6 0           60 SCTIM
     C                   PARM                    @@DCID           12            12A DCID
     C                   PARM                    @@DESC           30            30A DC DSC
     C                   PARM                    @@DOCT            8             8A DOCTYP
     C                   PARM                    @@FL01            1             1A IMG/FAX
     C                   PARM                    @@PTH            63            63A FULL PTH
     C                   PARM                    @@FLR             8             8A FOLD
     C                   PARM                    @@SFLR           12            12A SUBFLR
     C                   PARM                    @@RTCD            1             1A RTNCODE
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*===============================================================
     C* Key Lists
     C*===============================================================
     C     IFKY          KLIST
     C                   KFLD                    @@DCID
     C*
     C     DOKY          KLIST
     C                   KFLD                    @@DCID
     C*
     C     DTKY          KLIST
     C                   KFLD                    @@DOCT
     C*===============================================================
     C* Like Defines
     C*===============================================================
     C*
     C     @@RTCD        IFEQ      *BLANKS
     C                   EXSR      ASSQUE
     C                   ENDIF
     C*
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C* ASSQUE - Assign Queue
     C*****************************************************************
     C     ASSQUE        BEGSR
     C*
     C                   Z-ADD     @@SDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         $$SDT
     C                   MOVE      @@STM         $$STM
     C*
     C     DOKY          CHAIN     PDO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DOFLR         XXFLR             8
     C                   MOVEL     DOSFLR        XXSFLR           12
     C     XXFLR         CAT       '/'           XX9A              9
     C     XX9A          CAT(P)    XXSFLR        $$PTH
     C                   ELSE
     C                   MOVEL     *BLANKS       $$PTH
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       $$DSC
     C     @@FL01        IFEQ      'F'
     C     IFKY          CHAIN     PIF01                              63
     C     *IN63         IFEQ      *OFF
     C     FFRM          CAT(P)    IFTSI:1       $$DSC
     C                   ELSE
     C                   MOVEL     '*N/A*'       $$DSC
     C                   ENDIF
     C                   ELSE
     C     DTKY          CHAIN     PDT01                              63
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     DTDESC        $$DSC
     C                   ELSE
     C                   MOVEL     '*N/A*'       $$DSC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CALL      'IS115R'
     C                   PARM                    $$SDT             8
     C                   PARM                    $$STM             6
     C                   PARM      @@DOCT        $$DOCT            8
     C                   PARM      @@DCID        $$DCID           12
     C                   PARM      @@FL01        $$FL01            1
     C                   PARM                    $$FLR             8
     C                   PARM                    $$SFLR           12
     C                   PARM                    $$PTH            63
     C                   PARM      @@QID         $$QID            10
     C                   PARM                    $$DSC            30
     C                   PARM      *BLANKS       $$RTC1            1
     C                   PARM      *BLANKS       $$QOD            10
     C*
     C     $$RTC1        IFNE      'A'
     C     $$RTC1        ANDNE     'B'
     C     $$QOD         IFNE      *BLANKS
     C                   MOVEL     $$QOD         @@QID
     C                   ENDIF
     C                   MOVEL(P)  'R'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDSR
