     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H* REVISIONS:                                                    *
     H*                                                              *
     H*  09/27/06  C:33   Seq#: WA129/54958                          *
     H*                   Remove prior printed records.              *
     H*                                                              *
     H*  06/29/06  C:168  PROJECT: 136WBG                            *
     H*                   INITIAL PROGRAMMING                        *
     H****************************************************************
     FWXAGP     UF   E             DISK    USROPN
     F                                     RENAME(PAG01:DBRCDF)
     FWXAGL1    UF   E           K DISK    USROPN
      *
      *

     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Move to Outq')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@AGT#            5 0
     C                   PARM                    @@USER           10
     C                   PARM                    @@OUTQ           10
     C                   PARM                    @@ERR             1
     C                   PARM                    @@REC             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     AGKY          PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXAGT#
     C*
     C     AGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHDR
     C                   KFLD                    XXAGT#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    AGCO#         XXCO#
     C     *LIKE         DEFINE    AGHDR         XXHDR
     C     *LIKE         DEFINE    AGAGT#        XXAGT#
      *
     C                   EXSR      PGMSTR
     C*
     C                   CLEAR                   XXHDR
     C*
     C***  Begin Add  ***  09/27/06  **************************************
     C                   EXSR      RMVPRT
     C***  End   Add  ***  09/27/06  **************************************
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'M'
     C                   EXSR      CHGQUE
     C     @@MODE        WHENEQ    'P'
     C                   EXSR      MVEQUE
     C     @@MODE        WHENEQ    'B'
     C                   EXSR      CHGQUE
     C                   EXSR      MVEQUE
     C                   ENDSL
      *
     C                   EXSR      PGMEND
      *
     C*================================================================
     C* CHGHDR - SUBROUTINE FOR UPDATING THE QUE IN HEADER RECORD
     C*================================================================
     C     CHGHDR        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'M'
     C                   MOVEL     @@OUTQ        AGOUTQ
     C                   MOVE      'M'           AGSTAT
     C     @@MODE        WHENEQ    'P'
     C                   MOVE      'P'           AGSTAT
     C     @@MODE        WHENEQ    'B'
     C                   MOVEL     @@OUTQ        AGOUTQ
     C                   MOVE      'P'           AGSTAT
     C                   ENDSL
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDSR                                                  CHGHDR
      *
     C*================================================================
     C* CHGQUE - SUBROUTINE FOR UPDATING THE QUE IN FILE
     C*================================================================
     C     CHGQUE        BEGSR
     C*
     C     AGKY1         CHAIN     PAG01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AGRUN         IFEQ      'A'
     C     AGSTAT        ANDEQ     *BLANK
     C                   MOVEL     'M'           AGSTAT
     C                   MOVEL     @@OUTQ        AGOUTQ
     C                   UPDATE    PAG01
     C                   ENDIF
      *
     C     AGKY1         READE     PAG01                                  60
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  CHGQUE
      *
     C*================================================================
     C* MVEQUE - SUBROUTINE FOR MOVING THE SPOOL FILE TO NEW QUE
     C*================================================================
     C     MVEQUE        BEGSR
      *
     C                   Z-ADD     *ZEROS        XXTOT             9 0
     C*
     C     AGKY1         CHAIN     PAG01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AGRUN         IFEQ      'A'
     C     AGSTAT        ANDEQ     'M'
     C                   EXSR      MVESPL
     C     QQERR         IFEQ      *BLANKS
     C                   MOVEL     'P'           AGSTAT
     C                   UPDATE    PAG01
     C                   ADD       1             XXTOT
     C                   ENDIF
     C                   ENDIF
      *
     C     AGKY1         READE     PAG01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      XXTOT         @@REC
      *
     C                   ENDSR                                                  CHGSTS
     C*================================================================
     C* MVESPL - SUBROUTINE FOR MOVING THE SPOOL FILE TO NEW QUE
     C*================================================================
     C     MVESPL        BEGSR
      *
      * overrides the spool file to the new outque
      *
     C                   MOVEL     AGJOB#        QQJOB#
     C                   MOVEL     AGSPL#        QQSPL#
     C                   MOVEL     AGSPCY        QQCOPY
      *
     C                   CALL      'W7174C'
     C                   PARM      AGSPLN        QQSPLF           10
     C                   PARM                    QQJOB#            6
     C                   PARM      AGJOBU        QQJOBU           10
     C                   PARM      AGJOBN        QQJOBN           10
     C                   PARM                    QQSPL#            4
     C                   PARM      AGOUTQ        QQOUTQ           10
     C                   PARM                    QQCOPY            4
     C                   PARM      AGSPFR        QQFORM           10
     C                   PARM      *BLANKS       QQERR             1
      *
     C                   ENDSR                                                  CHGSTS
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
     C                   CLOSE     WXAGP
     C                   CLOSE     WXAGL1
     C                   SETON                                        LR
      *
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
     C*                    CALL 'W7172C'
     C*                    PARM @@USER    QQUSER 10
     C*
     C                   OPEN      WXAGP
     C                   OPEN      WXAGL1
     C*
     C                   CLEAR                   AGCO#
     C                   CLEAR                   AGAGT#
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   Z-ADD     AGCO#         XXCO#
     C                   Z-ADD     AGAGT#        XXAGT#
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C                   EXSR      CHGHDR
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C***  Begin Add  ***  09/27/06  **************************************
     C*================================================================
     C* RMVPRT - Remove all prior printed records
     C*================================================================
     C     RMVPRT        BEGSR
     C*
     C     AGKY1         CHAIN     PAG01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AGRUN         IFNE      'A'
     C     AGSTAT        OREQ      'P'
     C     AGJOBN        OREQ      'W7168C'
     C                   DELETE    PAG01
     C                   ENDIF
     C*
     C     AGKY1         READE     PAG01                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C***  End   Add  ***  09/27/06  **************************************
