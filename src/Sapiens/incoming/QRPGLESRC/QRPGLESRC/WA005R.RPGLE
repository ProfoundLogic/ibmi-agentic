     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0005R - COMPRESS EMP#/DIV/NAME
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL COMPRESS EMP#/DIV/NAME TO    *
     H*                ONE 80 CHARACTER FIELD.                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/20/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************

     D*
     D NM1             S              1    DIM(80)
     D NM2             S              1    DIM(80)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXEMP#            9 0
     C                   PARM                    XXDIV             5 0
     C                   PARM                    XXNAME           40
     C                   PARM                    XXEDN            80
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   CLEAR                   XXEDN
     C                   CLEAR                   NM1
     C                   CLEAR                   NM2
     C                   CLEAR                   XXNUM
     C                   Z-ADD     1             Y                 3 0
     C*
     C                   MOVEL     XXEMP#        XXNUM            80
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C     XXZERO        DO        9             X                 3 0
     C                   MOVEA     NM1(X)        NM2(Y)
     C                   ADD       1             Y
     C                   ENDDO                                                  XXZERO->X->7
     C*
     C                   CLEAR                   NM1
     C                   CLEAR                   XXNUM
     C                   ADD       2             Y
     C*
     C                   MOVEL     XXDIV         XXNUM            80
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C     XXZERO        DO        5             X
     C                   MOVEA     NM1(X)        NM2(Y)
     C                   ADD       1             Y
     C                   ENDDO                                                  XXZERO->X->7
     C*
     C     XXDIV         IFNE      *ZERO
     C                   ADD       2             Y
     C                   ENDIF                                                  XXDIV<>*ZERO
     C*
     C                   MOVEA     XXNAME        NM2(Y)
     C*
     C                   MOVEA     NM2           XXEDN
     C*
     C                   RETURN
     C*
