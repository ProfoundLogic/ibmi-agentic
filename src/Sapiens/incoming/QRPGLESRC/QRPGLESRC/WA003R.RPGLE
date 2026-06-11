     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA003R - CHANGE NUMERIC FIELDS TO ALPHA
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CHANGE NUMERIC FIELDS TO     *
     H*                ALPHA WITH ZERO SUPPRESSION AND EDIT MASKS.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/29/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/14/98  C:33  SEQ#: WA091/11509                            *
     H*                  ACCOUNT FOR NEGATIVE NUMBERS.                *
     H*                                                               *
     H*  02/13/98  C:33  PROJECT: WC577                               *
     H*                  'XXX,XXX' CALCULATES FOR TOO MANY SPACES WHEN*
     H*                  LENGTH IS DIVISIBLE BY 3.                    *
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
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   CLEAR                   NM1
     C*
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C     ' '           CHECKR    XXNUM         XXLEN             3 0
     C     XXZERO        SUB       1             XXBLNK            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C                   DO        XXBLNK        X                 3 0
     C                   MOVE      ' '           NM1(X)
     C                   ENDDO                                                  X->XXBLNK
     C*
     C                   CLEAR                   NM2
     C*
     C                   MOVE      *OFF          NEG
     C                   Z-ADD     XXLEN         X
     C*
     C     XXLEN         IFNE      *ZERO
     C     NM1(X)        ANDNE     *BLANK
     C*
     C     NM1(X)        IFLT      '0'
     C     NM1(X)        ORGT      '9'
     C*
     C***  BEGIN CHG  ***  05/14/98  *********************************
     C**********           SUB  1         XXLEN
     C                   SELECT
     C     NM1(X)        WHENEQ    '}'
     C                   MOVEA     '0'           NM1(X)
     C     NM1(X)        WHENEQ    'J'
     C                   MOVEA     '1'           NM1(X)
     C     NM1(X)        WHENEQ    'K'
     C                   MOVEA     '2'           NM1(X)
     C     NM1(X)        WHENEQ    'L'
     C                   MOVEA     '3'           NM1(X)
     C     NM1(X)        WHENEQ    'M'
     C                   MOVEA     '4'           NM1(X)
     C     NM1(X)        WHENEQ    'N'
     C                   MOVEA     '5'           NM1(X)
     C     NM1(X)        WHENEQ    'O'
     C                   MOVEA     '6'           NM1(X)
     C     NM1(X)        WHENEQ    'P'
     C                   MOVEA     '7'           NM1(X)
     C     NM1(X)        WHENEQ    'Q'
     C                   MOVEA     '8'           NM1(X)
     C     NM1(X)        WHENEQ    'R'
     C                   MOVEA     '9'           NM1(X)
     C                   ENDSL
     C***  END   CHG  ***  05/14/98  *********************************
     C*
     C                   MOVE      *ON           NEG               1
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXLEN<>*ZERO
     C*
     C**
     C*
     C                   SELECT
     C*
     C     EDTMSK        WHENEQ    'XXX,XXX'
     C*
     C                   Z-ADD     XXLEN         W                 3 0
     C     XXLEN         DIV       3             Y                 3 0
     C***  BEGIN ADD  ***  02/13/98  *********************************
     C                   MVR                     XXREM             3 0
     C*
     C     XXREM         IFEQ      *ZERO
     C                   SUB       1             Y
     C                   ENDIF                                                  XXREM=*ZERO
     C***  END   ADD  ***  02/13/98  *********************************
     C*
     C                   ADD       Y             XXLEN
     C                   Z-ADD     XXLEN         Z                 3 0
     C*
     C                   DO        XXLEN         X
     C*
     C     NM1(W)        IFNE      *BLANK
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C                   MOVEA     ','           NM2(Z)
     C                   ELSE
     C                   MOVE      NM1(W)        NM2(Z)
     C                   SUB       1             W
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   SUB       1             Z
     C*
     C                   ENDDO                                                  X->XXLEN
     C*
     C**
     C*
     C     EDTMSK        WHENEQ    'X,XXX.XX'
     C*
     C                   Z-ADD     XXLEN         W
     C     XXLEN         DIV       3             Y
     C                   ADD       Y             XXLEN
     C                   ADD       1             XXLEN
     C                   Z-ADD     XXLEN         Z
     C*
     C                   DO        XXLEN         X
     C*
     C     X             IFEQ      1
     C*
     C                   MOVEL     *BLANK        NM2(Z)
     C*
     C                   ELSE
     C*
     C     NM1(W)        IFNE      *BLANK
     C     X             ORLE      4
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C*
     C     X             IFEQ      4
     C                   MOVEA     '.'           NM2(Z)
     C                   ELSE
     C                   MOVEA     ','           NM2(Z)
     C                   ENDIF                                                  Z=4
     C*
     C                   ELSE
     C*
     C     NM1(W)        IFEQ      *BLANK
     C                   MOVE      '0'           NM2(Z)
     C                   ELSE
     C                   MOVE      NM1(W)        NM2(Z)
     C                   ENDIF                                                  NM1,W=*BLANK
     C*
     C                   SUB       1             W
     C*
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   ENDIF                                                  X=1
     C*
     C                   SUB       1             Z
     C*
     C                   ENDDO                                                  X->XXLEN
     C*
     C**
     C*
     C     EDTMSK        WHENEQ    'DATE'
     C*
     C                   Z-ADD     1             W
     C*
     C     NM1(8)        IFNE      *BLANK
     C*
     C                   DO        10            X
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           NM2(X)
     C                   ELSE
     C                   MOVEL     NM1(W)        NM2(X)
     C                   ADD       1             W
     C                   ENDIF                                                  X=3/6
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   ENDIF                                                  NM1,10<>*BLANK
     C*
     C**
     C*
     C     EDTMSK        WHENEQ    'RPMY'
     C*
     C                   Z-ADD     1             W
     C*
     C     NM1(6)        IFNE      *BLANK
     C*
     C                   DO        7             X
     C*
     C     X             IFEQ      3
     C                   MOVEL     '/'           NM2(X)
     C                   ELSE
     C                   MOVEL     NM1(W)        NM2(X)
     C                   ADD       1             W
     C                   ENDIF                                                  X=3/6
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   ENDIF                                                  NM1,10<>*BLANK
     C*
     C**
     C*
     C                   OTHER
     C*
     C                   MOVEA     NM1           NM2
     C*
     C                   ENDSL
     C*
     C**
     C*
     C     NEG           IFEQ      *ON
     C***  BEGIN CHG  ***  05/14/98  *********************************
     C********** XXLEN     ADD  1         X
     C                   Z-ADD     XXLEN         X
     C***  END   CHG  ***  05/14/98  *********************************
     C                   MOVEA     '-'           NM2(X)
     C                   ENDIF                                                  NEG=*ON
     C*
     C                   MOVEL     *BLANKS       XXNUM
     C                   MOVEA     NM2           XXNUM
     C*
     C                   RETURN
     C*
