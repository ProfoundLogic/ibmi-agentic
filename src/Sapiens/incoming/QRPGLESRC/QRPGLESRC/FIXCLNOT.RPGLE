     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE DA700R - API TO WRITE A DARTS REMARK
     H*****************************************************************
      *  Description:                                                   *
      *
      *  This API will write a DARTS remark.
      *  It will not do any validation of the passed parameters.
      *  SDCMP/CMCOMM is 275 CHAR LONG.  The max length of a parm is
      *  256. If the comment is longer than 256, the additional 19
      *  bytes are passed in QQCOM2: i.e. QQCOMM and QQCOM2 will
      *  concatenate into CMCOMM.
      *  If QQFL01 is passed in a blanks and QQDIFL is passed in as 'Y'
      *  QQFL01 will be overwritten with the intended value from STCTP.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  WHATEVER YOU WANT WRITTEN TO DARTS.
      *
      *  SEND BACK       :  NEXT AVAILIBLE SEQUENCE# FOR A DARTS REMARK.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    NONE                                                         *
      *                                                                 *
      *  Created:   4/01/98  C:47  Project:  87MCB                      *
      *                                                                 *
      * *****************************************************************
      ** REVISIONS:                                                     *
      *                                                                 *
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     Fclnot1c   uf   E           K DISK    Rename(pnt01:pnt0p)
     Fclnot2c   uf   E           K DISK    Rename(pn201:pn20p)
     Fclnot3c   uf   E           K DISK    Rename(pn301:pn30p)
      *

     D not1            S              1    DIM(275)
     D not2            S              1    DIM(75)
     D not3            S              1    DIM(256)

     C* Fix CLNOT1C
     C                   Read      pnt0p
     C                   DoW       Not %Eof(clnot1c)
     C                   Clear                   not1
     C                   MOVEA     NTnote        not1
     C                   DO        275           X                 3 0
     C     not1(x)       IFEQ      X'0D'
     C     not1(x)       OREQ      X'0C'
     C     not1(x)       OREQ      X'25'
     C                   MOVE      X'40'         not1(x)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     not1          ntnote
     C                   update    pnt0p
     c                   read      pnt0p
     C                   Enddo

     C* Fix CLNOT2C
     C                   Read      pn20p
     C                   DoW       Not %Eof(clnot2c)
     C                   Clear                   not2
     C                   MOVEA     N2note        not2
     C                   DO        75            X                 3 0
     C     not2(x)       IFEQ      X'0D'
     C     not2(x)       OREQ      X'0C'
     C     not2(x)       OREQ      X'25'
     C                   MOVE      X'40'         not2(x)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     not2          n2note
     C                   update    pn20p
     c                   read      pn20p
     C                   Enddo

     C* Fix CLNOT3C
     C                   Read      pn30p
     C                   DoW       Not %Eof(clnot3c)
     C                   Clear                   not3
     C                   MOVEA     N3note        not3
     C                   DO        256           X                 3 0
     C     not3(x)       IFEQ      X'0D'
     C     not3(x)       OREQ      X'0C'
     C     not3(x)       OREQ      X'25'
     C                   MOVE      X'40'         not3(x)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     not3          n3note
     C                   update    pn30p
     c                   read      pn30p
     C                   Enddo

     c                   Seton                                        lr
