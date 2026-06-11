     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE CVTRMCD2 - CONVERT REMARK CODE IN SMSLP, SMARP, SDCMP   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*  05/15/92 WLD   PROJECT: AJP                                  *
     H*                 INITIALIZE FIELDS ADDED FOR DARTS ENHANCEMENT *
     H*                                                               *
     H*  04/24/92 KRR   PROJECT: AJP                                  *
     H*                 CHANGE CMPNAM TO DIFFERENT VALUE DEPENDING    *
     H*                 ON VALUE OF CMPGM                             *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*****************************************************************
     F*
     FSTCTL     IF   E           K DISK
     FSMSLP     UF   E             DISK
     FSMARP     UF   E             DISK
     FSDCMP     UF   E             DISK
     F*

     C*
     C*
     C* REMARK TYPE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXTYPE            2
     C*
     C* SECURITY FILE CONVERSION
     C*
     C                   READ      PSL01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     SLCODE        XXTYPE
     C     CTKY          CHAIN     PCT01                              50
     C                   MOVE      *BLANKS       SLRMCD
     C     *IN50         IFEQ      '0'
     C     XXTYPE        ANDNE     *BLANKS
     C                   MOVEL     CTRMCD        SLRMCD
     C                   ELSE
     C                   MOVEL     'XXXXX'       SLRMCD
     C                   END
     C*
     C                   UPDATE    PSL01
     C*
     C                   READ      PSL01                                  60
     C*
     C                   END
     C*
     C* ADDITIONAL REMARKS FILE CONVERSION
     C*
     C                   READ      PAR01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     ARCODE        XXTYPE
     C     CTKY          CHAIN     PCT01                              50
     C                   MOVE      *BLANKS       ARRMCD
     C     *IN50         IFEQ      '0'
     C     XXTYPE        ANDNE     *BLANKS
     C                   MOVEL     CTRMCD        ARRMCD
     C                   ELSE
     C                   MOVEL     'XXXXX'       ARRMCD
     C                   END
     C*
     C                   Z-ADD     ARDT          ARIDT
     C*
     C*** BEGIN ADD *** 04/24/92 **************************************
     C********   ARPGM     IFEQ 'W4112R'
     C********             MOVEL'W0070R'  ARPNAM
     C********             END
     C********   ARPGM     IFEQ 'W4117R'
     C********   ARPGM     OREQ 'W4700R'
     C********   ARPGM     OREQ 'W7165R'
     C********   ARPGM     OREQ 'W7231R'
     C********   ARPGM     OREQ 'W7700R'
     C********   ARPGM     OREQ 'W8706R'
     C********             MOVEL'W0050R'  ARPNAM
     C********             END
     C*** END ADD *** 04/24/92 ****************************************
     C                   UPDATE    PAR01
     C*
     C                   READ      PAR01                                  60
     C*
     C                   END
     C*
     C* REMARK DETAIL FILE CONVERSION
     C*
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     CMCODE        XXTYPE
     C     CTKY          CHAIN     PCT01                              50
     C*** BEGIN ADD *** 05/15/92 **************************************
     C*                                                                *********
     C*** BEGIN CHG *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C                   MOVE      CTSMCD        CMSMCD
     C*** END   ADD *** 02/23/98 ***********************************************
     C*
     C                   MOVE      *BLANKS       CMFL01
     C                   MOVE      *BLANKS       CMFL02
     C                   MOVE      *BLANKS       CMFL03
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C*** END   ADD *** 05/15/92 **************************************
     C                   MOVE      *BLANKS       CMRMCD
     C     *IN50         IFEQ      '0'
     C     XXTYPE        ANDNE     *BLANKS
     C                   MOVEL     CTRMCD        CMRMCD
     C                   ELSE
     C                   MOVEL     'XXXXX'       CMRMCD
     C                   END
     C*
     C                   Z-ADD     CMDT          CMIDT
     C*
     C*** BEGIN ADD *** 04/24/92 **************************************
     C*******    CMPGM     IFEQ 'W4112R'
     C*******              MOVEL'W0070R'  CMPNAM
     C*******              END
     C*******    CMPGM     IFEQ 'W4117R'
     C*******    CMPGM     OREQ 'W4700R'
     C*******    CMPGM     OREQ 'W7165R'
     C*******    CMPGM     OREQ 'W7231R'
     C*******    CMPGM     OREQ 'W7700R'
     C*******    CMPGM     OREQ 'W8706R'
     C*******              MOVEL'W0050R'  CMPNAM
     C*******              END
     C*** END ADD *** 04/24/92 ****************************************
     C                   UPDATE    PCM01
     C*
     C                   READ      PCM01                                  60
     C*
     C                   END
     C*
     C*** END PROGRAM
     C                   MOVE      '1'           *INLR
