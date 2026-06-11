     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE ISFAXPR - GET THE DOCID TO BE USED FOR THE PRINT FAX OR
     H*                THE FAX A FAX.
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTION         *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  FILE USGE:                                                  *
     H*****************************************************************
     F*
     FSDCMLA    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*
     C     CMKY          KLIST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXSEQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    DCID             12
     C                   PARM                    FOLD             80
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ             5
     C                   PARM                    OPTFLG            1
     C                   PARM                    RCODE             1
     C*
     C*
     C                   MOVE      QQSEQ         XXSEQ#            5 0
     C                   MOVE      QQKEY         XXKEY            40
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CMDCID        DCID
     C                   MOVEL     *BLANKS       RCODE
     C                   ELSE
     C                   MOVEL     '1'           RCODE
     C                   ENDIF
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
