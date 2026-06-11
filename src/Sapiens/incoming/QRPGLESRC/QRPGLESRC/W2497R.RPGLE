     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2497R - CLIENT TABLE FILE SEARCH
     H*****************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM DISPLAYS THE CLIENT TABLE   *
     H*    KEY LIST FOR THE SELECTED TABLE.                           *
     H*                                                               *
     H* CALLED FROM:  W2496R                                          *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED:  07/24/98            PROJECT: 60ARN                  *
     H* Adapted from W2493R                                           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/28/02  C:134  PROJECT: WC577                               *
     H*           C:61 - Copied support changes from W2493R.          *
     H*                - UPDATED SO ONLY ACTIVE TABLE ENTRIES         *
     H*                  ARE DISPLAYED WHILE IN SEARCH MODE.          *
     H*                - CORRECTED PAGE UP.                           *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*                                                               *
     H*****************************************************************
      *
     FW2497F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
      *
     FWTCIL     IF   E           K DISK
     FWTVLL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *
      *  Constants
      *

     D XCKDSC          C                   CONST('Table ID')
      ***  BEGIN ADD  ***  06/28/02  *********************************
     D SF1PAG          C                   CONST(12)
      ***  END   ADD  ***  06/28/02  *********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *

     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CITBID        XXID
     C     *LIKE         DEFINE    VLKEY         XXKEY
      *
      *  CLIENT TABLE ID
      *
     C     IDKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXID
      *
      *  CLIENT TABLE
      *
     C     VLKYY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXID
     C                   KFLD                    XXKEY
      *
      *  COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     TOP           TAG
      *
     C                   TIME                    DSDTTM
     C***** BEGIN DEL **** 06/28/02 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 06/28/02 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
      ***** BEG ADD ******* 06/28/02 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 06/28/02 **********************************
      *
     C                   MOVEL     QQID          SCID
     C                   MOVEL     QQKEY         SCKEY
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
      *
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      *  INITIALIZE
      *
     C                   EXSR      INIT
      *
     C                   MOVE      *BLANKS       XXID
     C                   MOVEL     SCID          XXID
     C     IDKEY         CHAIN     PCI01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CIDESC        SCIDSC
     C***** BEGIN DEL **** 06/28/02 ***********************************
     C********             MOVELCIFN01    SCFNLB
     C********   SCFNLB    CAT  ':':0     SCFNLB
     C***** END   DEL **** 06/28/02 ***********************************
     C                   MOVEL     CIFN01        SCFN01
      *
     C                   ELSE
      *
     C                   MOVE      *ALL'*'       SCIDSC
     C***** BEGIN DEL **** 06/28/02 ***********************************
     C**********           MOVE *ALL'*'   SCFNLB
     C***** END   DEL **** 06/28/02 ***********************************
     C                   MOVE      *ALL'*'       SCFN01
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCKEY         XXKEY
     C     VLKYY         SETLL     PVL01                              60
      *
      ***        If no record found, see if there is a record previous
      ***        to the one requested.
      *
     C     *IN60         IFEQ      *ON
     C     IDKEY         READPE    PVL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      VLKEY         XXKEY
     C     VLKYY         SETLL     PVL01                              60
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      'F'           @PGD
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
      *
     C     STRT          TAG
      *
     C***** BEGIN DEL **** 06/28/02 ***********************************
     C********             MOVELQ2FILE    SCFILE           SET FORMAT/FILE
     C********   ' '       CHEKRQ2RECF    @P      50
     C********   10        SUB  @P        @P
     C********             MOVE *BLANKS   SCRECF
     C********   SCRECF    CAT  Q2RECF:@P SCRECF            FOR DISPLAY
     C***** END   DEL **** 06/28/02 ***********************************
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
      *
      *
      *
      *******************************************************************
      *   PROCESS ALL COMMAND KEYS
      *******************************************************************
      *
     C     *IN11         IFEQ      '1'
      *
      ****************************************
      *   F3 = EXIT                          *
      ****************************************
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
      ****************************************
      *   F12 = CANCEL                       *
      ****************************************
      *
     C     *INKL         IFEQ      '1'
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
      *
     C                   END                                                    END *INKL
      *
      ****************************************
      *   *IN10 -  PROCESS HELP REQUESTS     *
      ****************************************
      *
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
      *
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C***** BEGIN DEL **** 06/28/02 ***********************************
     C********             WRITESCR99
     C***** END   DEL **** 06/28/02 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
      *
      ****************************************
      *   *IN17 -  ROLLDOWN REQUEST          *
      ****************************************
      *
     C     *IN17         IFEQ      '1'                                          ROLLDOWN
      *
     C     KEYPF         IFEQ      *OFF                                         NO ROLL IF EOF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   MOVEL     KEYP          XXKEY
     C     VLKYY         SETLL     PVL01
     C                   MOVE      'B'           @PGD
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLDOWN
      *
      *
      ****************************************
      *   *IN18 -  ROLLUP REQUEST            *
      ****************************************
      *
     C     *IN18         IFEQ      '1'                                          ROLLUP
      *
     C     KEYNF         IFEQ      *OFF                                         NO ROLL IF EOF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   MOVEL     KEYN          XXKEY
     C     VLKYY         SETGT     PVL01
     C                   MOVE      'F'           @PGD
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
      *
     C                   GOTO      STRT
     C                   END                                                    *IN11
      *
      **************************************************************************
      *
      *   CHECK FOR USER SELECTION TO REPOSITION
      *
     C     SCPKEY        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVEL     SCPKEY        QQKEY
     C                   MOVE      *BLANKS       SCPKEY
     C                   GOTO      TOP
     C                   END                                                    END SCCODE NE BLKS
      *
      *  CHECK FOR SEARCH SELECTION
      *
     C***  BEGIN CHG  ***  06/28/02  *********************************
     C*********  1         DO   12        RRN2
     C     1             DO        SF1PAG        RRN2
     C***  END   CHG  ***  06/28/02  *********************************
     C     RRN2          CHAIN     SCR01S                             50
      *
     C     *IN50         IFEQ      '0'
      *
      *  IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
      *  SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
      *
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCKEY         QQKEY
     C                   LEAVE
      *
     C                   ELSE                                                   ELSE EQ '1'
      *
      *  IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
      *  A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
      *
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   MOVE      *BLANKS       SCSEL
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
      *
     C                   END                                                    END EQ '1'
      *
      *  IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
      *  WITHOUT A SELECTED RECORD
      *
     C                   END                                                    END 0 *IN50
      *
     C                   ENDDO
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *
      *****************************************************************
      * INIT SUBROUTINE                ** Subroutine **   I N I T     *
      *****************************************************************
      *
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C     *LIKE         DEFINE    *IN60         @LDF
     C                   MOVE      *OFF          @LDF
     C                   MOVE      *BLANK        @PGD              1
      *
     C     *LIKE         DEFINE    *IN60         KEYPF
     C     *LIKE         DEFINE    *IN60         KEYNF
     C                   MOVE      *OFF          KEYPF
     C                   MOVE      *OFF          KEYNF
      *
     C     *LIKE         DEFINE    SCKEY         KEYP
     C     *LIKE         DEFINE    SCKEY         KEYN
     C                   MOVE      *BLANK        KEYP
     C                   MOVE      *BLANK        KEYN
      *
     C                   ENDSR
      *
      *
      *****************************************************************
      * LOAD SUBROUTINE                ** Subroutine **   L O A D     *
      *****************************************************************
     C     LOAD          BEGSR
      *
     C                   MOVE      *ON           @LDF
      *
     C     @LDF          DOWEQ     *ON
     C                   MOVE      *OFF          @LDF
      *
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANK        KEYP
     C                   MOVE      *BLANK        KEYN
     C                   MOVE      *OFF          KEYPF
     C                   MOVE      *OFF          KEYNF
      *
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
      *
     C     @PGD          IFEQ      'F'
     C                   MOVE      *ZEROS        RRN2
      *
     C                   ELSE
      *
     C***  BEGIN CHG  ***  06/28/02  *********************************
     C**********           Z-ADD13        RRN2
     C     SF1PAG        ADD       1             RRN2
     C***  END   CHG  ***  06/28/02  *********************************
     C                   ENDIF
      *
      *  LOAD THE SUBFILE SCREEN WITH UPTO 12 LINES
      *
     C***  BEGIN CHG  ***  06/28/02  *********************************
     C***  BEGIN CHG  ***  06/28/02  *********************************
     C**********           DO   12
     C                   MOVE      00            CNT               2 0
     C     SF1PAG        DOWGT     CNT
     C***  END   CHG  ***  06/28/02  *********************************
     C***  END   CHG  ***  06/28/02  *********************************
      *
     C     @PGD          IFEQ      'F'
     C     IDKEY         READE     PVL01                                  60
      *
     C                   ELSE
      *
     C     IDKEY         READPE    PVL01                                  60
     C                   ENDIF
      *
     C     *IN60         IFEQ      '1'
     C                   LEAVE
     C                   END
      *
     C***  BEGIN ADD  ***  06/28/02  *********************************
     C     QQCALL        IFNE      'X'
     C     VLSTAT        ANDEQ     'A'
     C     QQCALL        OREQ      'X'
     C                   ADD       1             CNT
     C***  END   ADD  ***  06/28/02  *********************************
      *
     C     @PGD          IFEQ      'F'
     C                   ADD       1             RRN2
      *
     C                   ELSE
      *
     C                   SUB       1             RRN2
     C                   ENDIF
      *
     C                   MOVEL     VLKEY         SCKEY
     C                   MOVEL     VLDESC        SCKDSC
     C                   MOVEL     VLSTAT        SCSTAT
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
      *
     C     RRN2          IFEQ      1
     C                   MOVE      SCKEY         KEYP
     C                   Z-ADD     RRN2          #@$2
     C                   ENDIF
      *
     C***  BEGIN CHG  ***  06/28/02  *********************************
     C********** RRN2      IFEQ 12
     C     RRN2          IFEQ      SF1PAG
     C***  END   CHG  ***  06/28/02  *********************************
     C                   MOVE      SCKEY         KEYN
     C                   ENDIF
      *
      **** BEGIN ADD **** 06/28/02 *********************************************
     C                   ENDIF
      **** END   ADD **** 06/28/02 *********************************************
      *
     C                   ENDDO
      *
     C     @PGD          IFNE      'F'
     C     RRN2          ANDGT     1
     C                   MOVE      *BLANKS       XXKEY
     C     VLKYY         SETLL     PVL01
     C                   MOVE      *ON           @LDF
     C                   MOVE      'F'           @PGD
     C                   ENDIF
      *
     C                   ENDDO
      *
      *  Set Page Indicators
      *
     C     KEYP          IFGT      *BLANKS
     C                   MOVE      KEYP          XXKEY
     C     VLKYY         CHAIN     PVL01                              60
     C     IDKEY         READPE    PVL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           KEYPF
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     KEYN          IFGT      *BLANKS
     C                   MOVE      KEYN          XXKEY
     C     VLKYY         CHAIN     PVL01                              60
     C     IDKEY         READE     PVL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           KEYNF
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN05
     C                   ENDIF
      *
      *  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
      *
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANK        SCKEY
     C                   MOVEL     *BLANK        SCKDSC
     C                   MOVEL     *BLANK        SCSTAT
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
      *
     C                   MOVEL     *BLANK        SCKEY
      *
     C                   ENDSR
