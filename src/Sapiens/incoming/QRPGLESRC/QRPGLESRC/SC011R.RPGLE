     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H/TITLE SC011R - Generic Front End for DARTS prompt              *
     H*****************************************************************
     H* Description: this program prompts the user to select a key    *
     H*   type.  Based on the key type selected, a program is called  *
     H*   (value is in file SMKTP in field KTSRC5)                    *
     H*                                                               *
     H* Called from: Menu                                             *
     H* Call to:     ISGDxxR prompt program where xx is determined    *
     H*              by the key type selected.                        *
     H*                                                               *
     F* Created: 02/01/2002  C:87  Project: DR173                     *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* xx/xx/xx  c:xxx  Project: xxxxx                               *
     H*    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *
     H*                                                               *
     H*****************************************************************
     F* Key type files
     FSMKTP     IF   E             DISK
     F*

     C****************************************************************
     C*  ENTRY PARAMETERS
     C****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*
     C                   MOVE      *BLANKS       PRPGM            10            prompt program
     C                   MOVE      *OFF          ##KL              1
     C*
     C****************************************************************
     C*  Start processing
     C****************************************************************
     C     *INKL         DOUEQ     *OFF
     C                   SETOFF                                         KL
     C*
     C*     Call SC092R to display key type selection screen
     C                   MOVE      '2'           @@CALL
     C                   MOVE      ' '           @@KC
     C                   MOVE      ' '           @@KL
     C                   MOVE      *ZERO         @@RRN
     C*
     C                   CALL      'SC092R'
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C     ##KL          PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN     SMKTP                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  KTKTYP        QQID              2
     C                   MOVEL(P)  KTSRC5        PRPGM            10
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  @@RRN<>*ZERO
     C*
     C*   Call the prompt program as long as a key type was selected
     C     ##KL          IFNE      *ON
     C     QQID          ANDNE     *BLANK
     C                   CALL      PRPGM
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQPRDS
     C                   PARM                    QQID
     C                   ENDIF                                                  *INKL<>*ON
     C*
     C                   ENDDO                                                  *INKL=*OFF
     C*
     C*   end the program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
