     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - WA1                                                       WCMC
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA107R                                            *
     H*    CREATED: 1         C:45                                    *
     H*    DESCRIPTION: GET CLAIM MASTER INFORMATION FOR WRITE/UPDATE *
     H*                 OF WCMC                                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 11/05/97 C:15 PROJECT: WC577                                  *
     H*               Changed the length of the entry parm for date   *
     H*               reported from 6A to 7A (for V3 compliance).     *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FWMCML1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMDPL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  NATURE CODE TABLE FILE
     C*
     C     TBLKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXKEY            10
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C     DPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCTG1
     C*
     C     *ENTRY        PLIST
      * INPUT FROM CALLER
     C                   PARM                    QQCO#                           I
     C                   PARM                    QQFND                           I
     C                   PARM                    QQFYR                           I
     C                   PARM                    QQCASE                          I
     C                   PARM                    QQEMP#                          I
     C                   PARM                    QQDIV                           I
      * RETURN TO CALLER
     C                   PARM                    QQNA                            O
     C                   PARM                    QQNADS                          O
     C                   PARM                    QQPA                            O
     C                   PARM                    QQPADS                          O
     C                   PARM                    QQCA                            O
     C                   PARM                    QQCADS                          O
     C                   PARM                    QQRTCD            1             I/O
     C                   PARM                    QQCTG1                          O
     C                   PARM                    QQCTGD           40             O
     C*** BEGIN CHG *** 11/05/97 ***********************************************
     C******************** PARM           QQDTRP  6         O
     C                   PARM                    QQDTRP            7             O
     C*** END CHG ***** 11/05/97 ***********************************************
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #@CO#             3
     C                   PARM                    #@FND             3
     C                   PARM                    #@FYR             3
     C                   PARM                    #@CASE            7
     C                   PARM      Q1USER        #@USR            10
     C                   PARM                    #@RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
      *
     C     *LIKE         DEFINE    CMCO#         QQCO#
     C     *LIKE         DEFINE    CMFND         QQFND
     C     *LIKE         DEFINE    CMFYR         QQFYR
     C     *LIKE         DEFINE    CMCASE        QQCASE
     C     *LIKE         DEFINE    DPEMP#        QQEMP#
     C     *LIKE         DEFINE    DPDIV         QQDIV
      *
     C     *LIKE         DEFINE    CMNATR        QQNA
     C     *LIKE         DEFINE    NADESC        QQNADS
     C     *LIKE         DEFINE    CMPART        QQPA
     C     *LIKE         DEFINE    PADESC        QQPADS
     C     *LIKE         DEFINE    CMCAUS        QQCA
     C     *LIKE         DEFINE    CADESC        QQCADS
     C     *LIKE         DEFINE    CMCTG1        QQCTG1
      * PROGRAM INITIALIZATION
     C                   EXSR      INIT
      * MAINLINE
     C                   DO
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         #@CO#
     C                   MOVE      CMFND         #@FND
     C                   MOVE      CMFYR         #@FYR
     C                   MOVE      CMCASE        #@CASE
     C                   MOVE      *BLANKS       #@RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     #@RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      'X'           QQRTCD                         X=NO HIT
     C                   ELSE                                                   RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C                   MOVE      ' '           QQRTCD                         BLK=HIT
     C                   MOVE      CMDTRP        QQDTRP
     C     CMCTG1        IFNE      *BLANK
     C                   EXSR      EMPLYR
     C                   ENDIF
     C     CMNATR        IFNE      *BLANK
     C                   EXSR      NATURE
     C                   ENDIF
     C     CMPART        IFNE      *BLANK
     C                   EXSR      PART
     C                   ENDIF
     C     CMCAUS        IFNE      *BLANK
     C                   EXSR      CAUSE
     C                   ENDIF
     C*** BEG ADD ****** 05/25/07 **********************************************
     C                   ENDIF                                                  RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C                   ELSE
     C                   MOVE      'X'           QQRTCD                         X=NO HIT
     C                   ENDIF                                                  PCM01=*IN60=0
     C                   ENDDO
      * PROGRAM TERMINATION
     C                   MOVE      '1'           *INLR
      /EJECT
     C     NATURE        BEGSR
     C                   MOVEL(P)  CMNATR        XXKEY
     C     TBLKY         CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CMNATR        QQNA
     C                   MOVE      NADESC        QQNADS
     C                   ELSE
     C                   CLEAR                   QQNADS
     C                   ENDIF                                                  PNA01=*IN60=0
      *
     C                   ENDSR
      /EJECT
     C     PART          BEGSR
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBLKY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CMPART        QQPA
     C                   MOVE      PADESC        QQPADS
     C                   ELSE
     C                   CLEAR                   QQPADS
     C                   ENDIF                                                  PNA01=*IN60=0
     C                   ENDSR
      /EJECT
     C     CAUSE         BEGSR
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBLKY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CMCAUS        QQCA
     C                   MOVE      CADESC        QQCADS
     C                   ELSE
     C                   CLEAR                   QQCADS
     C                   ENDIF                                                  PNA01=*IN60=0
     C                   ENDSR
      /EJECT
     C     EMPLYR        BEGSR
     C                   MOVEL     CMCTG1        QQCTG1
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DPDESC        QQCTGD
     C                   ELSE
     C                   CLEAR                   QQCTGD
     C                   ENDIF                                                              0
     C                   ENDSR
      /EJECT
     C     INIT          BEGSR
     C                   CLEAR                   QQNA
     C                   CLEAR                   QQNADS
     C                   CLEAR                   QQPA
     C                   CLEAR                   QQPADS
     C                   CLEAR                   QQCA
     C                   CLEAR                   QQCADS
     C                   CLEAR                   PCM01
     C                   CLEAR                   PNA01
     C                   CLEAR                   PPA01
     C                   CLEAR                   PCA01
     C                   ENDSR
