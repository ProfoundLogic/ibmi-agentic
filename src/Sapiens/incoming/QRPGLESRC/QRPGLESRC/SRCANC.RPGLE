     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRCANC - CHECK CANCELLATION DATE FOR ALL DIVISIONS
     H*****************************************************************
     H*    PROGRAM DESCRIPTION:                                       *
     H*                                                               *
     H*       THIS PROGRAM CHECKS THE CANCELLATION DATE FOR ALL       *
     H*       DIVISIONS.                                              *
     H*                                                               *
     H*       RETURN CODE:      'Y' ALL DIVISIONS HAVE THE SAME CX    *
     H*                             DATE.                             *
     H*                         'N' DIVISIONS DO NOT HAVE THE SAME    *
     H*                             CX DATE.                          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7720R - GENERATE FINAL AUDITED PREMIUM AMOUNT              *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: LARRY KORZON                                      *
     H* CREATED:    01/06/93            PROJECT: ASJ                  *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/16/00 C:65  PROJECT: WC936                                 *
     H*                ADD DATA STRUCTURE FOR CANCEL DATES.           *
     H*                                                               *
     H* 11/15/00 C:65  PROJECT: WC936                                 *
     H*                THIS PROGRAM IS NOT WORKING CORRECTLY ANYMORE  *
     H*                DUE TO CHANGES IN SETTING ELPTDT AT AUDIT TIME.*
     H*                CHECK THE CX DATE IN EMPLOYER MASTER INSTEAD.  *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     F*** BEGIN ADD ***  11/15/99  ************************************
     FWMEML     IF   E           K DISK

     D*
     D*** END ADD *****  11/15/99  ************************************
     D*** BEGIN ADD ***  08/16/00  ************************************
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     E*** END ADD *****  08/16/00  ************************************

     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    ELDIV
     C*** END ADD *****  11/15/99  ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQCNFL            1
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELPTDT        SVPTDT
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      'N'           QQCNFL
     C*
     C*  GET EMPLOYER DETAIL RECORD FOR THE FIRST DIVISION AND SAVE
     C*  THE POLICY TO-DATE.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*** BEGIN CHG ***  11/15/99  ************************************
     C*********            Z-ADDELPTDT    SVPTDT
     C     EMKEY         CHAIN     PEM01                              50
     C                   Z-ADD     1             XXX               1 0
     C                   Z-ADD     0             SVCNDT            7 0
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      CD(XXX)       SVCNDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*** END CHG *****  11/15/99  ************************************
     C*
     C     ELKY          READE     PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*** BEGIN CHG ***  11/15/99  ************************************
     C*********  ELPTDT    IFEQ SVPTDT
     C     EMKEY         CHAIN     PEM01                              50
     C                   Z-ADD     1             XXX               1 0
     C                   Z-ADD     0             XXCNDT            7 0
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      CD(XXX)       XXCNDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C     XXCNDT        IFEQ      SVCNDT
     C*** END CHG *****  11/15/99  ************************************
     C                   MOVE      'Y'           QQCNFL
     C                   ELSE
     C                   MOVE      'N'           QQCNFL
     C                   GOTO      TERM
     C                   END
     C*
     C     ELKY          READE     PEL01                                  60
     C                   END
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
