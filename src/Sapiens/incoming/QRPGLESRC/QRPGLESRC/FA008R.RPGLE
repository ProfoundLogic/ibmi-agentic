     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA008R - API to Access FMP2L1 for Specific State
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    F0031R1                                                      *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  01/29/97  C:30  Project:  87MBB                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60      - General File Access                                *
     H*    99 *OFF - First Time Execution                               *
     H*                                                                 *
     H*******************************************************************
      *
      * MCMS Provider Auxiliary file
     FFMP2L1    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS1)
      *

     D DBFDS1          DS
     D  DBRRN1               397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *

     I**********    'OVRDBF FILE(FMP2L1GA-C         OVRDBF
     I**********    ') TOFILE(FMP2L1'
     I**********    'DLTOVR FILE(FMP2L1GA-C         DLTOVR
     I**********    ')'
      *

     C     P2KY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     P2KY1         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXFDAT
      *
     C     *LIKE         DEFINE    P2FEIN        XXFEIN
     C     *LIKE         DEFINE    P2MOD         XXMOD
     C     *LIKE         DEFINE    P2SV01        XXCMSV
     C     *LIKE         DEFINE    P2EFDT        XXFDAT
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@EOF             1            '1' = ENDFILE
     C                   PARM                    @@RTCD            1            '1' = Error
     C                   PARM                    @@ST              2            State Code
     C                   PARM                    @@FEIN            9            FEIN
     C                   PARM                    @@MOD             3            Mod
     C                   PARM                    @@SV01           11            Service Code 1
     C                   PARM                    @@EFDT            8            Effective Date
     C                   PARM                    @@AM01            9            Amount 1
     C                   PARM                    @@FL01            1            Flag 1
     C                   PARM                    @@FL02            1            Flag 2
     C                   PARM                    @@AM02            9            Amount 2
     C                   PARM                    @@STAT            1            Status
      *
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *          M A I N     L O G I C
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      SRONCE
     C                   ELSE
     C                   EXSR      SRGET
     C                   ENDIF
      *
     C     @@EOF         IFEQ      *OFF
     C                   RETURN
     C                   ENDIF
      *
      *  EOJ Logic
      *
     C                   EXSR      SRCLO
      *
     C                   MOVE      *ON           *INLR
      *
      *******************************************************************
      *  SR@@DB - Subroutine to Assign PARMs to File Fields.
      *******************************************************************
     C     SR@@DB        BEGSR
     C                   MOVE      @@FEIN        P2FEIN                         FEIN
     C                   MOVE      @@MOD         P2MOD                          Mod
     C                   MOVE      @@SV01        P2SV01                         Service Code 1
     C*
     C                   MOVE      @@EFDT        XXEFDT            8 0
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2EFDT
     C*
     C                   MOVE      @@AM01        P2AM01                         Amount 1
     C                   MOVE      @@FL01        P2FL01                         Flag 1
     C                   MOVE      @@FL02        P2FL02                         Flag 2
     C                   MOVE      @@AM02        P2AM02                         Amount 2
     C                   MOVE      @@STAT        P2STAT                         Status
     C                   ENDSR
      *
      *******************************************************************
      *  SRDB@@ - Subroutine to Assign File Fields to PARM Fields.
      *******************************************************************
     C     SRDB@@        BEGSR
     C                   MOVE      P2FEIN        @@FEIN                         FEIN
     C                   MOVE      P2MOD         @@MOD                          Mod
     C                   MOVE      P2SV01        @@SV01                         Service Code 1
     C                   MOVE      P2EFDT        @@EFDT                         Effective Date
     C                   MOVE      P2AM01        @@AM01                         Amount 1
     C                   MOVE      P2FL01        @@FL01                         Flag 1
     C                   MOVE      P2FL02        @@FL02                         Flag 2
     C                   MOVE      P2AM02        @@AM02                         Amount 2
     C                   MOVE      P2STAT        @@STAT                         Status
     C                   ENDSR
      *
      *
      *******************************************************************
      *  SRCLO - Subroutine to Perform "Last Call" logic
      *******************************************************************
     C     SRCLO         BEGSR
      *
     C                   CLOSE     FMP2L1
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRGET - Subroutine to Read Equal on Provider/Mod
      *******************************************************************
     C     SRGET         BEGSR
      *
     C                   EXSR      SR@@DB
      *
     C     P2KY          READE     PP201                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   EXSR      SRDB@@
     C                   ELSE
     C                   MOVE      *ON           @@EOF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRONCE - Subroutine to Execute First Time Processing
      *******************************************************************
     C     SRONCE        BEGSR
      *
     C                   MOVE      *ON           *IN99
      *
      *  Override Database Files to Specific State Files
      *
     C********** @@ST      CAT  ')'       CH3     3
     C********** OVRDBF    CAT  CH3       CMD    38 P
     C**********           Z-ADD38        LENGTH
      *
     C**********           CALL 'QCMDEXC'                60
     C**********           PARM           CMD
     C**********           PARM           LENGTH 155
      *
     C                   OPEN      FMP2L1
      *
     C                   EXSR      SR@@DB
     C                   MOVE      @@FEIN        XXFEIN
     C                   MOVE      @@MOD         XXMOD
      *
     C     P2KY          CHAIN     PP201                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           @@RTCD
     C                   ELSE
     C                   EXSR      SRDB@@
     C                   ENDIF
      *
     C                   ENDSR
