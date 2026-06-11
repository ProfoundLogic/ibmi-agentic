     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA009R - API to Add, Change, Delete FMP2L for Specific State
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
     FFMP2L     UF A E           K DISK    USROPN
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

     I**********    'OVRDBF FILE(FMP2LGA)-C         OVRDBF
     I**********    ' TOFILE(FMP2L'
     I**********    'DLTOVR FILE(FMP2LGA)'C         DLTOVR
      *
      *  Key List for Provider Auxiliary file
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
     C                   PARM                    @@MODE            1            'D' or Blank
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
     C                   PARM                    @@FL10            1            Flag 10
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
     C                   EXSR      SROPEN
      *
      *  Process either 'Add', 'Change', or 'Delete' Provider Auxiliary record
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'D'
     C                   EXSR      SRDEL
      *
     C     @@MODE        WHENEQ    *BLANK
     C                   EXSR      SRGET
     C     *IN60         IFEQ      *ON
     C                   EXSR      SRWRT
     C                   ELSE
     C                   EXSR      SRCHG
     C                   ENDIF
      *
     C                   OTHER
     C                   ENDSL
      *
     C                   EXSR      SRCLO
      *
     C                   MOVE      *ON           *INLR
      *
      *******************************************************************
      *  SR@@DB - Subroutine to Assign PARMs to File Fields.
      *******************************************************************
     C     SR@@DB        BEGSR
      *
     C                   MOVE      @@FEIN        P2FEIN                         FEIN
     C                   MOVE      @@MOD         P2MOD                          Mod
     C                   MOVE      @@SV01        P2SV01                         Service Code 1
     C*
     C                   MOVE      @@EFDT        XXEFDT            8 0          Effective Date
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2EFDT
     C*
     C                   MOVE      @@AM01        P2AM01                         Amount 1
     C                   MOVE      @@FL01        P2FL01                         Flag 1
     C                   MOVE      @@FL02        P2FL02                         Flag 2
     C                   MOVE      @@FL10        P2FL10                         Flag 10
     C                   MOVE      @@AM02        P2AM02                         Amount 2
     C                   MOVE      @@STAT        P2STAT                         Status
      *
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        P2USER
     C                   MOVEL     Q1PGM         P2PGM
     C                   Z-ADD     DSTIME        P2TIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2DT
      *
     C     @@ST          IFNE      'KY'
     C                   MOVE      @@AM01        XXAMT             9 2
     C                   Z-ADD     XXAMT         P2AM01                         for a 9.2 field
     C                   ELSE
     C                   MOVE      @@AM01        P2AM01                         for a 9.6 field
     C                   ENDIF
      *
     C                   Z-ADD     *ZEROS        P2AM03
     C                   Z-ADD     *ZEROS        P2AM04
     C                   Z-ADD     *ZEROS        P2AM05
     C                   Z-ADD     *ZEROS        P2AM06
     C                   Z-ADD     *ZEROS        P2AM07
     C                   Z-ADD     *ZEROS        P2AM08
     C                   Z-ADD     *ZEROS        P2AM09
     C                   Z-ADD     *ZEROS        P2AM10
     C                   MOVE      *BLANKS       P2FL03
     C                   MOVE      *BLANKS       P2FL04
     C                   MOVE      *BLANKS       P2FL05
     C                   MOVE      *BLANKS       P2FL06
     C                   MOVE      *BLANKS       P2FL07
     C                   MOVE      *BLANKS       P2FL08
     C                   MOVE      *BLANKS       P2FL09
     C                   MOVE      'H'           P2FL10
     C                   MOVEL     Q1USER        P2IUSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2IDT
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRCHG - Subroutine to Update a Provider Auxiliary record
      *******************************************************************
     C     SRCHG         BEGSR
      *
     C                   EXSR      SR@@DB
     C                   UPDATE    PP201
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRCLO - Subroutine to Close the Provider Auxiliary recored
      *******************************************************************
     C     SRCLO         BEGSR
      *
     C                   CLOSE     FMP2L
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRDB@@ - Subroutine to Assign File Fields to PARM Fields.
      *******************************************************************
     C     SRDB@@        BEGSR
     C                   MOVE      P2FEIN        @@FEIN                         FEIN
     C                   MOVE      P2MOD         @@MOD                          Mod
     C                   MOVE      P2SV01        @@SV01                         Service Code 1
     C*
     C                   Z-ADD     P2EFDT        SRDTI                                      te
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@EFDT
     C*
     C                   MOVE      P2AM01        @@AM01                         Amount 1
     C                   MOVE      P2FL01        @@FL01                         Flag 1
     C                   MOVE      P2FL02        @@FL02                         Flag 2
     C                   MOVE      P2AM02        @@AM02                         Amount 2
     C                   MOVE      P2STAT        @@STAT                         Status
     C                   ENDSR
      *
      *******************************************************************
      *  SRDEL - Subroutine to delete a Provider Auxiliary record
      *******************************************************************
     C     SRDEL         BEGSR
      *
     C                   EXSR      SRKFLD                                       Assign Key Fields
      *
     C     P2KY1         CHAIN     PP201                              60
      *
     C     *IN60         IFEQ      '0'
     C                   DELETE    PP201
     C                   ENDIF
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRGET - Subroutine to CHAIN to the Provider Auxiliary record
      *******************************************************************
     C     SRGET         BEGSR
      *
     C                   EXSR      SRKFLD                                       Assign Key Fields
      *
     C     P2KY1         CHAIN     PP201                              60
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRKFLD - Subroutine to Assign PARMs to File Key Fields.
      *******************************************************************
     C     SRKFLD        BEGSR
      *
     C                   MOVE      @@FEIN        XXFEIN                         FEIN
     C                   MOVE      @@MOD         XXMOD                          Mod
     C                   MOVE      @@SV01        XXCMSV                         Service Code 1
      *
     C                   MOVE      @@EFDT        XXEFDT
      *
     C                   Z-ADD     XXEFDT        SRDTI                          Effective Date
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SROPEN - Subroutine to Open file FMP2Lxx using OVRDBF
      *******************************************************************
     C     SROPEN        BEGSR
      *
      *  Override Database Files to Specific State Files
      *
     C********** @@ST      CAT  ')'       CH3     3
     C********** OVRDBF    CAT  CH3       CMD    37 P
     C**********           Z-ADD37        LENGTH
      *
     C**********           CALL 'QCMDEXC'                60
     C**********           PARM           CMD
     C**********           PARM           LENGTH 155
      *
     C                   OPEN      FMP2L
      *
     C                   ENDSR
      *
      *******************************************************************
      *  SRWRT - Subroutine to Write Provider Auxiliary record
      *******************************************************************
     C     SRWRT         BEGSR
      *
     C                   MOVE      @@FEIN        P2FEIN                         FEIN
     C                   MOVE      @@MOD         P2MOD                          Mod
     C                   MOVE      @@SV01        P2SV01                         Service Code 1
     C                   Z-ADD     XXFDAT        P2EFDT                         Effective Date
      *
     C                   EXSR      SR@@DB
      *
     C                   WRITE     PP201
      *
     C                   ENDSR
