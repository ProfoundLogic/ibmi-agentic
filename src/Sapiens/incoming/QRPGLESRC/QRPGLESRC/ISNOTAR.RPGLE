     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      /TITLE ISNOTAR- COPY DOCUMENT NOTES TO ADDTIONAL REMARKS FILE
      *****************************************************************
      * REVISIONS
      *
      *  04/06/04 C:128  Support: 42004                                 *
      *           This program was outputting dates to the AR file in
      *           the wrong format. AR dates must be in *CYMD format.
      *
      *****************************************************************
      * INDICATOR USAGE
     H*  60       General CHAIN
     H*  61       CHAIN
     H*  62       CHAIN
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
     F*
     FIMDNL     IF   E           K DISK
     FSMARL     UF A E           K DISK
     FSDCMLA    IF   E           K DISK
     F*
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************

     D*
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

     C*
     C*
     C*   KEY LIST FOR THE DARTS FIL
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C*
     C*   KEY LIST FOR THE BATCH NOTES FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C*
     C*
     C     ARK$$         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C                   KFLD                    $$ASQ#            5 0
     C*
     C     ARKY2         KLIST
     C                   KFLD                    QQKEY
     C*
     C*
     C     DNKY          KLIST
     C                   KFLD                    QQDCID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5 0
     C                   PARM                    QQRCD             1
      *** 04/06/04 Begin Add ******************************************
     C     DT6POS        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YMD'        SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C*
     C     DT8POS        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** 04/06/04 End   Add ******************************************
     C*
     C                   TIME                    DSDTTM
      *
     C*
     C*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*
     C     STRT          TAG
     C***
     C**** GET THE DART REMARK RECORD ***
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*
     C*DOCID GET DOC NOTES
     C     DNKY          CHAIN     PDN01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C** GET NEXT AVAIL ARSEQ#,
     C*
     C                   MOVE      *HIVAL        $$ASQ#
     C     ARK$$         SETGT     PAR01
     C     ARKY          READPE    PAR01                                  62
     C*
     C     *IN62         IFEQ      *OFF
     C                   ELSE
     C                   MOVE      *ZERO         ARASQ#
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C*** MOVE FIELDS TO AR FIELDS, THEN ADD 1 TO ARASQ# THEN WRITE RCD
      ***  MAKE SURE TO USE DATE THAT WILL LOCK OUT USERS FROM CHANGING
      ***  THESE RCDS IN DARTS***
     C                   MOVEL     CMKEY         ARKEY
     C                   MOVE      CMSEQ#        ARSEQ#
     C                   MOVEL     DNCM75        ARCM75
     C                   ADD       1             ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     CMPNAM        ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
      *** 04/06/04 Begin Chg ******************************************
     C******               MOVE DNTRDT    ARTRDT
     C*
     C                   Z-ADD     DNTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DT8POS
     C                   MOVEL     SRDTO         ARTRDT
      *** 04/06/04 End   Chg ******************************************
     C*
     C                   MOVE      DNTRTM        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     DNLUSR        ARUSER
     C                   MOVEL     DNLPGM        ARPGM
     C                   MOVE      DNLTIM        ARTIME
     C*
      *** 04/06/04 Begin Chg ******************************************
     C******               MOVE DNLDT     ARDT
     C*
     C                   Z-ADD     DNLDT         SRDTI
     C                   CALL      'SRCVTDAT'    DT6POS
     C                   MOVEL     SRDTO         ARDT
     C*
     C******               MOVE DNIDT     ARIDT
     C*
     C                   Z-ADD     DNIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DT6POS
     C                   MOVEL     SRDTO         ARIDT
      *** 04/06/04 End   Chg ******************************************
     C                   MOVEL     DNIUSR        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C     DNKY          READE     PDN01                                  61
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C/EJECT
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             XXTIME            6 0
     C                   Z-ADD     0             XXDATE            6 0
     C                   Z-ADD     0             XXSEQ#            5 0
     C                   MOVE      *HIVAL        X#SEQ#            5 0
     C*
     C                   Z-ADD     0             XXASQ#            5 0
     C*
     C*
     C                   ENDSR
     C*
     C*
