     H* (C) Copyright Fiserv Insurance Solutions, Inc., 1997 - 2006
     H*
     H/TITLE WA063R - Delete a selected log record & associate records.
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:
     H*
     H*    This program will delete a slected log record and associated
     H*    log records.
     H*
     H*  CALLED FROM:
     H*    W0080R1
     H*
     H*  CALLS TO:
     H*
     H*    PROGRAMMER: C:183
     H*    CREATED:    02/16/07     PROJECT: 114WLY
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWMLGP     UF   E             DISK
     FWDPTL     UF   E           K DISK
     FWEREL     UF   E           K DISK
     FWDAJL     UF   E           K DISK
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RTCD            1
      *
     C     P0KY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGDSQH
     C*
     C     AJKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGDSQM
      *
     C     @@RRN         CHAIN     WMLGP
     C                   IF        %FOUND(WMLGP)
      *
     C     P0KY          CHAIN     WDPTL                              99
     C                   DOW       *IN99 = *OFF
     C                   DELETE    WDPTL                                90
     C     P0KY          READE     WDPTL                                  99
     C                   ENDDO
      *
     C     P0KY          CHAIN     WEREL                              99
     C                   DOW       *IN99 = *OFF
     C                   DELETE    WEREL                                90
     C     P0KY          READE     WEREL                                  99
     C                   ENDDO
      *
     C     AJKY          CHAIN     WDAJL                              99
     C                   DOW       *IN99 = *OFF
     C                   DELETE    WDAJL                                90
     C     AJKY          READE     WDAJL                                  99
     C                   ENDDO
      *
     C                   DELETE    WMLGP
     C                   MOVE      *OFF          @@RTCD
      *
      * No wmlgp - error
     C                   ELSE
     C                   MOVE      *ON           @@RTCD
     C                   ENDIF
      *
     C                   EVAL      *INLR = *ON
     C                   RETURN
