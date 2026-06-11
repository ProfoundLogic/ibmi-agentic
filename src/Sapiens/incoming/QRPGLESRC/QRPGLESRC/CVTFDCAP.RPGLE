     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTFDCAP - UPDATE FDCAP/CARCFL FOR SV vs. PR vs. RC CODES
     H**************************************************************************
     H* THE FOLLOWING CHART WILL BE FOLLOWED FOR THE UPDATE OF FDCAP/CARCFL:   *
     H* 1. CAPRTY IFEQ 'H'... CHAIN TO FMSVP                                   *
     H*      A) IF GET A HIT CARCFL = 'S'                                      *
     H*      B) IF NO HIT CARCFL = 'N'                                         *
     H* 2. CARCFL IFEQ 'Y' LEAVE AS IS                                         *
     H* 3. CARCFL IFEQ 'X' LEAVE AS IS                                         *
     H* 4. CAPRTY IFNE 'H' AND CARCFL IFNE 'Y' ANDNE 'X' CARCFL = 'C'          *
     H**************************************************************************
     H*
     FFDCAP     UF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN

     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D* QCMDEXC CMD STRING MAPPED
     D*
     D                 DS
     D  @@CMD                  1     56
     D  @@ST                  40     41
     D*
     D* SERVICE CODE
     D*
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNIT                 9     11  0

     C*
     C* INITIALIZE SECTION
     C*
     C                   MOVE      *BLANKS       OPNFL1            1            FILE OPEN FMSVP
     C                   MOVE      *BLANKS       OVRFL1            1            FILE OVRD FMSVP
     C*
     C* SERV CODE FILE
     C*
     C     SVKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    CAFDAT
     C*
     C* READ THE DETAIL CHARGE FILE
     C*
     C                   READ      PCA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* ONLY PROCESSRECORDS FOR CONVERSION IF CARCFL NE 'X' AND NE 'Y'
     C*
     C     CARCFL        IFNE      'X'
     C     CARCFL        ANDNE     'Y'
     C*
     C* IF CAPRTY IS 'H' WE MUST CHECK THE SERVICE CODE FILE, IF IT IS NOT
     C* AN 'H' WE CAN UPDATE CARCFL WITH A 'C'
     C*
     C     CAPRTY        IFEQ      '4'
     C*
     C* CLOSE SERVICE CODE FILE IF OPEN
     C*
     C     OPNFL1        IFEQ      '1'
     C                   CLOSE     FMSVL1
     C                   MOVE      ' '           OPNFL1
     C                   END
     C*
     C* DELETE OVERRIDE TO FMSVL1
     C*
     C     OVRFL1        IFEQ      '1'
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              61
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFL1
     C                   END
     C*
     C* OVERRIDE TO CORRECT STATE FMSVL1
     C*
     C     OVRFL1        IFEQ      ' '
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     CAAST         @@ST                           Generic SPCD File
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C                   MOVE      '1'           OVRFL1
     C                   END
     C*
     C* OPEN FILE FOR PROCESSING
     C*
     C     OPNFL1        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           OPNFL1
     C                   END
     C*
     C* CHAIN TO THE SERVICE CODE FILE TO SEE IF CURRENT CODE EXISTS
     C*
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVE      CACPT4        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C     SVKY          SETGT     PSV01
     C                   READP     PSV01                                  60
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C                   MOVEL     'S'           CARCFL
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   MOVEL     'N'           CARCFL
     C                   END                                                    END *IN60 EQ '0'
     C                   UPDATE    PCA01
     C*
     C                   ELSE                                                   ELSE CAPRTY EQ 'H'
     C*
     C                   MOVEL     'C'           CARCFL
     C                   UPDATE    PCA01
     C*
     C                   END                                                    END CAPRTY EQ 'H'
     C*
     C                   END                                                    END CARCFL NE 'X,Y'
     C*
     C                   READ      PCA01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
**  QCMDEXC TABLE
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
