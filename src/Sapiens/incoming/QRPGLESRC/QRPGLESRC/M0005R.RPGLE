     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE M0005R - HELP PROCESSOR -
     FM0005F    O    F  100        WORKSTN

     D STREAM          DS
     D  S                      1    100
     D                                     DIM(100)
     D  CV_T                   1     12
     D  T                             2B 0
     D                                     DIM(6)
     D                                     OVERLAY(CV_T)
     D* LENGTH OF DATA STREAM TO SEND (DISPLAY) IN HEX
     D  LSND                   1      2B 0
     D* LENGTH OF DATA STREAM TO RECEIVE IN HEX
     D  LREC                   3      4B 0
     D* REQUESTED FUNCTION
     D* HEX 71 SEND
     D* HEX 73 SEND RECEIVE
     D  OP                     5      5
     D* MUST EQUAL HEX 04
     D  ESC                    6      6
     D* MUST EQUAL HEX 11
     D  WTD                    7      7
     D* MUST EQUAL HEX 00
     D  CC1                    8      8
     D* MUST EQUAL HEX 08
     D  CC2                    9      9
     D* MUST EQUAL HEX 11
     D  SBA                   10     10
     D* STARTING ROW TO DISPLAY CHARACTER STREAM IN HEX
     D  ROW                   11     11
     D* STARTING COLUMN TO DISPLAY CHAR STREAM IN HEX
     D  COL                   12     12
     D* DATA STRUCTURE TO CONVERT STARTING ROW/COL TO HEX VALUES
     D                 DS
     D  ROWB                   1      2B 0
     D  ROWC                   2      2
     D  COLB                   3      4B 0
     D  COLC                   4      4

     C     *ENTRY        PLIST
     C                   PARM                    FUNC              1            FUNCTION S/R
     C                   PARM                    CHARS           100            CHAR STREAM
     C                   PARM                    LEN               2 0          SEND LEN
     C                   PARM                    ROWI              2 0          START ROW
     C                   PARM                    COLI              2 0          START COL
     C*
     C     *IN99         IFEQ      '0'
     C                   EXSR      FIRST
     C                   END
     C*
     C*   CHECK FOR VALID STARTING ROW AND COLUMN
     C*
     C     ROWI          IFLT      01
     C     ROWI          ORGT      24
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C     COLI          IFLT      01
     C     COLI          ORGT      80
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C*  IF NO ERRORS WRITE USER DEFINED DATA STREAM TO DISPLAY
     C*
     C     *IN98         IFEQ      '0'
     C                   MOVEA     CHARS         S
     C*
     C     FUNC          IFEQ      'R'
     C                   EXSR      CTLRDE
     C                   MOVE      '1'           *INLR
     C                   ELSE
     C                   EXSR      CTLSND
     C                   END
     C*
     C*  CONVERT STARTING ROW/COLUMN TO HEX VALUES
     C*
     C                   Z-ADD     ROWI          ROWB
     C                   Z-ADD     COLI          COLB
     C                   MOVE      ROWC          ROW
     C                   MOVE      COLC          COL
     C*
     C*  DISPLAY THE DATA STREAM
     C*
     C                   EXCEPT    WRITE
     C*
     C                   END
     C                   RETURN
     C*
     C*  SUBROUTINE TO SET CONTROL CHARACTERS TO SEND
     C*
     C     CTLSND        BEGSR
     C                   Z-ADD     0             T
     C                   Z-ADD     LEN           LSND
     C*  SET CONTROL CHARACTERS
     C                   BITON     '1237'        OP
     C                   BITON     '5'           ESC
     C                   BITON     '37'          WTD
     C                   BITOFF    '01234567'    CC1
     C                   BITON     '4'           CC2
     C                   BITON     '37'          SBA
     C                   ENDSR
     C*
     C*  SUBROUTINE TO SET SEND/RECEIVE CHARACTERS
     C*
     C     CTLRDE        BEGSR
     C                   Z-ADD     0             T
     C                   Z-ADD     SNDLEN        LSND
     C                   Z-ADD     RCVLEN        LREC
     C                   BITON     '12367'       OP
     C                   BITON     '5'           ESC
     C                   BITON     '136'         WTD
     C                   BITOFF    '01234567'    CC1
     C                   BITON     '4'           CC2
     C                   ENDSR
     C*
     C*  SUBROUTINE FOR FIRST TIME PROCESSING
     C*
     C     FIRST         BEGSR
     C                   Z-ADD     4             SNDLEN            2 0
     C                   Z-ADD     1920          RCVLEN            4 0
     C                   MOVE      '1'           *IN99
     C                   ENDSR
     OM0005F    E            WRITE
     O                                         K005 'SCR01'
     O                       STREAM             100
