     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE M0007R - HELP PROCESSOR -
     FM0007F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FINPUT     IF   F   92        DISK    USROPN

     D S               S              1    DIM(100)
     D NUM             S              1  0 DIM(8)
     D TT              S              1    DIM(80)
     D WTX             S              1    DIM(16)
     D DFW             S             24    DIM(3) CTDATA PERRCD(1)
     D ST              S              5  0 DIM(50)
     D KEY             S              5  0 DIM(2)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D CLRWNT          DS
     D  STRRWA                 1      2
     D  STRCLA                 3      4
     D  ENDRWA                 5      6
     D  ENDCLA                 7      8
     D WNDDS           DS                  OCCURS(22)
     D  WNDTXT                 1     80
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     IINPUT     NS  01
     I                                  1    6  SRCSTM
     I                                  1    4  SRCST1
     I                                  1    4 0SRCST2
     I                                  7   12  SRCDTE
     I                                 13   92  SRCTXT
     I                                 18   27  WNDID
     I                                 29   30  STRRWA
     I                                 32   33  STRCLA
     I                                 35   36  ENDRWA
     I                                 38   39  ENDCLA

     C*
     C*  ENTRY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    FILNAM           10
     C                   PARM                    LIBNAM           10
     C                   PARM                    MBRNAM           10
     C*
     C*  FIRST TIME PROCESSING
     C*
     C     *IN99         IFEQ      '0'
     C                   EXSR      FIRST
     C                   END
     C*
     C*  MAINLINE
     C*
     C     *IN98         IFEQ      '0'
     C                   EXSR      WNDNUM
     C                   END
     C*
     C     *IN98         IFEQ      '0'
     C                   EXSR      WNDVAL
     C                   END
     C*
     C     REPLAY        TAG
     C*
     C     *IN98         IFEQ      '0'
     C                   EXSR      WNDBLD
     C                   END
     C*
     C     *IN98         IFEQ      '1'
     C                   EXSR      DFTWND
     C                   END
     C*
     C                   MOVE      *BLANK        SCUP
     C                   MOVE      *BLANK        SCDOWN
     C     *IN98         IFEQ      '0'
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVE      RR(1)         SCUP              9
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVE      RR(2)         SCDOWN            9
     C                   END
     C                   END
     C*
     C                   EXSR      WRTWND
     C*
     C     STRT          TAG
     C                   EXFMT     SCR01
     C*
     C     *IN98         IFEQ      '0'
     C     *IN18         IFEQ      '1'
     C     *IN19         OREQ      '1'
     C                   GOTO      REPLAY
     C                   END
     C                   END
     C*
     C                   CLOSE     INPUT
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C/EJECT
     C*
     C*  SUBROUTINE FOR FIRST TIME PROCESSING
     C*
     C     FIRST         BEGSR
     C                   Z-ADD     *ZERO         STRRW             2 0
     C                   Z-ADD     *ZERO         STRCL             2 0
     C                   Z-ADD     *ZERO         ENDRW             2 0
     C                   Z-ADD     *ZERO         ENDCL             2 0
     C                   Z-ADD     *ZERO         LBPOS             2 0
     C                   Z-ADD     *ZERO         TBPOS             2 0
     C                   Z-ADD     *ZERO         ROW               2 0
     C                   Z-ADD     *ZERO         COL               2 0
     C                   Z-ADD     *ZERO         LEN               2 0
     C                   Z-ADD     *ZERO         LSND              2 0
     C                   Z-ADD     *ZERO         RCDS              2 0
     C                   Z-ADD     *ZERO         PS                2 0
     C                   Z-ADD     *ZERO         PSE               2 0
     C                   Z-ADD     *ZERO         LT                2 0
     C                   Z-ADD     *ZERO         X                 2 0
     C                   Z-ADD     *ZERO         XX                2 0
     C                   MOVE      *BLANK        TSTPAR            1
     C                   MOVE      *BLANK        TSTPRT            2
     C                   MOVE      *BLANK        BTMBAR            1
     C                   MOVE      *BLANK        RHTBAR            1
     C                   MOVE      *BLANK        FUNC              1
     C*  HEX 20
     C*
     C                   BITOFF    '01234567'    X20               1
     C                   BITON     '2'           X20
     C*  HEX 22 HILITE
     C*
     C                   BITOFF    '01234567'    X22               1
     C                   BITON     '26'          X22
     C*  HEX 23 REVERSE IMAGE/ HILIGHT
     C*
     C                   BITOFF    '01234567'    X23               1
     C                   BITON     '267'         X23
     C*
     C*  HEX 1F RIGHT AND LEFT BAR CHAR
     C*
     C                   BITOFF    '01234567'    X1F               1
     C                   BITON     '34567'       X1F
     C                   MOVE      X1F           RHTBAR
     C*
     C*  HEX 00 TO TEST FOR END OF FILE IN TEXT MANAGEMENT
     C*
     C                   BITOFF    '01234567'    TSTPAR
     C*
     C*  OPEN WINDOW TEXT FILE
     C*
     C                   OPEN      INPUT                                25
     C     *IN25         IFEQ      '1'
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END
     C*
     C                   MOVE      '1'           *IN99
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO CHECK FOR VALID NUMERIC DATA
     C*
     C     WNDNUM        BEGSR
     C*
     C*  FIRST RECORD SHOULD CONTAIN THE STARTING AND ENDING ROW/COL
     C*
     C                   READ      INPUT                                  25
     C     *IN25         IFEQ      '1'
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C*
     C*  CHECK FOR VALID NUMERIC DATA
     C*
     C                   MOVE      CLRWNT        NUM
     C     1             DO        8             X
     C     NUM(X)        IFLT      0
     C     NUM(X)        ORGT      9
     C                   MOVE      '1'           *IN98
     C                   MOVE      '1'           *IN26
     C                   END
     C  N26              END
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO CHECK FOR VALID WINDOW POSITION
     C*
     C     WNDVAL        BEGSR
     C*
     C*  MOVE STARTING AND ENDING ROW/COLUMN ALPHA DATA TO NUMERIC
     C*  FIELDS
     C*
     C                   MOVE      STRRWA        STRRW
     C                   MOVE      STRCLA        STRCL
     C                   MOVE      ENDRWA        ENDRW
     C                   MOVE      ENDCLA        ENDCL
     C*
     C*  STARTING ROW
     C*
     C     STRRW         IFLT      2
     C     STRRW         ORGT      23
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C*  STARTING COLUMN
     C*
     C     STRCL         IFLT      4
     C     STRCL         ORGT      75
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C*  ENDING ROW
     C*
     C     ENDRW         IFLT      2
     C     ENDRW         ORGT      23
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C*  ENDING COLUMN
     C*
     C     ENDCL         IFLT      4
     C     ENDCL         ORGT      75
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C*  MUST BE ABLE TO DISPLAY AT LEAST ONE CHARACTER IN WINDOW
     C*  FOR VALID WINDOW SIZE
     C*
     C     ENDRW         IFLT      STRRW
     C     ENDCL         ORLT      STRCL
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO READ WINDOW TEXT FILE
     C*
     C     WNDBLD        BEGSR
     C*
     C                   EXSR      WNDPOS
     C     *IN30         IFEQ      '0'
     C                   MOVE      *ZERO         KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C     2             CHAIN     INPUT                              60
     C                   EXSR      LOAD
     C                   END
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C     KEY(2)        CHAIN     INPUT                              60
     C                   EXSR      LOAD
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C     KEY(1)        CHAIN     INPUT                              60
     C                   EXSR      LOADP
     C                   END
     C*
     C  N30              MOVE      '1'           XXDOWN
     C                   MOVE      '1'           *IN30
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO CALCULATE WINDOW POSITIONS
     C*
     C     WNDPOS        BEGSR
     C*
     C*  THE SIZE OF THE WINDOW TEXT TO DISPLAY IS THE DIFF
     C*  BETWEEN STARTING COLUMN AND ENDING COLUMN
     C*
     C     ENDCL         SUB       STRCL         LEN
     C                   ADD       1             LEN
     C     STRCL         SUB       3             LBPOS
     C*
     C*  NUMBER OF RECORDS TO DISPLAY IS DIFFERENCE BETWEEN TOP
     C*  BAR POSITION AND ENDING ROW
     C*
     C     STRRW         SUB       1             TBPOS
     C     ENDRW         SUB       TBPOS         RCDS
     C*
     C*  SET POSITION FOR RIGHT BAR OF WINDOW
     C*
     C     LEN           ADD       17            PS
     C*
     C*  SET POSITION FOR HEX CHARACTER TO TURN OFF DSPATR HI-LITE
     C*
     C     LEN           ADD       18            PSE
     C                   Z-ADD     TBPOS         ROW
     C                   Z-ADD     LBPOS         COL
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO PUT WINDOW TEXT TO DATA STRUCTURE
     C*
     C     WNDLIN        BEGSR
     C                   EXSR      PRRTST
     C     *IN25         IFEQ      '0'
     C                   EXSR      PARTST
     C     I1            OCCUR     WNDDS
     C                   MOVE      SRCTXT        WNDTXT
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO TEST FOR NON-DISPLAYABLE CHARACTERS PUT IN
     C*  BY TEXT MANAGEMENT
     C*
     C     PARTST        BEGSR
     C*
     C                   MOVEA     SRCTXT        TT
     C     1             DO        LEN           XX
     C     TT(XX)        IFEQ      TSTPAR
     C                   MOVE      *BLANK        TT(XX)
     C                   END
     C                   MOVEA     TT            SRCTXT
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO CHECK FOR PRINT DEFINITION RECORD ADDED
     C*  BY TEXT MANAGEMENT
     C*
     C     PRRTST        BEGSR
     C                   MOVE      '0'           *IN25
     C                   MOVEL     SRCTXT        TSTPRT
     C     TSTPRT        IFEQ      '.P'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN25
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO SET UP DEFAULT ERROR WINDOW
     C*
     C     DFTWND        BEGSR
     C*
     C*  AN INVALID WINDOW WAS SPECIFIED IN WINDOW TEXT FILE
     C*  SET UP DEFAULT WINDOW
     C*
     C     1             DO        2             X
     C     X             OCCUR     WNDDS
     C                   MOVEL     DFW(X)        WNDTXT
     C                   END
     C     X             OCCUR     WNDDS
     C                   MOVEA     'FILE: '      WTX(1)
     C                   MOVEA     FILNAM        WTX(7)
     C                   MOVEA     WTX           WNDTXT
     C                   MOVEA     *BLANKS       WTX
     C                   ADD       1             X
     C     X             OCCUR     WNDDS
     C                   MOVEA     'LIB: '       WTX(1)
     C                   MOVEA     LIBNAM        WTX(6)
     C                   MOVEA     WTX           WNDTXT
     C                   MOVEA     *BLANKS       WTX
     C                   ADD       1             X
     C     X             OCCUR     WNDDS
     C                   MOVEA     'MBR: '       WTX(1)
     C                   MOVEA     MBRNAM        WTX(6)
     C                   MOVEA     WTX           WNDTXT
     C                   ADD       1             X
     C                   MOVEL     DFW(3)        WNDTXT
     C*
     C*  SET UP DEFAULT ERROR WINDOW ROW/COL LENGHT
     C*
     C                   Z-ADD     16            ROW
     C                   Z-ADD     50            COL
     C                   Z-ADD     24            LEN
     C                   Z-ADD     X             RCDS
     C     LEN           ADD       17            PS
     C     LEN           ADD       18            PSE
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO WRITE WINDOW TO DISPLAY
     C*
     C     WRTWND        BEGSR
     C*
     C                   EXSR      WNDTOP
     C                   EXSR      WNDDTA
     C                   EXSR      WNDBTM
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO WRITE TOP BAR OF WINDOW TO DISPLAY
     C*
     C     WNDTOP        BEGSR
     C*
     C*  TOTAL DATA STREAM LENGTH = LENGTH OF TEXT + 7 CONTROL
     C*  CHAR (ESC,WTD,CC1,CC2,SBA,ROW,COL) + RIGHT AND LEFT BAR
     C*  OF WINDOW
     C*
     C     13            ADD       LEN           LSND
     C*
     C*  WRITE TOP BAR OF WINDOW
     C*
     C                   MOVEA     *BLANK        S(16)
     C                   MOVEA     X23           S(13)
     C                   MOVEA     X20           S(PSE)
     C                   CALL      'M0005R'
     C                   PARM                    FUNC
     C                   PARM                    S
     C                   PARM                    LSND
     C                   PARM                    ROW
     C                   PARM                    COL
     C                   MOVEA     *BLANK        S(13)
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO WRITE EACH LINE OF WINDOW
     C*
     C     WNDDTA        BEGSR
     C*
     C     1             DO        RCDS          X
     C                   ADD       1             ROW
     C     X             OCCUR     WNDDS
     C                   MOVEA     WNDTXT        S(16)
     C                   MOVEA     X1F           S(14)
     C                   MOVEA     X22           S(13)
     C                   MOVEA     X1F           S(PS)
     C                   MOVEA     X20           S(PSE)
     C                   CALL      'M0005R'
     C                   PARM                    FUNC
     C                   PARM                    S
     C                   PARM                    LSND
     C                   PARM                    ROW
     C                   PARM                    COL
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*
     C*  SUBROUTINE TO WRITE BOTTOM BAR OF WINDOW
     C*
     C     WNDBTM        BEGSR
     C                   MOVEA     *BLANK        S(16)
     C                   MOVE      X23           S(13)
     C                   MOVE      ' '           S(14)
     C                   MOVE      ' '           S(PS)
     C                   MOVE      X20           S(PSE)
     C                   MOVEA     SCUP          S(17)
     C                   MOVEA     SCDOWN        S(27)
     C                   ADD       1             ROW
     C*
     C                   CALL      'M0005R'
     C                   PARM                    FUNC
     C                   PARM                    S
     C                   PARM                    LSND
     C                   PARM                    ROW
     C                   PARM                    COL
     C*
     C                   ENDSR
     C*
     C*  SUBROUTINES TO BUILD INITIAL DISPLAY, ROLLING INFORMATION
     C*
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     0             I1
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1                5 0
     C                   EXSR      FILL
     C                   READ      INPUT                                  60
     C                   EXSR      PRRTST
     C   25              MOVE      '1'           *IN60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      RCDS                                 61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     ST(1)         KEY(1)
     C                   Z-ADD     ST(RCDS)      KEY(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C     1             ADD       RCDS          I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     INPUT                                  60
     C  N60SRCST2        COMP      1                                      60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     ST(1)         KEY(1)
     C                   Z-ADD     ST(RCDS)      KEY(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     0             I1
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C     2             CHAIN     INPUT                              60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   READ      INPUT                                  60
     C                   EXSR      PRRTST
     C   25              MOVE      '1'           *IN60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      RCDS                                 61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   EXSR      WNDLIN
     C                   MOVE      SRCST1        ST(I1)
     C                   ENDSR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *ZERO         ST
     C     1             DO        RCDS          X
     C     X             OCCUR     WNDDS
     C                   MOVE      *BLANK        WNDTXT
     C                   END
     C                   ENDSR
**
An invalid window was
specified in -
Contact data processing.
**
ROLL UP
ROLL DOWN
