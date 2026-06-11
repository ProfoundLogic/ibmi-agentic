     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F6202R  - EDI/NOI DATA ELEMENT ARRAY EDITOR
     H*****************************************************************
     H* Description:                                                  *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 02/18/05 C:79 PROJECT: TX029                                  *
     H*               ADD LOGIC FOR TEXAS 837                         *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 11 - Chain indicator on log file.                             *
     H* 12 - Chain indicator on all other data base files.            *
     H* LR - Indicates when program is to be ended with all files     *
     H*      closed.                                                  *
     H*****************************************************************
     H*
     FFTMXL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     F*

     D*===============================================================
     D DN              S             30    DIM(999)                             Data Elements
     E*

     I*===============================================================
     I*

     C*================================================================
     C*
     C*--------  MAINLINE -----------------------
     C*
     C*   1. For the specified data element number, retrieve the
     C*      entry in the data element criteria table.
     C*   2. For the specified data element number, retrieve the
     C*      entry(s) in the edit matrix table.
     C*   3. For each entry in the edit matrix table, execute the
     C*      appropriate edit routine.
     C*   4. Pass back the string containing the error numbers, if
     C*      any, for the element number.
     C*   5. When the program is called with a zero element number,
     C*      terminate the program with LR on.
     C*
     C                   EXSR      SRINIT
     C*
     C     MXKY          CHAIN     PMX01                              11
     C*
     C     *IN11         DOWEQ     *OFF
     C*** BEG ADD *** 02/18/05 ****************************************
     C     MXSTAT        IFEQ      'A'
     C*** END ADD *** 02/18/05 ****************************************
     C*
     C     MXTSET        IFEQ      DN(1)
     C     MXTSET        OREQ      *BLANKS
     C*
     C     MXMTC         IFEQ      DN(2)
     C     MXMTC         OREQ      *BLANKS
     C*
     C                   SELECT
     C*
     C     MXMSG#        WHENEQ    1
     C                   EXSR      SRE001
     C*
     C     MXMSG#        WHENEQ    2
     C                   EXSR      SRE002
     C*
     C     MXMSG#        WHENEQ    3
     C                   EXSR      SRE003
     C*
     C     MXMSG#        WHENEQ    5
     C                   EXSR      SRE005
     C*
     C     MXMSG#        WHENEQ    6
     C                   EXSR      SRE006
     C*
     C     MXMSG#        WHENEQ    7
     C                   EXSR      SRE007
     C*
     C     MXMSG#        WHENEQ    8
     C                   EXSR      SRE008
     C*
     C     MXMSG#        WHENEQ    9
     C                   EXSR      SRE009
     C*
     C     MXMSG#        WHENEQ    10
     C                   EXSR      SRE010
     C*
     C     MXMSG#        WHENEQ    11
     C                   EXSR      SRE011
     C*
     C     MXMSG#        WHENEQ    12
     C                   EXSR      SRE012
     C*
     C     MXMSG#        WHENEQ    13
     C                   EXSR      SRE013
     C*
     C     MXMSG#        WHENEQ    14
     C                   EXSR      SRE014
     C*
     C     MXMSG#        WHENEQ    15
     C                   EXSR      SRE015
     C*
     C     MXMSG#        WHENEQ    16
     C                   EXSR      SRE016
     C*
     C     MXMSG#        WHENEQ    17
     C                   EXSR      SRE017
     C*
     C     MXMSG#        WHENEQ    18
     C                   EXSR      SRE018
     C*
     C     MXMSG#        WHENEQ    19
     C                   EXSR      SRE019
     C*
     C     MXMSG#        WHENEQ    20
     C                   EXSR      SRE020
     C*
     C     MXMSG#        WHENEQ    21
     C                   EXSR      SRE021
     C*
     C     MXMSG#        WHENEQ    22
     C                   EXSR      SRE022
     C*
     C     MXMSG#        WHENEQ    23
     C                   EXSR      SRE023
     C*
     C     MXMSG#        WHENEQ    24
     C                   EXSR      SRE024
     C*
     C     MXMSG#        WHENEQ    25
     C                   EXSR      SRE025
     C*
     C     MXMSG#        WHENEQ    26
     C                   EXSR      SRE026
     C*
     C     MXMSG#        WHENEQ    27
     C                   EXSR      SRE027
     C*
     C     MXMSG#        WHENEQ    28
     C                   EXSR      SRE028
     C*
     C     MXMSG#        WHENEQ    29
     C                   EXSR      SRE029
     C*
     C     MXMSG#        WHENEQ    30
     C                   EXSR      SRE030
     C*
     C     MXMSG#        WHENEQ    31
     C                   EXSR      SRE031
     C*
     C     MXMSG#        WHENEQ    32
     C                   EXSR      SRE032
     C*
     C     MXMSG#        WHENEQ    33
     C                   EXSR      SRE033
     C*
     C     MXMSG#        WHENEQ    34
     C                   EXSR      SRE034
     C*
     C     MXMSG#        WHENEQ    35
     C                   EXSR      SRE035
     C*
     C     MXMSG#        WHENEQ    36
     C                   EXSR      SRE036
     C*
     C     MXMSG#        WHENEQ    37
     C                   EXSR      SRE037
     C*
     C     MXMSG#        WHENEQ    38
     C                   EXSR      SRE038
     C*
     C     MXMSG#        WHENEQ    39
     C                   EXSR      SRE039
     C*
     C     MXMSG#        WHENEQ    40
     C                   EXSR      SRE040
     C*
     C     MXMSG#        WHENEQ    41
     C                   EXSR      SRE041
     C*
     C     MXMSG#        WHENEQ    42
     C                   EXSR      SRE042
     C*
     C     MXMSG#        WHENEQ    43
     C                   EXSR      SRE043
     C*
     C     MXMSG#        WHENEQ    44
     C                   EXSR      SRE044
     C*
     C     MXMSG#        WHENEQ    45
     C                   EXSR      SRE045
     C*
     C     MXMSG#        WHENEQ    46
     C                   EXSR      SRE046
     C*
     C     MXMSG#        WHENEQ    47
     C                   EXSR      SRE047
     C*
     C     MXMSG#        WHENEQ    48
     C                   EXSR      SRE048
     C*
     C     MXMSG#        WHENEQ    49
     C                   EXSR      SRE049
     C*
     C     MXMSG#        WHENEQ    50
     C                   EXSR      SRE050
     C*
     C     MXMSG#        WHENEQ    51
     C                   EXSR      SRE051
     C*
     C     MXMSG#        WHENEQ    52
     C                   EXSR      SRE052
     C*
     C     MXMSG#        WHENEQ    53
     C                   EXSR      SRE053
     C*
     C     MXMSG#        WHENEQ    54
     C                   EXSR      SRE054
     C*
     C     MXMSG#        WHENEQ    55
     C                   EXSR      SRE055
     C*
     C     MXMSG#        WHENEQ    56
     C                   EXSR      SRE056
     C*
     C     MXMSG#        WHENEQ    57
     C                   EXSR      SRE057
     C*
     C     MXMSG#        WHENEQ    58
     C                   EXSR      SRE058
     C*
     C     MXMSG#        WHENEQ    59
     C                   EXSR      SRE059
     C*
     C     MXMSG#        WHENEQ    60
     C                   EXSR      SRE060
     C*
     C     MXMSG#        WHENEQ    61
     C                   EXSR      SRE061
     C*
     C     MXMSG#        WHENEQ    62
     C                   EXSR      SRE062
     C*
     C     MXMSG#        WHENEQ    63
     C                   EXSR      SRE063
     C*
     C     MXMSG#        WHENEQ    64
     C                   EXSR      SRE064
     C*
     C     MXMSG#        WHENEQ    65
     C                   EXSR      SRE065
     C*
     C     MXMSG#        WHENEQ    66
     C                   EXSR      SRE066
     C*
     C     MXMSG#        WHENEQ    67
     C                   EXSR      SRE067
     C*
     C     MXMSG#        WHENEQ    68
     C                   EXSR      SRE068
     C*
     C                   OTHER
     C                   EXSR      SRE999
     C*
     C                   ENDSL
     C*
     C*
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/18/05 ****************************************
     C*
     C     MXKY          READE     PMX01                                  11
     C                   ENDDO
     C*
     C*   Return the error string to the caller.
     C*
     C                   RETURN
     C/EJECT
     C*================================================================
     C*          ROUTINE - SRINIT
     C*
     C*   Initiallization routine.
     C*          This routine ensures the following steps are done:
     C*          1. All work fields, parm lists, key lists, and
     C*             data structures are defined and initiallized.
     C*
     C     SRINIT        BEGSR
     C*
     C     *LIKE         DEFINE    QQCO#         XXCO#
     C     *LIKE         DEFINE    QQJST         XXJST
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C     *LIKE         DEFINE    MXDN#         XXDN#
     C*
     C                   CLEAR                   XXERRS
     C*
     C                   MOVE      *BLANKS       XXDVAL           30
     C                   MOVE      *ZEROS        XTDAT8            8 0
     C                   MOVE      *ZEROS        XXDAT8            8 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       XXST              2
     C                   MOVE      *BLANKS       XXWK1A            1
     C                   MOVE      *BLANKS       XXWK4A            4
     C*
     C*   Define parm lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQJST             2
     C                   PARM                    QQDN#             5 0
     C                   PARM                    QQTSET            3
     C                   PARM                    QQMTC             2
     C                   PARM                    DN
     C                   PARM                    XXERRS           30
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
     C*
     C*   Define key lists
     C*
     C*          WTMXL - Edit Matrix Table
     C*
     C     MXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C*
     C*  FTGTL - GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTKEY
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQDN#         XXDN#
     C                   MOVE      QQDN#         P                 5 0
     C                   MOVE      *BLANK        XXKEY            10
     C*
     C                   ENDSR
     C**************************************************************************
     C*
     C*   ROUTINE - SRE001
     C*
     C     SRE001        BEGSR
     C*
     C     DN(P)         IFEQ      *BLANKS
     C     XXERRS        CAT       '001':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE002
     C*
     C     SRE002        BEGSR
     C*
     C                   MOVE      'TSET'        XXTBID
     C                   MOVEL(P)  DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '1'
     C     XXERRS        CAT       '002':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE003
     C*
     C     SRE003        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE004
     C*
     C     SRE004        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE005
     C*
     C     SRE005        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE006
     C*
     C     SRE006        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE007
     C*
     C     SRE007        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE008
     C*
     C     SRE008        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE009
     C*
     C     SRE009        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE010
     C*
     C     SRE010        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE011
     C*
     C     SRE011        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      'FRWP'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     DN(P)         XXTKEY
     C     GTKY          CHAIN     PGT01                              12
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '011':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE012
     C*
     C     SRE012        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C     DN(P)         ANDNE     'Y'
     C     DN(P)         ANDNE     'N'
     C     XXERRS        CAT       '012':0       XXERRS
     C                   END
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE013
     C*
     C     SRE013        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE014
     C*
     C     SRE014        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE015
     C*
     C     SRE015        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE016
     C*
     C     SRE016        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE017
     C*
     C     SRE017        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE018
     C*
     C     SRE018        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      DN(P)         X                 5 0
     C     X             IFLT      0
     C     X             ORGT      7
     C     XXERRS        CAT       '018':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE019
     C*
     C     SRE019        BEGSR
     C*
     C     DN(P)         IFNE      *BLANKS
     C                   MOVE      DN(P)         X
     C     X             IFLT      0
     C     X             ORGT      6
     C     XXERRS        CAT       '019':0       XXERRS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE020
     C*
     C     SRE020        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE021
     C*
     C     SRE021        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE022
     C*
     C     SRE022        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE023
     C*
     C     SRE023        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE024
     C*
     C     SRE024        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE025
     C*
     C     SRE025        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE026
     C*
     C     SRE026        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE027
     C*
     C     SRE027        BEGSR
     C*
     C                   MOVEL     DN(P)         XXWK1A
     C     XXWK1A        IFNE      *BLANK
     C     XXWK1A        ANDNE     '1'
     C     XXWK1A        ANDNE     '2'
     C     XXWK1A        ANDNE     '3'
     C     XXWK1A        ANDNE     '4'
     C     XXWK1A        ANDNE     '5'
     C     XXWK1A        ANDNE     '6'
     C     XXWK1A        ANDNE     '7'
     C     XXWK1A        ANDNE     '8'
     C     XXWK1A        ANDNE     '9'
     C     XXERRS        CAT       '027':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE028
     C*
     C     SRE028        BEGSR
     C*
     C                   MOVE      DN(P)         XXDVAL
     C     ' ':'0'       XLATE     XXDVAL        XXDVAL
     C                   TESTN                   XXDVAL               12
     C     *IN12         IFEQ      *OFF
     C     XXERRS        CAT       '028':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE029
     C*
     C     SRE029        BEGSR
     C*
     C                   MOVE      DN(P)         XXDAT8
     C     XXDAT8        IFNE      0
     C*
     C                   MOVE      XXDAT8        XXWK4A
     C                   MOVEL     XXWK4A        XTDAT8
     C                   MOVEL     XXDAT8        XXWK4A
     C                   MOVE      XXWK4A        XTDAT8
     C                   Z-ADD     XTDAT8        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XTDAT8
     C     SRERR         IFNE      ' '
     C                   MOVE      '1'           *IN12
     C                   ENDIF
     C*
     C     *IN12         IFEQ      *ON
     C     XXERRS        CAT       '029':0       XXERRS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE030
     C*
     C     SRE030        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE031
     C*
     C     SRE031        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE032
     C*
     C     SRE032        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE033
     C*
     C     SRE033        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE034
     C*
     C     SRE034        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE035
     C*
     C     SRE035        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE036
     C*
     C     SRE036        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE037
     C*
     C     SRE037        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE038
     C*
     C     SRE038        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE039
     C*
     C     SRE039        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE040
     C*
     C     SRE040        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE041
     C*
     C     SRE041        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE042
     C*
     C     SRE042        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE043
     C*
     C     SRE043        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE044
     C*
     C     SRE044        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE045
     C*
     C     SRE045        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE046
     C*
     C     SRE046        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE047
     C*
     C     SRE047        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE048
     C*
     C     SRE048        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE049
     C*
     C     SRE049        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE050
     C*
     C     SRE050        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE051
     C*
     C     SRE051        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE052
     C*
     C     SRE052        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE053
     C*
     C     SRE053        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE054
     C*
     C     SRE054        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE055
     C*
     C     SRE055        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE056
     C*
     C     SRE056        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE057
     C*
     C     SRE057        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE058
     C*
     C     SRE058        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE059
     C*
     C     SRE059        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE060
     C*
     C     SRE060        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE061
     C*
     C     SRE061        BEGSR
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE062
     C*
     C     SRE062        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE063
     C*
     C     SRE063        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE064
     C*
     C     SRE064        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE065
     C*
     C     SRE065        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE066
     C*
     C     SRE066        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE067
     C*
     C     SRE067        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE068
     C*
     C     SRE068        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE069
     C*
     C     SRE069        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE070
     C*
     C     SRE070        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE071
     C*
     C     SRE071        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE072
     C*
     C     SRE072        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE073
     C*
     C     SRE073        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE074
     C*
     C     SRE074        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE075
     C*
     C     SRE075        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE076
     C*
     C     SRE076        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE077
     C*
     C     SRE077        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE078
     C*
     C     SRE078        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE079
     C*
     C     SRE079        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE080
     C*
     C     SRE080        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE081
     C*
     C     SRE081        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE082
     C*
     C     SRE082        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE998
     C*
     C     SRE998        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*   ROUTINE - SRE999
     C*
     C     SRE999        BEGSR
     C*
     C*
     C                   ENDSR
