     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE S0015R - OPTION/FUNCTION SPECIAL AUTHORITY INFORMATION
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/12/11  C:235 PROJECT: WC1795                              *
     H*                  MAKE SECURITY LEVEL DIFF IN QUOTE/LIVE       *
     H*                                                               *
     H*  11/01/00  C:33  PROJECT: SY043                               *
     H*                  CHANGE PROGRAM ALLOW FOR MULTIPLE ENTRIES.   *
     H*****************************************************************
     FS0015F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSDOFP     UF A E             DISK
     F                                     RENAME(POF01:DBRCDF)
     F***  BEG   CHG  ***  01/12/11  *********************************
     F***SDOFL   IF  E           K        DISK
     FSDOFL1    IF   E           K DISK
     F                                     RENAME(POF01:POF03)
     F***  END   CHG  ***  01/12/11  *********************************
     F***  BEGIN ADD  ***  11/01/00  *********************************
     F                                     INFDS(OFFDS)
      /COPY SCOPYMT01D
     D*
     D*
     D                 DS
     D  OFTY                   1      6
     D                                     DIM(6)
     D  OFTY01                 1      1
     D  OFTY02                 2      2
     D  OFTY03                 3      3
     D  OFTY04                 4      4
     D  OFTY05                 5      5
     D  OFTY06                 6      6
     D*
     D OFFDS           DS
     D  OFRRN                397    400B 0
     D***  END   ADD  ***  11/01/00  *********************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Option/Function -
     D                                     Authority')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     OFKEY         KLIST
     C                   KFLD                    SCPGID
     C                   KFLD                    SCTYPE
     C                   KFLD                    SCNBR
     C***  BEG   ADD  ***  01/12/11  *********************************
     C     OFKEY1        KLIST
     C                   KFLD                    SCPGID
     C                   KFLD                    KKQTLV
     C                   KFLD                    SCTYPE
     C                   KFLD                    SCNBR
     C***  END   ADD  ***  01/12/11  *********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C     *LIKE         DEFINE    OFSEQ#        SVSEQ#
     C     *LIKE         DEFINE    SCQTLV        KKQTLV
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C     SCPGID        IFEQ      *BLANKS
     C                   SETON                                        9089
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C                   CALL      'SRCHKOBJ'                           99
     C                   PARM                    SCPGID
     C                   PARM      '*PGM'        ##OBJT            7
     C     *IN90         PARM                    ##ERR             1
     C     *IN90         IFEQ      *ON
     C                   SETON                                        84
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDNE     '1'
     C     SCTYPE        ANDNE     '2'
     C                   SETON                                        9088
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCNBR         ANDEQ     *ZEROS
     C                   SETON                                        9087
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDEQ     '2'
     C     SCNBR         ANDGT     22
     C                   SETON                                        9087
     C                   ENDIF
     C***  BEGIN DEL  ***  11/01/00  *********************************
     C***  BEG  ADD   ***  01/12/11  *********************************
     C     SCQTLV        IFEQ      *BLANKS
     C                   SETON                                        9044
     C                   ELSE
     C     SCQTLV        IFNE      'Q'
     C     SCQTLV        ANDNE     'L'
     C                   SETON                                        9046
     C                   ENDIF
     C                   ENDIF
     C****
     C     SCQTLV        IFEQ      'Q'
     C                   MOVEL     'L'           KKQTLV
     C     OFKEY1        CHAIN     SDOFL1                             99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9047
     C                   ENDIF
     C                   ENDIF
     C***  END  ADD******  01/12/11  *********************************
     C**********
     C********** *IN90     IFEQ *OFF
     C********** SCPGID    IFNE SHPGID
     C********** SCTYPE    ORNE SHTYPE
     C********** SCNBR     ORNE SHNBR
     C********** OFKEY     CHAINSDOFL                99
     C********** *IN99     IFEQ *OFF
     C**********           SETON                     9086
     C**********           ENDIF
     C**********           ENDIF
     C**********           ENDIF
     C***  END   DEL  ***  11/01/00  *********************************
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTY01        ANDEQ     *BLANKS
     C     SCTY02        ANDEQ     *BLANKS
     C     SCTY03        ANDEQ     *BLANKS
     C     SCTY04        ANDEQ     *BLANKS
     C     SCTY05        ANDEQ     *BLANKS
     C     SCTY06        ANDEQ     *BLANKS
     C     SCLVL         ANDEQ     *ZEROS
     C                   SETON                                        9085
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     F***  BEG   CHG  ***  01/12/11  *********************************
     C*****      OFKEY     CHAINSDOFL                99
     C                   MOVEL     SCQTLV        KKQTLV
     C     OFKEY1        CHAIN     SDOFL1                             99
     F***  END   CHG  ***  01/12/11  *********************************
     C*
     C     *IN90         DOWEQ     *OFF
     C     *IN99         ANDEQ     *OFF
     C*
     C     OFRRN         IFNE      @@RRN
     C*
     C     SCTY01        COMP      *BLANK                             7171
     C     SCTY02        COMP      *BLANK                             7272
     C     SCTY03        COMP      *BLANK                             7373
     C     SCTY04        COMP      *BLANK                             7474
     C     SCTY05        COMP      *BLANK                             7575
     C     SCTY06        COMP      *BLANK                             7676
     C*
     C   71SCTY01        LOOKUP    OFTY                                   71
     C   72SCTY02        LOOKUP    OFTY                                   72
     C   73SCTY03        LOOKUP    OFTY                                   73
     C   74SCTY04        LOOKUP    OFTY                                   74
     C   75SCTY05        LOOKUP    OFTY                                   75
     C   76SCTY06        LOOKUP    OFTY                                   76
     C*
     C     *IN71         IFEQ      *ON
     C     *IN72         OREQ      *ON
     C     *IN73         OREQ      *ON
     C     *IN74         OREQ      *ON
     C     *IN75         OREQ      *ON
     C     *IN76         OREQ      *ON
     C                   SETON                                        86  90
     C                   ENDIF                                                  *IN71=*ON
     C*
     C                   ENDIF                                                  OFRRN<>@@RRN
     C*
     F***  BEG   CHG  ***  01/12/11  *********************************
     C*****      OFKEY     READESDOFL                    99
     C     OFKEY1        READE     SDOFL1                                 99
     F***  END   CHG  ***  01/12/11  *********************************
     C*
     C                   ENDDO                                                  *IN90=*OFF
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C     CPYFLG        IFEQ      *OFF
     C                   MOVEL     OFPGID        SHPGID
     C                   MOVEL     OFTYPE        SHTYPE
     C                   MOVEL     OFNBR         SHNBR
     C                   ELSE
     C                   MOVEL     *BLANKS       SHPGID
     C                   MOVEL     *BLANKS       SHTYPE
     C                   MOVEL     *BLANKS       SHNBR
     C                   ENDIF
     C*
     C                   MOVEL     OFPGID        SCPGID
     C                   MOVEL     OFTYPE        SCTYPE
     C                   MOVEL     OFNBR         SCNBR
     C                   MOVEL     OFTY01        SCTY01
     C                   MOVEL     OFTY02        SCTY02
     C                   MOVEL     OFTY03        SCTY03
     C                   MOVEL     OFTY04        SCTY04
     C                   MOVEL     OFTY05        SCTY05
     C                   MOVEL     OFTY06        SCTY06
     C                   MOVEL     OFLVL         SCLVL
     C********BEG ADD**********01/12/11***********************************
     C                   MOVEL     OFQTLV        SCQTLV
     C********END ADD**********01/12/11***********************************
     C                   MOVEL     OFIUSR        SCIUSR
     C                   Z-ADD     OFIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIDT
     C                   MOVEL     OFITIM        SCITIM
     C                   MOVEL     OFUSER        SCUSR
     C                   Z-ADD     OFDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   MOVEL     OFTIME        SCTIME
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCPGID        OFPGID
     C                   MOVEL     SCTYPE        OFTYPE
     C                   MOVEL     SCNBR         OFNBR
     C                   MOVEL     SCTY01        OFTY01
     C                   MOVEL     SCTY02        OFTY02
     C                   MOVEL     SCTY03        OFTY03
     C                   MOVEL     SCTY04        OFTY04
     C                   MOVEL     SCTY05        OFTY05
     C                   MOVEL     SCTY06        OFTY06
     C                   MOVEL     SCLVL         OFLVL
     C********BEG ADD**********01/12/11***********************************
     C                   MOVEL     SCQTLV        OFQTLV
     C********END ADD**********01/12/11***********************************
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         OFPGM
     C                   MOVEL     Q1USER        OFUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OFDT
     C                   MOVEL     DSTIME        OFTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   CLEAR                   OFSEQ#
     C***  BEG  CHG******  01/12/11  *********************************
     C*******    OFKEY     SETGTSDOFL
     C*******    OFKEY     REDPESDOFL                    99
     C     OFKEY1        SETGT     SDOFL1
     C     OFKEY1        READPE    SDOFL1                                 99
     C***  END  CHG******  01/12/11  *********************************
     C     OFSEQ#        ADD       1             SVSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   MOVE      SVSEQ#        OFSEQ#
     C***  END   ADD  ***  11/01/00  *********************************
     C                   MOVE      'A'           OFSTAT
     C                   MOVEL     Q1PGM         OFIPGM
     C                   MOVEL     Q1USER        OFIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OFIDT
     C                   MOVEL     DSTIME        OFITIM
     C                   WRITE     DBRCDF
     C                   FEOD      SDOFP
     C                   ENDSR
