     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*
     H*  F0400R - PROVIDER BILLS WORK WITH ERRORS MAINTENANCE         *
     H*                                                               *
     H*  CREATED: 03/02/07
     H*           C:188 PROJECT: 10MJC - EDI EDITS (P2P IMPORT)
     H*                                                               *
     H*  CALLED FROM: F0400R                                          *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0400F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FFAEOP     IF   E             DISK
     F*
     FFDELP     UF   E             DISK
     F                                     INFDS(DBFDSA)
     F                                     RENAME(PEL01:DBRCDF)
     FFTFLL     IF   E           K DISK
     F*
     FFTEDL     IF   E           K DISK
     F*
     F*  WCMC NDC Drug Code File
     FFMRXL     IF   E           K DISK
     FFMRXP     IF   E             DISK
     F                                     RENAME(PRX01:PRX02)
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSD          DS
     D  DBRRND               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSG          DS
     D  DBRRNG               397    400B 0
     D DBFDSZ          DS
     D  DBRRNZ               397    400B 0
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D                 DS
     D  ##NUM                  1     80
     D  XXNUM                  6      9
     D*
     D                 DS
     D  XADATA                 1     40
     D  XALIN#                 1      4
     D  XAFDAT                 6     15
     D  XACPT4                17     28
     D  XAPMOD                30     31
     D  XADESC                33     40
     D*
     D                 DS
     D  XBDATA                 1     40
     D  XBFDAT                 1     10
     D  XBLIN#                12     15
     D  XBCPT4                17     28
     D  XBPMOD                30     31
     D  XBDESC                33     40
     D*
     D                 DS
     D  XCDATA                 1     40
     D  XCCPT4                 1     12
     D  XCPMOD                14     15
     D  XCDESC                17     24
     D  XCLIN#                26     29
     D  XCFDAT                31     40
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Errors')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D SLMSG1          C                   CONST('MC40081')
     D SLMSG2          C                   CONST('FSMESG')
     D CHGNUM          C                   CONST('SA020R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D DELETE          C                   CONST('** Delete  **')
     D MRAADJ          C                   CONST('MRA adjustment')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D #OFSCR          C                   CONST(1)
     D*================================================================
     D*  Program specific constants
     D*================================================================
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D CANCEL          C                   CONST('12F12=Cancel        -
     D                                            YYY')
     D*** BEGIN ADD *** 07/23/99 **************************************
     D NETWRK          C                   CONST('077=Network Info    -
     D                                            YYY')
     D*** END ADD *** 07/23/99 ****************************************
     D PCERTF          C                   CONST('18F18=View Pre-Cert -
     D                                            YYY')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          faeop
     C                   PARM                    @@RRN2            9 0          fdelp
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     RXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNDC            11
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY1
     C*
     C     GTKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ELNVAL        HLNVAL
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY1
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C*
     C* CONVERT @@CALL
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'A'
     C     @@MODE        OREQ      'C'
     C                   MOVEL     ' '           @@CALL            1
     C*
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL     '1'           @@CALL
     C*
     C                   ENDSL
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C*
     C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
     C* FORMAT ALL DATES ON SCREEN
     C*
     C                   ENDSR
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
     C*                    EXSR WRKFLW
     C     ##KL          IFEQ      '1'
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKC         ANDNE     *ON
     C*
     C** NEW VALUE CANNOT BE BLANKS
     C*
     C* INPUT FIELD AS DATE
     C*
     C     *IN52         IFEQ      *ON
     C     SCNDAT        IFEQ      *ZEROS
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SCNDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C*
     C* INPUT FIELD AS TEXT FIELD
     C*
     C     *IN51         IFEQ      *ON
     C     SCNVAL        ANDEQ     *BLANKS
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ELSE
     C*
     C* INPUT FIELD AS NUMERIC
     C*
     C     *IN50         IFEQ      *ON
     C     SCNVA#        ANDEQ     *ZERO
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  NVAL EQ BLANKS
     C                   ENDIF
     C                   ENDIF                                                  NDC VALIDATION
     C*
     C** IF NEW VALUE IS FOR AN NDC FIELD, LOOKUP
     C*
     C     SCFLD         IFEQ      'DPNDC'
     C     SCFLD         OREQ      'DHNDC'
     C                   MOVEL(P)  SCNVAL        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C*          *IN46     ANDEQ*OFF
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CK1END        ENDSR
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETON                                        90
     C*
     C     @@MODE        IFNE      'D'
     C                   CLEAR                   HLNVAL
     C                   SELECT
     C*
     C**  NEW VALUE FIELD SEARCH BASED ON VALUE OF SCFLD
     C*
     C     HFLD          WHENEQ    'SCNVAL'
     C*
     C* PROC CODE
     C*
     C                   SELECT
     C     SCFLD         WHENEQ    'DHPRCD'
     C                   MOVE      SCNVAL        HLNVAL
     C                   MOVE      *BLANKS       ##PROC            5
     C                   MOVE      *BLANKS       ##PROD           50
     C                   Z-ADD     0             ##RRN1            9 0
     C                   MOVE      EOAST         ##AST
     C                   CALL      'F2013R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1
     C                   PARM                    ##PROC
     C                   PARM                    ##PROD
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    ##AST
     C     ##RTCD        IFEQ      '1'
     C     ##PROD        OREQ      *BLANKS
     C                   MOVEL     HLNVAL        SCNVAL
     C                   ELSE
     C                   MOVEL(P)  ##PROC        SCNVAL
     C                   ENDIF                                                  ##RTCD = 1
     C*
     C* PLACE OF SERVICE
     C*
     C     SCFLD         WHENEQ    'DHPOS'
     C     SCFLD         OREQ      'DPPOS'
     C     SCFLD         OREQ      'DUPOS'
     C     SCFLD         OREQ      'DDPOS'
     C                   MOVEL     SCNVAL        HLNVAL
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##POS
     C                   CALL      'F2370R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##AST             2
     C                   PARM                    ##POS            15
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##POS         OREQ      *BLANKS
     C                   MOVEL(P)  HLNVAL        SCNVAL
     C                   ELSE
     C                   MOVE      ##POS         SCNVAL
     C                   ENDIF
     C*
     C* MODIFIERS
     C*
     C     SCFLD         WHENEQ    'DHMOD1'
     C     SCFLD         OREQ      'DHMOD2'
     C     SCFLD         OREQ      'DHMOD3'
     C     SCFLD         OREQ      'DHMOD4'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCNVAL        HLNVAL
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLNVAL        SCNVAL
     C                   ELSE
     C                   MOVEL     ##KEY         SCNVAL
     C                   ENDIF
     C*
     C     SCFLD         WHENEQ    'DURATE'
     C*
     C     SCFLD         WHENEQ    'DURVCD'
     C                   MOVEL     SCNVAL        HLNVAL
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      *BLANKS       ##CMSV           15
     C                   MOVE      *BLANKS       ##CDSC           40
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C     ##KL          IFEQ      '1'
     C     ##CMSV        OREQ      *BLANKS
     C                   MOVEL     HLNVAL        SCNVAL
     C                   ENDIF                                                  ##KL = '1'
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCNVAL
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C     SCFLD         WHENEQ    'DUPMOD'
     C                   MOVEL(P)  SCNVAL        HLNVAL
     C                   MOVE      EOAST         #2AST
     C                   CALL      'F2315R'
     C                   PARM      '2'           #2CALL            1
     C                   PARM      *BLANK        #2KC              1
     C                   PARM      *BLANK        #2KL              1
     C                   PARM                    #2AST             2
     C                   PARM      *BLANKS       #2KEY            15
     C     #2KEY         IFEQ      *BLANKS
     C                   MOVEL(P)  HLNVAL        SCNVAL
     C                   ELSE
     C                   MOVEL     #2KEY         SCNVAL
     C                   ENDIF
     C*
     C     SCFLD         WHENEQ    'DPNDC'
     C     SCFLD         OREQ      'DHNDC'
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2350R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNVAL        SCNVAL
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCNVAL
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
     C*
     C     SCFLD         WHENEQ    'DPRXPT'
     C                   MOVEL(P)  SCNVAL        HLNVAL
     C                   MOVEL(P)  'FLPH'        ##TBID
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##TBID
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNVAL        SCNVAL
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCNVAL
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C*
     C     SCFLD         WHENEQ    'DPCERT'
     C                   MOVEL(P)  SCNVAL        HLNVAL
     C                   MOVEL(P)  'FLCT'        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCNVAL
     C                   ENDIF                                                  99 = OFF
     C                   ENDIF                                                  99 = OFF
     C                   ELSE                                                   *IN99 = *OFF
     C                   MOVEL     HLNVAL        SCNVAL
     C                   ENDIF                                                  99 = OFF
     C*
     C     SCFLD         WHENEQ    'DPNWRF'
     C                   MOVEL(P)  SCNVAL        HLNVAL
     C                   MOVEL(P)  'FLFI'        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCNVAL
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  99 = OFF
     C                   ELSE
     C                   MOVEL     GTKEY         HLNVAL
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ENDSL
     C*
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C**
     C                   ENDIF
     C**
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     @@RRN2        CHAIN     DBRCDF                             99
     C*
     C                   MOVEL(P)  ELLIN#        ##NUM
     C                   MOVE(P)   *BLANKS       ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVE      XXNUM         SCLIN#
     C*
     C                   MOVE      ELFLD         SCFLD
     C*
     C* GET FIELD DESCRIPTION
     C*
     C     ELFLD         CHAIN     PFL01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FLDESC        SCFLDD
     C                   ENDIF
     C*
     C                   MOVE      ELCODE        SCCODE
     C*
     C* GET CODE DESCRIPTION
     C*
     C     ELCODE        CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     EDDESC        SCDESC
     C                   ENDIF
     C*
     C* CHECK IF FIELD DATE, NUMERIC, OR CHAR
     C*
     C     1             SUBST     ELCODE:1      CHAR1             1
     C*
     C     CHAR1         IFEQ      'D'                                          is date
     C                   SETON                                        52
     C                   SETOFF                                       5051
     C                   MOVE      ELVALU        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVDAT
     C     ELNVAL        IFNE      *BLANKS
     C                   MOVE      ELNVAL        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCNDAT
     C                   ENDIF
     C                   ELSE
     C*
     C     ELFLD         IFEQ      'CACHRG'                                     numeric
     C     ELFLD         OREQ      'CAUNTS'
     C     ELFLD         OREQ      'DPRXDY'
     C                   SETON                                        50
     C                   SETOFF                                       5152
     C                   MOVE      ELVALU        SCVAL#
     C                   MOVE      ELNVAL        SCNVA#
     C                   ELSE
     C*
     C                   SETON                                        51
     C                   SETOFF                                       5052
     C                   MOVEL     ELVALU        SCVALU
     C                   MOVEL     ELNVAL        SCNVAL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* IF THIS A DATE FIELD (POS 1 OF ELCODE='D') THEN FORMAT FOR SAVE
     C*
     C     *IN52         IFEQ      *ON                                          is date
     C                   MOVEL(P)  SCNDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WK7               7
     C                   MOVE(P)   WK7           ELNVAL
     C                   ELSE
     C*
     C     *IN51         IFEQ      *ON                                          is char
     C                   MOVEL     SCNVAL        ELNVAL
     C                   ELSE
     C*
     C     *IN50         IFEQ      *ON                                          is num
     C                   MOVE      SCNVA#        ELNVAL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C* SETOF INDICATORS FOR ERROR CHECKING:
     C*
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* IF IN DISPLAY MODE, PROTECT ALL FIELDS
     C*
     C     @@MODE        IFEQ      'D'
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC
     C                   PARM      *BLANK        ##OS              1            OUTCOME/SAVINGS
     C                   PARM                    ##ARLS            1            Auto Release Flag
     C                   PARM                    ##CCOD            1            Correct Coding Flag
     C                   PARM                    ##CRVW            1            Code Review Flag
     C                   PARM                    ##PROM            1            ProMatch Flag
     C                   PARM                    ##MCRV            1            Medicare CR Flag
     C                   PARM                    ##FREQ            1            Frequency Flag
     C                   PARM                    ##CRAC            1            CR Auto Complete
     C                   PARM                    ##PREO            1            CR Auto Complete
     C                   PARM                    ##PARN            1            Par/Non-Par/Both
     C                   PARM                    ##MGRI            1            Multi Group Restr
     C                   PARM                    ##KCIF            1            Keep Claimant info
     C                   PARM                    ##CURS            1            Cursor Positioning
     C                   PARM                    @@PCIB            1            Prov Check in Batch
     C                   PARM                    ##CLT#            3            Client Number
     C                   PARM                    ##UCPF            1            UCR Percentile
     C                   PARM                    ##DCF             1            Data Center
     C                   PARM                    ##AUTB            1            Auto Batch   Flag
     C                   PARM                    ##DEIF            1            DE/Image Flag
     C                   PARM                    ##AURE            1            Auto-Reinstate
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C*
     C                   SELECT
     C*
     C     ##PRIV        WHENEQ    '1'
     C                   Z-ADD     1             #1PRIV            1 0
     C*
     C     ##PRIV        WHENEQ    '2'
     C                   Z-ADD     2             #1PRIV
     C*
     C     ##PRIV        WHENEQ    '3'
     C                   Z-ADD     2             #1PRIV
     C*
     C                   OTHER
     C                   Z-ADD     1             #1PRIV
     C*
     C                   ENDSL
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   UPDATE    DBRCDF
     C*
     C     UPEND         ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* WRKFLW - Call Workflow Program
     C*================================================================
     C*          WRKFLW    BEGSR
     C*
     C** CHANGE USER PARMS
     C*
     C*                    MOVE EOCO#     #2CO#
     C*                    MOVE EOSUB#    #2SUB#
     C*                    MOVE EOFND     #2FND
     C*                    CALL 'F0006R2'
     C*                    PARM           #2CO#
     C*                    PARM           #2SUB#
     C*                    PARM           #2FND
     C*                    PARM ' '       ##RTCD
     C*
     C* WORKFLOW
     C*
     C*                    MOVE EOAST     ##AST
     C*                    MOVE EOSYS     ##SYS
     C*                    MOVE EOBTYP    ##BTYP
     C*                    MOVE EOSTYP    ##STYP
     C*                    MOVE @@MODE    ##MODE  1
     C*                    MOVE EOSUB#    KKSUB#
     C*                    MOVE EOFND     KKFND
     C*                    MOVE EOFYR     KKFYR
     C*                    MOVE EOCASE    KKCASE
     C*                    MOVE EOEOB#    KKEOB#
     C*                    MOVE EOCHRG    ##CHRG
     C*                    MOVE HLCHRG    ##HCHG  9
     C*
     C* GET HEADER AND DETAIL INFORMATION IF NOT ADD MODE
     C*
     C*                    Z-ADD0         ##RRN
     C*                    Z-ADD0         ##RRN3
     C*          @@MODE    IFNE 'A'
     C*
     C*          EOSYS     IFEQ 'P'
     C*          EOBTYP    ANDEQ'N'                        Physician
     C*
     C*** BEG ADD *** 01/18/99 ****************************************
     C*          EOSYS     OREQ 'V'
     C*          EOBTYP    ANDEQ'V'                        Physician
     C*** END ADD *** 01/18/99 ****************************************
     C*
     C*          EHKY      CHAINPEH01                99    Header
     C* N99                Z-ADDDBRRN2    ##RRN
     C*          DHKY      CHAINPDH01                99    Detail
     C* N99                Z-ADDDBRRN3    ##RRN3  90
     C*                    ENDIF                           EOSYS = 'P'
     C*
     C*          EOSYS     IFEQ 'H'
     C*          EOBTYP    ANDEQ'H'                        Hospital
     C*          EUKY      CHAINPEU01                99    Header
     C* N99                Z-ADDDBRRN5    ##RRN
     C*          DUKY      CHAINPDU01                99    Detail
     C* N99                Z-ADDDBRRN6    ##RRN3  90
     C*                    ENDIF                           EOSYS = 'H'
     C*
     C*          EOSYS     IFEQ 'P'
     C*          EOBTYP    ANDEQ'X'                        pharmacy
     C*          EPKY      CHAINPEP01                99    header
     C* N99                Z-ADDDBRRN7    ##RRN
     C*          DPKY      CHAINPDP01                99    detail
     C* N99                Z-ADDDBRRN8    ##RRN3
     C*                    ENDIF                           EOSYS = 'P'
     C*
     C*** BEG ADD *** 06/19/00 ****************************************
     C*          EOSYS     IFEQ 'P'
     C*          EOBTYP    ANDEQ'D'                        dental
     C*          EDKY      CHAINPED01                99    header
     C* N99                Z-ADDDBRRN9    ##RRN
     C*          DDKY      CHAINPDD01                99    detail
     C* N99                Z-ADDDBRRN0    ##RRN3
     C*                    ENDIF                           EOSYS = 'P'
     C*** END ADD *** 06/19/00 ****************************************
     C*
     C*                    ENDIF                           @@MODE IFNE 'A'
     C*
     C* GET FAEOP RELATIVE RECORD NUMBER
     C*
     C*          @@MODE    IFEQ 'A'
     C*                    Z-ADDDBRRN4    ##RRN2
     C*                    ELSE
     C*                    Z-ADD@@RRN     ##RRN2  90       FAEOP
     C*                    ENDIF                           @@MODE IFEQ A
     C*
     C*                    MOVEL'FTABL'   ##FIL1 10 P
     C*                    MOVEL'FMPRL'   ##FIL2 10 P
     C*                    MOVEL'FMSVP'   ##FIL3 10 P
     C*                    MOVEL'FMSVL1'  ##FIL4 10 P
     C*                    MOVEL'FMSVL2'  ##FIL5 10 P
     C*                    MOVEL'FTPSL'   ##FIL6 10 P
     C*                    MOVEL'FMPRP'   ##FIL7 10 P
     C*** BEG ADD *** 09/28/04 ****************************************
     C*                    MOVEL'FAAGP'   ##FIL8 10 P
     C*** END ADD *** 09/28/04 ****************************************
     C*
     C*                    CALL 'F0350C01'
     C*                    PARM           ##AST
     C*                    PARM           ##SYS
     C*                    PARM           ##BTYP
     C*                    PARM           ##STYP
     C*                    PARM ' '       ##KC
     C*                    PARM ' '       ##KL
     C*                    PARM           ##MODE
     C*                    PARM           ##RRN            header phys/hosp
     C*                    PARM           ##RRN2           faeop
     C*                    PARM           ##RRN3           deatil phys/hosp
     C*                    PARM           ##FIL1
     C*                    PARM           ##FIL2
     C*                    PARM           ##FIL3
     C*                    PARM           ##NWCD
     C*                    PARM           ##CHRG
     C*                    PARM           ##FIL4
     C*                    PARM           ##FIL5
     C*                    PARM           ##FIL6
     C*                    PARM           ##FIL7
     C*** BEG ADD *** 09/10/99 ****************************************
     C*                    PARM           XXEDI
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 08/04/00 ****************************************
     C*                    PARM           ##HCHG
     C*** END ADD *** 08/04/00 ****************************************
     C*** BEG ADD *** 09/28/04 ****************************************
     C*                    PARM           ##FIL8
     C*** END ADD *** 09/28/04 ****************************************
     C*
     C*                    ENDSR
