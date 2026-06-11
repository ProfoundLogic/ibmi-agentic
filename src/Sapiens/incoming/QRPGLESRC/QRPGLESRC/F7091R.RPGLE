     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H* PROGRAM: F7091R
     H* DESCRIPTION: CASE TRANSFER/DELETION SELECTION PROGRAM
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H*
     H*  REVISIONS:
     H*
     H* 12/18/24 C:342 Project : MC447 Phase 2
     H*                1.F10 Search disabled on the Group Field
     H*                DSPF Changes:
     H*                1.Changed Existing Group field into Typeaheadd
     H*
     H*****************************************************************
     H*
     FF7091F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFD2)
     F                                     RENAME(PCM01:PCM03)
     FFMSML     IF   E           K DISK
     F                                     INFDS(DBFD3)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFD4)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFD2           DS
     D  DBRRN2               397    400B 0
     D DBFD3           DS
     D  DBRRN3               397    400B 0
     D DBFD4           DS
     D  DBRRN4               397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Case Transfer/Deleti-
     D                                     on Decision prompt')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@TD              1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     SMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSUB#        X1SUB#
     C     *LIKE         DEFINE    CMFND         X1FND
     C     *LIKE         DEFINE    CMFYR         X1FYR
     C     *LIKE         DEFINE    CMCASE        X1CASE
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
     C*
     C                   SETOFF                                       7273
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C     SCFND         ANDEQ     *ZEROS
     C     SCFYR         ANDEQ     *ZEROS
     C     SCCASE        ANDEQ     *ZEROS
     C                   MOVE      *BLANKS       SCSNAM
     C                   MOVE      *BLANKS       SCFNAM
     C                   MOVE      *BLANKS       SCCNAM
     C                   SETOFF                                       61
     C                   MOVE      'X'           XXFLAG            1
     C                   ELSE
     C     SCSUB#        IFEQ      X1SUB#
     C     SCFND         ANDEQ     X1FND
     C     SCFYR         ANDEQ     X1FYR
     C     SCCASE        ANDEQ     X1CASE
     C                   SETON                                        9073
     C                   GOTO      ENDCK1
     C                   ELSE
     C                   MOVE      'Y'           XXFLAG
     C                   ENDIF
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   SETON                                        9081
     C                   GOTO      ENDCK1
     C                   ELSE
     C                   MOVE      SCSUB#        XXSUB#
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9082
     C                   GOTO      ENDCK1
     C                   ELSE
     C                   Z-ADD     DBRRN3        ##RRN1            9 0
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      SCSUB#        XXSUB#
     C                   MOVEL     SCFND         XXFND
     C*
     C     SCFND         IFEQ      *ZEROS
     C                   SETON                                        9084
     C                   GOTO      ENDCK1
     C                   ELSE
     C                   MOVEL     SCSUB#        XXSUB#
     C                   MOVEL     SCFND         XXFND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9085
     C                   GOTO      ENDCK1
     C                   ELSE
     C                   Z-ADD     DBRRN4        ##RRN2            9 0
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCFYR         IFEQ      *ZEROS
     C                   SETON                                        9088
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C     SCCASE        IFEQ      *ZEROS
     C                   SETON                                        9092
     C                   GOTO      ENDCK1
     C*
     C                   ELSE
     C*
     C                   MOVEL     CMCO#         XXCO#
     C                   MOVEL     SCSUB#        XXSUB#
     C                   MOVEL     SCFND         XXFND
     C                   MOVEL     SCFYR         XXFYR
     C                   MOVEL     SCCASE        XXCASE
     C*
     C     CMKY          CHAIN     PCM03                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9072
     C                   ELSE
     C                   Z-ADD     DBRRN2        ##RRN3            9 0
     C*
     C     CMLSN         CAT(P)    ',':0         XACLNM           30
     C                   CAT       CMFSN:1       XACLNM
     C     CMMID         IFNE      *BLANK
     C                   CAT       CMMID:1       XACLNM
     C                   CAT       '.':0         XACLNM
     C                   ENDIF
     C                   MOVEL     XACLNM        SCCNAM
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ENDCK1        ENDSR
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
     C*** SUBSCRIBER SEARCH
     C*
     C                   Z-ADD     *ZEROS        XXSUB#
     C                   Z-ADD     *ZEROS        XXFND
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##RRN1            9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN1        OREQ      *ZEROS
     C                   SETON                                        8390
     C                   ELSE
     C*
     C     ##RRN1        CHAIN     PSM02                              99
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   SETON                                        869061
     C                   ENDIF
     C*
     C*** FUND SEARCH ***
     C*
     C     HFLD          WHENEQ    'SCFND'
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   SETON                                        9081
     C                   ELSE
     C                   MOVE      SCSUB#        XXSUB#
     C                   ENDIF
     C*
     C******** Beg Del ***** 12/18/24 ***************************
     C****               CALL      'F0060R'
     C****               PARM      '2'           ##CALL            1
     C**** *INKC         PARM      '0'           ##KC              1
     C**** *INKL         PARM      '0'           ##KL              1
     C****               PARM                    ##RRN2            9 0
     C****
     C**** *INKC         IFEQ      *ON
     C****               MOVE      '1'           OUTFLG
     C****               ENDIF
     C****
     C**** *INKL         IFEQ      *ON
     C**** ##RRN2        OREQ      *ZEROS
     C****               SETON                                        8690
     C****               ELSE
     C****
     C**** ##RRN2        CHAIN     PFN02                              99
     C****
     C****               MOVEL(P)  FNFND         SCFND
     C****               MOVEL(P)  FNNAME        SCFNAM
     C****               SETON                                        899061
     C****               ENDIF
     C******** End Del ***** 12/18/24 ***************************
     C*
     C*** FYR AND CASE SEARCH
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   MOVEL(P)  'F0269R'      #FNAME           10
     C                   MOVEL(P)  'F0269Q'      #TNAME           10
     C*
     C                   CALL      'FA711C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C     ##RTN         IFEQ      'A'
     C                   SETON                                        9071
     C                   ELSE
     C*
     C                   CALL      'F0269Q'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##SUB#            5 0
     C                   PARM      *ZEROS        ##FND             3 0
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   MOVEL     ##SUB#        SCSUB#
     C                   MOVEL     ##FND         SCFND
     C                   MOVEL     ##FYR         SCFYR
     C                   MOVEL     ##CASE        SCCASE
     C*
     C     SCCASE        IFEQ      *ZEROS
     C                   SETON                                        90
     C                   ELSE
     C*
     C                   MOVE      ##SUB#        XXSUB#
     C     SMKY          CHAIN     PSM01                              99
     C                   Z-ADD     DBRRN3        ##RRN1            9 0
     C                   MOVEL(P)  SMNAME        SCSNAM
     C*
     C                   MOVEL     ##FND         XXFND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C                   Z-ADD     DBRRN4        ##RRN2            9 0
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   SETON                                        906191
     C*
     C                   MOVEL     #1CO#         XXCO#
     C                   MOVEL     ##SUB#        XXSUB#
     C                   MOVEL     ##FND         XXFND
     C                   MOVEL     ##FYR         XXFYR
     C                   MOVEL     ##CASE        XXCASE
     C*
     C     CMKY          CHAIN     PCM03                              99
     C                   Z-ADD     DBRRN2        ##RRN3            9 0
     C*
     C     CMLSN         CAT(P)    ',':0         XACLNM           30
     C                   CAT       CMFSN:1       XACLNM
     C     CMMID         IFNE      *BLANK
     C                   CAT       CMMID:1       XACLNM
     C                   CAT       '.':0         XACLNM
     C                   ENDIF
     C                   MOVEL     XACLNM        SCCNAM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   Z-ADD     *ZEROS        SCFND
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVE      *BLANKS       SCSNAM
     C                   MOVE      *BLANKS       SCFNAM
     C                   MOVE      *BLANKS       SCCNAM
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
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
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   SETOFF                                       1011
     C     @@TD          IFEQ      'T'
     C                   SETON                                        10
     C                   ELSE
     C                   SETON                                        11
     C                   ENDIF
     C*
     C** Get standard screen headings
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   MOVE      CMCO#         XXCO#
     C                   MOVE      CMSUB#        XXSUB#
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
     C*
     C                   MOVE      CMSUB#        X1SUB#
     C                   MOVE      CMFND         X1FND
     C                   MOVE      CMFYR         X1FYR
     C                   MOVE      CMCASE        X1CASE
     C*
     C                   MOVEL(P)  'FAT3PQ'      #FNAME
     C                   MOVEL(P)  'FAT3PQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C                   MOVEL(P)  'FAT3LQ'      #FNAME
     C                   MOVEL(P)  'FAT3LQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
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
     C                   UPDATE    DBRCDF
     C*
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@RRN         ##RRN4            9 0
     C                   PARM      ##RRN1        ##RRN5            9 0
     C                   PARM      ##RRN2        ##RRN6            9 0
     C                   PARM      ##RRN3        ##RRN7            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      XXFLAG        ##FLAG            1
     C                   PARM      'N'           ##WCMS            1
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *OFF          *INKL
     C                   SETON                                        90
     C                   ENDIF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMCMP
     C*                    MOVE *ON       OUTFLG
     C                   ENDSR
