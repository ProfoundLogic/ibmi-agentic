     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H bnddir('FTBNDDIR')
     H*****************************************************************
     H*****************************************************************
     H/TITLE N0037R01 - PROVIDER NETWORK CONTRACT INFORMATION
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays the Provider Network  **
     H**     Contract file.                                          **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     N0037R - Work with Network Contracts                    **
     H**   CALLS TO:                                                 **
     H**     F2493R   - Generic Table Search                         **
     H**     N1410R   - Prov Disc Code Srch                          **
     H**     N2325R   - Work With Network Master Records             **
     H**     SRCVTDAT - Convert date using CVTDAT command            **
     H**     SRPAST8  - Calculate a date in the past                 **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   01/15/98      PROJECT: 60MBS                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:                                                  *
     H*                                                              *
     H*  02/14/23 C:079 Support: 114/107109                          *
     H*                 Display correct description                  *
     H*                                                              *
     H*  06/06/19 C:180 Project: MC432                               *
     H*                 Replace N91D7R with FPINETDISC.              *
     H*                                                              *
     H*  02/21/08 C:183 Project: 114MPX                              *
     H*                 Call FA754R to write Tracking Record.        *
     H*                                                              *
     H*  11/07/03 C:79 PROJ:MC256
     H*                ADD F8=ADDITIONAL DETAIL INFO
     H*
     H*  05/19/03 C:62 PROJ:NW076
     H*                IGNORE INACTIVE CONTRACTS IN DATE EDITS.
     H*
     H*  03/21/02 C:62 PROJ:MC114 31749
     H*                DON'T ALLOW ENTRY OF DISCOUNT CODE IF MANUAL
     H*                FEE SCHEDULE FLAG IS 'Y'
     H*
     H*  07/26/01 C:62 PROJ:NW073  BASE ONLY
     H*                - ADD MODE: KEEP FROM ADDING IF OVERLAPS OTHER
     H*                  NETWORK
     H*
     H*  03/23/01 C:62 PROJ:NWMNT LOG#:165  BASE ONLY
     H*                -ADD F7=DISC RATE CODE INFO.
     H*                -TIGHTEN EDIT ON DISCOUNT CODE, SO THAT IF THE
     H*                 MANUAL FEE SCHEDULE FLAG IS "N", THEN REQUIRE
     H*                 THE ENTRY OF A VALID DISCOUNT CODE.
     H*
     H*  10/31/00 C:62 PROJ:NW068
     H*                SWAP OUT OLD SEARCH N1410R WITH NEW SEARCH.
     H*
     H*  01/11/00 C:62 PROJ:NW012
     H*                MAKE MFS TO BE MUTUALLY EXCLUSIVE.
     H*
     H*  03/09/98 C:62  Project: MC159
     H*                 Provider/Network Rewrite
     H*                 1. Update end date when when new begin date
     H*                    for contract is entered.
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions       **
     H**      40   Add mode                                          **
     H**      41   Position cursor at SCNTWK                         **
     H**      42   Position cursor at SCNINT                         **
     H**      43   Position cursor at SCSYS                          **
     H**      44   Position cursor at SCSTYP                         **
     H**      45   Position cursor at SCBGDT                         **
     H**      46   Position cursor at SCDISC                         **
     H**      47   Position cursor at SCDISP                         **
     H**      60   Used for chaining                                 **
     H**      71   NWCD - MCR0443: Invalid network code              **
     H**      72   SYS  - MCR0460: Invalid system type               **
     H**      73   STYP - MCR2383: Invalid value entered             **
     H**      74   BGDT - MCR0500: Invalid date                      **
     H**      75   BGDT - MCR0312: Term date can not be < start date **
     H**           ENDT - MCR0312: Term date can not be < start date **
     H**      76   ENDT - MCR0500: Invalid date                      **
     H**      77   DISC - MCR0313: Invalid network discount          **
     H**      79   FSFL - MCR0501: Value mut be 'Y' or 'N'           **
     H**      80   ECFL - MCR0501: Value mut be 'Y' or 'N'           **
     H**      81   MDFL - MCR0501: Value mut be 'Y' or 'N'           **
     H**      82          MCR0676: F10 used on non searchable field  **
     H**      83          MCR0900: Record already exists             **
     H**      84 - 89 are reserved for error screen error indicators **
     H**      90   Used as general purpose error indicator           **
     H**      97   Used when function key pressed                    **
     H**      99   Used when chaining to file being updated          **
     H**                                                             **
     H*****************************************************************
     H**                                                             **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTGTL  (PGT01)  Generic Table File (CO#/TBID/KEY)      **
     H**      FMS4P  (DBRCDF) Provider Network Contract              **
     H**      FMS4L2 (PS402)  Provider Network Contract              **
     H**                      (FEIN/MOD/NWCD/BGDT) Select SYS = 'H'  **
     H**      FMS4L3 (PS403)  Provider Network Contract              **
     H**                      (FEIN/MOD/NWCD/BGDT) Select SYS = 'P'  **
     H**      FMPDL  (PPD01)  Provider Naster File (FEIN/MOD)        **
     H**      NMBDLD (PBD01)  Billing Rate/Biscount (CODE/RCDT = 'D' **
     H**      NMNWP  (PNW01)  PPO Network File                       **
     H**      NMNWL  (PNW02)  PPO Network File (CO#/NWCD)            **
     H*****************************************************************
     H*
     FN0037F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTGTL     IF   E           K DISK
     FFMS4P     UF A E             DISK
     F                                     RENAME(PS401:DBRCDF)
     FFMS4L2    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F                                     INFDS(DBFDS2)
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS403)
     F                                     INFDS(DBFDS3)
     FFMPDL     IF   E           K DISK
     FNMBDLD    IF   E           K DISK
     FNMNWP     IF   E             DISK
     FNMNWL     IF   E           K DISK
     F                                     RENAME(PNW01:PNW02)
     FNMBDP     IF   E             DISK
     F                                     RENAME(PBD01:PBD02)
      /COPY SCOPYMT01D
     D PGM             S             70    DIM(2) CTDATA PERRCD(1)              Case status   s
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*
     D                 DS
     D  XXCMD                  1    140
     D  XXTYPE                 9     10
     D  XXPRDS                19     58
     D  XXCALL                62     62
     D  XXCODE                72     74
     D  XXEFDT                78     85
     d networkDisc     pr
     d                                2    const
     d                               40    const
     d                                1    const
     d                                3    const
     d                                8    const
     d                                9    const options(*nopass)
     d                                5    const options(*nopass)
      *
     d QQefdt          S              8A   INZ
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Network Con-
     D                                     tract Information')
     D OPTN2           C                   CONST('Discount Code -
     D                                     Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D ADDED           C                   CONST('Contract was added')


     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    ##STCD            2
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
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
     C*
     C*          FMS4L  (DBRCD2) Provider Network Contract (FEIN/MOD)
     C     KEY1          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*          FMS4L6 (PS402)  Provider Network Contract
     C*                          (FEIN/MOD/NWCD/CNT#/SYS/BGDT)
     C*
     C*          FMS4L2 (PS402)  Provider Network Contract
     C*                          (FEIN/MOD/NWCD/BGDT SELECT SYS = 'H')
     C*          FMS4L3 (PS403)  Provider Network Contract
     C*                          (FEIN/MOD/NWCD/BGDT SELECT SYS = 'P')
     C*
     C     S4KY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    SCNWCD
     C                   KFLD                    KKBGDT
     C*
     C     S4KY2         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    SCNWCD
     C*
     C*          FTGTL  (PGT01)  Generic Table File (CO#/KEY)
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*          FMPDL  (PPD01)  Provider Naster File (FEIN/MOD)
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*          NMNWL  (PNW01)  PPO Network File (CO#/NWCD)
     C     NWKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCNWCD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C     *LIKE         DEFINE    S4FEIN        KKFEIN
     C     *LIKE         DEFINE    S4MOD         KKMOD
     C     *LIKE         DEFINE    S4NWCD        KKNWCD
     C     *LIKE         DEFINE    S4CNT#        KKCNT#
     C     *LIKE         DEFINE    S4BGDT        KKBGDT
     C     *LIKE         DEFINE    S4ENDT        X1ENDT
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*================================================================
     C* CMDKEY - COMMAND KEY PROCESSING
     C*================================================================
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
     C*
     C                   DO
     C*
     C*          Validate Nework
     C*          71   NWCD - MCR0443: Invalid network code
     C*          ------------------------------------------------------
     C     SCNWCD        IFEQ      *BLANKS
     C                   SETON                                        7190
     C                   LEAVE                                                  VVV Leave
     C                   ELSE                                                   NWCD = Blanks
     C     NWKY          CHAIN     PNW02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7190
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN60 = On
     C                   MOVEL     NWNAME        SCNWDS
     C                   ENDIF                                                  NWCD = Blanks
     C*
     C*          Validate System type
     C*          72   SYS  - MCR0460: Invalid system type
     C*          ------------------------------------------------------
     C     SCSYS         IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   LEAVE                                                  VVV Leave
     C                   ELSE                                                   SYS  = Blanks
     C     SCSYS         IFNE      'P'
     C     SCSYS         ANDNE     'H'
     C                   SETON                                        7290
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  TYPE = '1'
     C                   MOVEL(P)  SCSYS         KKKEY
     C                   MOVEL(P)  'SYS'         KKTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSYDS
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  SYS  = Blanks
     C*
     C*          Validate System sub-type
     C*          73   STYP - MCR2383: Invalid value entered
     C*          ------------------------------------------------------
     C     SCSTYP        IFNE      *BLANKS
     C                   MOVEL(P)  'STYP'        KKTBID
     C                   MOVEL(P)  SCSTYP        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7390
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN60 = On
     C                   MOVEL(P)  GTDESC        SCSTDS
     C                   ENDIF                                                  STYP <> Blanks
     C*
     C*          Validate Contract begin and end dates
     C*          74   BGDT - MCR0500: Invalid date
     C*          75   BGDT - MCR0312: Term date can not be < start date
     C*               ENDT - MCR0312: Term date can not be < start date
     C*          76   ENDT - MCR0500: Invalid date
     C*          ------------------------------------------------------
     C     SCBGDT        IFEQ      0
     C                   SETON                                        7490
     C                   LEAVE                                                  VVV Leave
     C                   ELSE                                                   BGDT = Blanks
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBGDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7490
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  ERR = '1'
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT           10
     C     SCENDT        IFNE      0
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7690
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  ERR = '1'
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT           10
     C     XXENDT        IFLT      XXBGDT
     C                   SETON                                        7590
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  ENDT < BGDT
     C                   ENDIF                                                  ENDT <> Blanks
     C                   ENDIF                                                  BGDT = Blanks
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKBGDT
     C*
     C     SCENDT        IFNE      *ZEROS
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1ENDT
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1ENDT
     C                   ENDIF
     C*
     C* ADD MODE PROCESSING
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* Check to see if there are other open Network Codes
     C*
     C                   SELECT
     C*
     C     SCSYS         WHENEQ    'P'
     C*
     C     S4KY1         SETGT     PS403
     C     S4KY2         READPE    PS403                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C                   SETON                                        7890
     C                   LEAVE
     C                   ELSE
     C     KKBGDT        IFLE      S4ENDT
     C                   SETON                                        7890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READPE    PS403                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     S4KY1         SETLL     PS403
     C     S4KY2         READE     PS403                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     SCENDT        IFEQ      *ZEROS
     C                   SETON                                        8890
     C                   LEAVE
     C                   ELSE
     C     X1ENDT        IFGE      S4BGDT
     C                   SETON                                        8890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READE     PS403                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     SCSYS         WHENEQ    'H'
     C*
     C     S4KY1         SETGT     PS402
     C     S4KY2         READPE    PS402                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C                   SETON                                        7890
     C                   LEAVE
     C                   ELSE
     C     KKBGDT        IFLE      S4ENDT
     C                   SETON                                        7890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READPE    PS402                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     S4KY1         SETLL     PS402
     C     S4KY2         READE     PS402                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     SCENDT        IFEQ      *ZEROS
     C                   SETON                                        8890
     C                   LEAVE
     C                   ELSE
     C     X1ENDT        IFGE      S4BGDT
     C                   SETON                                        8890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READE     PS402                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C* CHANGE MODE PROCESSING
     C*
     C                   ELSE
     C*
     C                   SELECT
     C*
     C     SCSYS         WHENEQ    'P'
     C*
     C     S4KY1         SETGT     PS403
     C     S4KY2         READPE    PS403                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     @@RRN         IFNE      DBRRN3
     C     S4ENDT        IFEQ      *ZEROS
     C                   SETON                                        8890
     C                   LEAVE
     C                   ELSE
     C     SCENDT        IFNE      *ZEROS
     C     X1ENDT        IFLE      S4ENDT
     C                   SETON                                        8890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READPE    PS403                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     S4KY1         SETLL     PS403
     C     S4KY2         READE     PS403                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     @@RRN         IFNE      DBRRN3
     C     SCENDT        IFEQ      *ZEROS
     C                   SETON                                        8890
     C                   LEAVE
     C                   ELSE
     C     X1ENDT        IFGE      S4BGDT
     C                   SETON                                        8890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READE     PS403                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     SCSYS         WHENEQ    'H'
     C*
     C     S4KY1         SETGT     PS402
     C     S4KY2         READPE    PS402                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     @@RRN         IFNE      DBRRN2
     C     S4ENDT        IFEQ      *ZEROS
     C                   SETON                                        8890
     C                   LEAVE
     C                   ELSE
     C     SCENDT        IFNE      *ZEROS
     C     X1ENDT        IFLE      S4ENDT
     C                   SETON                                        8890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READPE    PS402                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     S4KY1         SETLL     PS402
     C     S4KY2         READE     PS402                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     S4STAT        IFNE      'I'
     C*
     C     @@RRN         IFNE      DBRRN2
     C     SCENDT        IFEQ      *ZEROS
     C                   SETON                                        8890
     C                   LEAVE
     C                   ELSE
     C     X1ENDT        IFGE      S4BGDT
     C                   SETON                                        8890
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     S4KY2         READE     PS402                                  99
     C*
     C                   ENDDO
     C*
     C     *IN90         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*
     C*          Validate Discount code
     C*          77   DISC - MCR0313: Invalid network discount
     C*          ------------------------------------------------------
     C     SCDISC        IFNE      *BLANKS
     C     SCFSFL        OREQ      'N'
     C     SCDISC        CHAIN     PBD01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7790
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN60 = On
     C                   MOVEL(P)  BDCOM1        SCDIDS
     C                   ENDIF                                                  DISC = Blanks
     C*
     C* DISCOUNT CODE MUST BE BLANK IF MANUAL FEE SCHEDULE FLAG = 'Y'
     C*
     C     SCFSFL        IFEQ      'Y'
     C     SCDISC        ANDNE     *BLANKS
     C                   SETON                                        8790
     C                   LEAVE
     C                   ENDIF
     C*
     C*          Validate Manual fee schedule flag
     C*          79   FSFL - MCR0501: Value mut be 'Y' or 'N'
     C*          ------------------------------------------------------
     C     SCFSFL        IFNE      'Y'
     C     SCFSFL        ANDNE     'N'
     C                   SETON                                        7990
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  FSFL <> Blanks
     C*          Validate Elevated care flag
     C*          83   If add: check for duplicate record
     C*          ------------------------------------------------------
     C                   ENDDO                                                  DO Loop
     C     ERRSTP        ENDSR
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
     C*** BEGIN ADD *** 03/23/01 **************************************
     C*================================================================
     C* KGDO - F7=DISCOUNT CODE INFORMATION
     C*================================================================
     C     KGDO          BEGSR
     C*
     C* VALID DISCOUNT CODE MUST BE ENTERED
     C*
     C     SCDISC        CHAIN     PBD01                              99
     C     *IN60         IFEQ      *ON
     C                   SETON                                        8990
     C                   ELSE
     C*
     C                   Z-ADD     140           XXLEN
     C                   MOVEL(P)  PGM(1)        XXCMD
     C                   MOVE      PGM(2)        XXCMD
     C                   MOVEL(P)  BDTYPE        XXTYPE
     C                   MOVEL(P)  BDCODE        XXCODE
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C                   MOVEL(P)  OPTN2         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        XXPRDS
     C                   MOVEL     '1'           XXCALL
     C*
      /Free
        If BDtype = 'D7';
           QQefdt = %CHAR(%date(BDEFDT:*CYMD):*ISO0);
           NetworkDisc(BDtype:XXprds:XXcall:XXcode:
            QQefdt);
        Else;
      /End-Free
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    XXLEN            15 5
      /Free
       Endif;
      /End-Free
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   ENDSR
     C*** BEG ADD *** 11/07/03 ****************************************
     C*================================================================
     C* KHDO - F8=ADDITIONAL DETAIL INFO
     C*================================================================
     C     KHDO          BEGSR
     C*
     C     SCFEIN        IFNE      *ZEROS
     C*
     C     PDTYPE        IFEQ      '1'
     C                   MOVE      PDFEIN        QQFEIN
     C                   MOVE      PDMOD         QQMOD
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'F0040R'
     C                   PARM                    SCPRDS
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM      *BLANK        QQRTC             1
     C                   PARM                    QQMODE
     C                   ENDIF
     C*
     C     PDTYPE        IFEQ      '4'
     C                   MOVE      PDFEIN        QQFEIN            9
     C                   MOVE      PDMOD         QQMOD             3
     C                   MOVE      'D'           QQMODE
     C                   CALL      'F0041R'
     C                   PARM                    SCPRDS
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM      *BLANK        QQRTC
     C                   PARM                    QQMODE
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETON                                        0290
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 11/07/03 ****************************************
     C*
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*             Format dates before search
     C*             ---------------------------------------------------
     C     SCBGDT        IFNE      0
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBGDT
     C                   ENDIF                                                  BGDT <> 0
     C     SCENDT        IFNE      0
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C                   ENDIF                                                  ENDT <> 0
     C*
     C                   SELECT
     C*
     C*             Network search
     C*             ---------------------------------------------------
     C     HFLD          WHENEQ    'SCNWCD'
     C*
     C                   CALL      'N2325R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        4190
     C                   ENDIF                                                  *INKL = On
     C     ##RRN         CHAIN     PNW01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     NWNWCD        SCNWCD
     C                   MOVEL     NWNAME        SCNWDS
     C                   ENDIF                                                  *IN60 = Off
     C                   SETON                                        4290
     C*
     C*             System sub-type search
     C*             ---------------------------------------------------
     C     HFLD          WHENEQ    'SCSTYP'
     C                   MOVEL(P)  OPTNM         ##PRDS
     C                   MOVE(P)   *BLANKS       ##CALL
     C                   MOVEL(P)  'STYP'        ##ID
     C                   MOVE      *BLANKS       ##KY
     C                   MOVE      *BLANKS       ##RTCD
     C                   CALL      'F2493R'
     C                   PARM                    ##PRDS           40
     C                   PARM                    ##CALL            1
     C                   PARM                    ##ID              4
     C                   PARM                    ##KY             10
     C                   PARM                    ##RTCD            1
     C     ##RTCD        IFEQ      'A'
     C                   MOVE      '1'           OUTFLG            1
     C                   ELSE                                                   RTCD = 'A'
     C                   MOVE      '0'           OUTFLG
     C                   ENDIF                                                  RTCD = 'A'
     C     ##RTCD        IFEQ      'B'
     C                   SETON                                        90
     C                   ENDIF                                                  RTCD = 'B'
     C     ##RTCD        IFEQ      '*'
     C                   MOVEL(P)  ##KY          SCSTYP
     C                   MOVEL(P)  SCSTYP        KKKEY
     C                   MOVEL(P)  'STYP'        KKTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSTDS
     C                   ENDIF                                                  *IN60 = Off
     C                   SETON                                        4590
     C                   ELSE                                                   RTCD = '*'
     C                   SETON                                        4490
     C                   ENDIF                                                  RTCD = '*'
     C*
     C*             Discount code search
     C*             ---------------------------------------------------
     C     HFLD          WHENEQ    'SCDISC'
     C                   CALL      'N2330R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG            1
     C                   ELSE                                                   RTCD = 'A'
     C                   MOVE      '0'           OUTFLG
     C                   ENDIF                                                  RTCD = 'A'
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF                                                  RTCD = 'B'
     C     *INKL         IFEQ      *OFF
     C     ##RRN         ANDNE     *ZEROS
     C*
     C     ##RRN         CHAIN     PBD02                              99
     C*
     C                   MOVEL(P)  BDCODE        SCDISC
     C                   MOVEL(P)  BDCOM1        SCDIDS
     C*
     C                   SETON                                        4790
     C                   ELSE                                                   RTCD = '*'
     C                   SETON                                        4690
     C                   ENDIF                                                  RTCD = '*'
     C*
     C*          Error - MCR0676: F10 used on non searchable field
     C*          ------------------------------------------------------
     C                   OTHER
     C                   SETON                                        8290
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL(P)  S4NWCD        SCNWCD
     C                   MOVEL(P)  S4NINT        SCNINT
     C                   MOVEL     S4CNT#        SCCNT#
     C                   MOVEL(P)  S4SYS         SCSYS
     C                   MOVEL(P)  S4STYP        SCSTYP
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCBGDT
     C                   Z-ADD     S4ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCENDT
     C                   MOVEL(P)  S4DISC        SCDISC
     C                   Z-ADD     S4DISP        SCDISP
     C                   MOVEL(P)  S4FSFL        SCFSFL
     C*
     C*          If add:
     C*             Assume 'N' for flags
     C*          ------------------------------------------------------
     C     *IN40         IFEQ      *ON
     C                   MOVEL(P)  'N'           SCFSFL
     C                   ENDIF                                                  *IN40 = On
     C*
     C*          If not add:
     C*             Get Network description
     C*          ------------------------------------------------------
     C     *IN40         IFEQ      *OFF
     C     NWKY          CHAIN     PNW02                              60
     C  N60              MOVEL     NWNAME        SCNWDS
     C                   ENDIF                                                  *IN40 = Off
     C*
     C*             Get System description
     C*             ---------------------------------------------------
     C                   MOVEL(P)  SCSYS         KKKEY
     C                   MOVEL(P)  'SYS'         KKTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSYDS
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*             Get System sub-type description
     C*             ---------------------------------------------------
     C     SCSTYP        IFNE      *BLANKS
     C                   MOVEL(P)  SCSTYP        KKKEY
     C                   MOVEL(P)  'STYP'        KKTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSTDS
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  STYP <> *blanks
     C*
     C*             Get Discount code description
     C*             ---------------------------------------------------
        //*** Beg Chg *** 02/14/23 ********************************************
     C**** SCDISC        CHAIN     PBD01                              60
     C**** *IN60         IFEQ      *OFF
     C****               MOVEL(P)  BDCOM1        SCDIDS
     C****               ENDIF                                                  *IN60 = Off
        Setgt (S4disc:S4bgDt) pbd01;
        Readpe (S4disc) pbd01;
        Dow not %Eof(NMBDLD);
           If BDstat <> 'I';
              scDiDs = BDcom1;
              Leave;
           Endif;
           Readpe (S4disc) pbd01;
        Enddo;
        //*** End Chg *** 02/14/23 ********************************************
     C     CLTID         IFEQ      '114'
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HDENDT            7 0
     C                   MOVE      SCNINT        HDNINT           15
     C                   MOVE      SCCNT#        HDCNT#           15
     C                   MOVE      SCSYS         HDSYS             1
     C                   MOVE      SCSTYP        HDSTYP            3
     C                   MOVE      SCDISC        HDDISC            3
     C                   MOVE      SCDISP        HDDISP            6 2
     C                   MOVE      SCFSFL        HDFSFL            1
     C                   ENDIF                                                  CLTID IFEQ 114
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL(P)  SCNWCD        S4NWCD
     C                   MOVEL(P)  SCNINT        S4NINT
     C                   MOVEL     SCCNT#        S4CNT#
     C                   MOVEL(P)  SCSYS         S4SYS
     C                   MOVEL(P)  SCSTYP        S4STYP
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         S4BGDT
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         S4ENDT
     C                   MOVEL(P)  SCDISC        S4DISC
     C                   Z-ADD     SCDISP        S4DISP
     C                   MOVEL(P)  SCFSFL        S4FSFL
     C   40              MOVE      'A'           S4STAT
     C                   MOVEL(P)  Q1USER        S4USER
     C                   MOVEL(P)  Q1PGM         S4PGM
     C                   Z-ADD     DSTIME        S4TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S4DT
     C     *IN40         IFEQ      *ON
     C                   Z-ADD     SCFEIN        S4FEIN
     C                   Z-ADD     SCMOD         S4MOD
     C                   MOVEL(P)  S4USER        S4IUSR
     C                   MOVEL(P)  S4PGM         S4IPGM
     C                   Z-ADD     S4TIME        S4ITIM
     C                   Z-ADD     S4DT          S4IDT
     C                   MOVEL     *BLANKS       SCNWDS
     C                   MOVEL     *BLANKS       SCSYDS
     C                   MOVEL     *BLANKS       SCSTDS
     C                   MOVEL     *BLANKS       SCDIDS
     C                   ENDIF                                                  *IN40 = On
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
     C     *INKG         CASEQ     *ON           KGDO
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKH         CASEQ     *ON           KHDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*          Return function key settings and exit
     C*          ------------------------------------------------------
     C                   MOVE      *INKL         @@KL
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*          40 - Set on if 'Add' Mode
     C*          ------------------------------------------------------
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C*          Get provider name
     C*          ------------------------------------------------------
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQFEIN        KKFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   MOVE      QQMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              60
     C  N60              MOVEL     PDNAME        SCNAME
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*          MODE 'S' - Change status
     C*          ------------------------------------------------------
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     S4STAT        IFEQ      'I'
     C                   MOVE      'A'           S4STAT
     C                   ELSE                                                   STAT = 'I'
     C                   MOVE      'I'           S4STAT
     C                   ENDIF                                                  STAT = 'I'
     C                   MOVEL     Q1USER        S4USER
     C                   MOVEL     Q1PGM         S4PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S4DT
     C                   MOVEL     DSTIME        S4TIME
     C                   UPDATE    DBRCDF
     C     CLTID         IFEQ      '114'
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C                   ENDIF                                                  *IN99 = Off
     C*
     C                   RETURN
     C                   ENDIF                                                  MODE = 'S'
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C     CLTID         IFEQ      '114'
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATP - Update previous contract with end date if adding new.
     C*================================================================
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C   09              SETON                                        09
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMS4P
     C     CLTID         IFEQ      '114'
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C*
     C                   MOVEL     *ON           OUTFLG
     C*
     C                   ENDSR
     C*================================================================
     C* WRTTR - Write Tracking Record
     C*================================================================
     C     WRTTR         BEGSR
      *
     C     @@MODE        IFEQ      'A'
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN            9 0
     C                   PARM      S4MOD         @@MOD             3 0
     C                   PARM      S4NWCD        @@NWCD            5
     C                   PARM      S4BGDT        @@BGDT            7 0
     C                   PARM      'All'         @@FLD            10
     C                   PARM      ADDED         @@FVAL           80
     C                   PARM      *BLANKS       @@TVAL           80
      *
     C                   ELSE
     C     HDENDT        IFNE      S4ENDT
      *
     C                   MOVEL(P)  'S4ENDT'      @@FLD
     C                   MOVEL(P)  HDENDT        @@FVAL
     C                   MOVEL(P)  S4ENDT        @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN            9 0
     C                   PARM      S4MOD         @@MOD             3 0
     C                   PARM      S4NWCD        @@NWCD            5
     C                   PARM      S4BGDT        @@BGDT            7 0
     C                   PARM                    @@FLD            10
     C                   PARM                    @@FVAL           80
     C                   PARM                    @@TVAL           80
     C                   ENDIF                                                  HDENDT IFNE S4ENDT
      *
     C     HDNINT        IFNE      S4NINT
      *
     C                   MOVEL(P)  'S4NINT'      @@FLD
     C                   MOVEL(P)  HDNINT        @@FVAL
     C                   MOVEL(P)  S4NINT        @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN
     C                   PARM      S4MOD         @@MOD
     C                   PARM      S4NWCD        @@NWCD
     C                   PARM      S4BGDT        @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDNINT IFNE S4NINT
      *
     C     HDCNT#        IFNE      S4CNT#
      *
     C                   MOVEL(P)  'S4CNT#'      @@FLD
     C                   MOVEL(P)  HDCNT#        @@FVAL
     C                   MOVEL(P)  S4CNT#        @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN
     C                   PARM      S4MOD         @@MOD
     C                   PARM      S4NWCD        @@NWCD
     C                   PARM      S4BGDT        @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDCNT# IFNE S4CNT#
      *
     C     HDSYS         IFNE      S4SYS
      *
     C                   MOVEL(P)  'S4SYS'       @@FLD
     C                   MOVEL(P)  HDSYS         @@FVAL
     C                   MOVEL(P)  S4SYS         @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN
     C                   PARM      S4MOD         @@MOD
     C                   PARM      S4NWCD        @@NWCD
     C                   PARM      S4BGDT        @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDSYS  IFNE S4SYS
      *
     C     HDSTYP        IFNE      S4STYP
      *
     C                   MOVEL(P)  'S4STYP'      @@FLD
     C                   MOVEL(P)  HDSTYP        @@FVAL
     C                   MOVEL(P)  S4STYP        @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN
     C                   PARM      S4MOD         @@MOD
     C                   PARM      S4NWCD        @@NWCD
     C                   PARM      S4BGDT        @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDSTYP IFNE S4STYP
      *
     C     HDDISC        IFNE      S4DISC
      *
     C                   MOVEL(P)  'S4DISC'      @@FLD
     C                   MOVEL(P)  HDDISC        @@FVAL
     C                   MOVEL(P)  S4DISC        @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN
     C                   PARM      S4MOD         @@MOD
     C                   PARM      S4NWCD        @@NWCD
     C                   PARM      S4BGDT        @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDDISC IFNE S4DISC
      *
     C     HDDISP        IFNE      S4DISP
      *
     C                   MOVEL(P)  'S4DISP'      @@FLD
     C                   MOVEL(P)  HDDISP        @@FVAL
     C                   MOVEL(P)  S4DISP        @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN
     C                   PARM      S4MOD         @@MOD
     C                   PARM      S4NWCD        @@NWCD
     C                   PARM      S4BGDT        @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDDISP IFNE S4DISP
      *
     C     HDFSFL        IFNE      S4FSFL
      *
     C                   MOVEL(P)  'S4FSFL'      @@FLD
     C                   MOVEL(P)  HDFSFL        @@FVAL
     C                   MOVEL(P)  S4FSFL        @@TVAL
      *
     C                   CALL      'FA754R'
     C                   PARM      S4FEIN        @@FEIN
     C                   PARM      S4MOD         @@MOD
     C                   PARM      S4NWCD        @@NWCD
     C                   PARM      S4BGDT        @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDFSFL IFNE S4FSFL
     C                   ENDIF                                                              S4FSFL
      *
     C                   ENDSR                                                  WRTTR
** pgm
CALL N91XXR PARM('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 'x'
'xxx' 'xxxxxxxx')
