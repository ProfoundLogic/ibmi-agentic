     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  U4110R01 - SELECT RECIPIENT - PRE-CERT LETTERS C:62 02/18/98 *
     H*                                                               *
     H*  CALLED FROM: U4110R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/26/99 C:62 PROJECT: MC145                                  *
     H*               DIDT NEEDS TO BE CONVERTED.                     *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  POSITION CURSORS IN KJDO:  31 - 34.                          *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1: 71 - 87.                         *
     H*                                                               *
     H*****************************************************************
     FU4110F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FUXLCP1    UF A E             DISK
     F                                     RENAME(PLC01:DBRCDF)
     F                                     INFDS(DBFDS)
     FSMDOP     IF   E             DISK
     FUMPCP     IF   E             DISK
     FFMCML1    IF   E           K DISK
     FFMEML     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FSMUPP     IF   E             DISK
     FSMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSTSTP     IF   E             DISK
     FSTABL     IF   E           K DISK
     F                                     RENAME(PST01:PST02)
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Select Recipient')
     D COMAPI          C                   CONST('FA007R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          UXLCP1
     C                   PARM                    @@RRN2            9 0          UMPCP
     C                   PARM                    @@RRN3            9 0          SMDOP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*  State code search parm list
     C*
     C     STCPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C* Network code search parm list.
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CMKY          KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     SMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     UPKY          KLIST
     C                   KFLD                    SCDIUS
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATCVT - CONVERTS THE DATE UDRING A SEARCH
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDIDT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* DIARY TYPE MUST BE 'Y', 'N' OR ' '.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDTYP        ANDNE     'Y'
     C     SCDTYP        ANDNE     'N'
     C     SCDTYP        ANDNE     ' '
     C*
     C                   SETON                                        8890
     C*
     C                   ENDIF
     C*
     C* DIARY RECIPIENT CAN'T BE BLANK IF DIARY TYPE EQ 'Y'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDTYP        ANDEQ     'Y'
     C     SCDIUS        ANDEQ     *BLANKS
     C*
     C                   SETON                                        8990
     C*
     C                   ENDIF
     C*
     C* DIARY RECIPIENT MUST BE BLANK IF DIARY TYPE NE 'Y'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDTYP        ANDNE     'Y'
     C     SCDIUS        ANDNE     *BLANKS
     C*
     C                   SETON                                        3990
     C*
     C                   ENDIF
     C*
     C* DIARY RECIPIENT MUST BE VALID IF NOT BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDIUS        ANDNE     *BLANKS
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* DIARY DATE MUST BE VALID IF RECIPIENT ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDIUS        ANDNE     *BLANKS
     C*
     C                   Z-ADD     SCDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDIDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* DIARY DATE MUST BE ZERO IF DIARY USER IS BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDIUS        ANDEQ     *BLANKS
     C     SCDIDT        ANDNE     *ZEROS
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* CLAIMANT FLAG IS NOT BLANK THEN IT MUST BE '1'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRCFL        ANDNE     *BLANKS
     C     SCRCFL        ANDNE     '1'
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* PROVIDER FLAG IS NOT BLANK THEN IT MUST BE '1'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRPFL        ANDNE     *BLANKS
     C     SCRPFL        ANDNE     '1'
     C*
     C                   SETON                                        8690
     C*
     C                   ENDIF
     C*
     C* TREATING PHYSICIAN FLAG IS NOT BLANK THEN IT MUST BE '1'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRTFL        ANDNE     *BLANKS
     C     SCRTFL        ANDNE     '1'
     C*
     C                   SETON                                        8790
     C*
     C                   ENDIF
     C*
     C* TREATING PHYSICIAN FLAG CAN'T BE USED IF IT DON'T EXIST.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTPHY        ANDEQ     *ZEROS
     C     SCRTFL        ANDNE     *BLANKS
     C*
     C                   SETON                                        4190
     C*
     C                   ENDIF
     C*
     C* SUBSCRIBER FLAG IS NOT BLANK THEN IT MUST BE '1'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSFL        ANDNE     *BLANKS
     C     SCRSFL        ANDNE     '1'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C*
     C* GROUP FLAG IS NOT BLANK THEN IT MUST BE '1'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRGFL        ANDNE     *BLANKS
     C     SCRGFL        ANDNE     '1'
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF
     C*
     C* EMPLOYER FLAG IS NOT BLANK THEN IT MUST BE '1'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCREFL        ANDNE     *BLANKS
     C     SCREFL        ANDNE     '1'
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF
     C*
     C* EMPLOYER FLAG MUST BE BLANK IF EMPLOYER NUMBER IS ZERO
     C*
     C     *IN90         IFEQ      *OFF
     C     SCREFL        ANDNE     *BLANKS
     C     SCEMP#        ANDEQ     *ZEROS
     C*
     C                   SETON                                        7890
     C*
     C                   ENDIF
     C*
     C* EMPLOYER ATTENTION MUST BE BLANK IF EMPLOYER NUMBER IS ZERO
     C*
     C     *IN90         IFEQ      *OFF
     C     SCREAT        ANDNE     *BLANKS
     C     SCEMP#        ANDEQ     *ZEROS
     C*
     C                   SETON                                        7990
     C*
     C                   ENDIF
     C*
     C* OTHER FLAG IS NOT BLANK THEN IT MUST BE '1'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCROFL        ANDNE     *BLANKS
     C     SCROFL        ANDNE     '1'
     C*
     C                   SETON                                        8090
     C*
     C                   ENDIF
     C*
     C     SCROFL        IFEQ      '1'
     C*
     C* OTHER NAME IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRONM        ANDEQ     *BLANKS
     C*
     C                   SETON                                        8190
     C*
     C                   ENDIF
     C*
     C* OTHER ADDRESS 1 IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCROA1        ANDEQ     *BLANKS
     C*
     C                   SETON                                        8290
     C*
     C                   ENDIF
     C*
     C* OTHER CITY IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCROCT        ANDEQ     *BLANKS
     C*
     C                   SETON                                        8390
     C*
     C                   ENDIF
     C*
     C* OTHER STATE IS REQUIRED AND MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCROST        KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                        8490
     C*
     C                   ENDIF
     C*
     C* ZIP CODE IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCROZ1        ANDEQ     *ZEROS
     C*
     C                   SETON                                        8590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* MUST SELECT ONE RECIPIENT
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRCFL        ANDEQ     *BLANKS
     C     SCRPFL        ANDEQ     *BLANKS
     C     SCRTFL        ANDEQ     *BLANKS
     C     SCRSFL        ANDEQ     *BLANKS
     C     SCRGFL        ANDEQ     *BLANKS
     C     SCREFL        ANDEQ     *BLANKS
     C     SCROFL        ANDEQ     *BLANKS
     C*
     C                   SETON                                        4290
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SCDIUS'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      1             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                          31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPUSR         SCDIUS
     C                   MOVEL     UPUSNM        SCUSNM
     C                   SETON                                          32
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCROST'
     C*
     C                   CALL      'SY601R'      STCPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   MOVEL     STKEY         SCROST
     C                   SETON                                        34
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C* SCR01 VARIABLES
     C*
     C     @@RRN3        CHAIN     PDO01                              99
     C*
     C                   MOVEL(P)  DODIFL        SCDTYP
     C                   MOVEL(P)  DORMCD        SCRMCD
     C*
     C                   CALL      'DA200R'
     C                   PARM      SCRMCD        ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'Y'
     C*
     C                   MOVEL(P)  ##DESC        SCRMDS
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCRMDS
     C*
     C                   ENDIF
     C                   MOVEL(P)  LCDIUS        SCDIUS
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        SCUSNM
     C   99              MOVEL(P)  *BLANKS       SCUSNM
     C*
     C*** BEGIN CHG *** 05/26/99 **************************************
     C**********           Z-ADDLCDIDT    SCDIDT
     C                   Z-ADD     LCDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDIDT
     C*** END   CHG *** 05/26/99 **************************************
     C                   MOVEL(P)  LCRCFL        SCRCFL
     C                   MOVEL(P)  LCRPFL        SCRPFL
     C                   MOVEL(P)  LCRTFL        SCRTFL
     C                   MOVEL(P)  LCRSFL        SCRSFL
     C                   MOVEL(P)  LCRSAT        SCRSAT
     C                   MOVEL(P)  LCRGFL        SCRGFL
     C                   MOVEL(P)  LCRGAT        SCRGAT
     C                   MOVEL(P)  LCREFL        SCREFL
     C                   MOVEL(P)  LCREAT        SCREAT
     C*
     C     @@RRN2        CHAIN     PPC01                              99
     C*
     C                   MOVEL(P)  PCCERT        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     CMLSN         CAT(P)    ',':0         SCCLNM
     C                   CAT(P)    CMFSN:1       SCCLNM
     C*
     C     CMMID         IFNE      *BLANK
     C*
     C                   CAT(P)    CMMID:1       SCCLNM
     C                   CAT(P)    '.':0         SCCLNM
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     PCFEIN        SCFEIN
     C                   Z-ADD     PCMOD         SCMOD
     C                   Z-ADD     PCFEIN        KKFEIN
     C                   Z-ADD     PCMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C  N99              MOVEL(P)  PDNAME        SCPRNM
     C   99              MOVEL(P)  *BLANKS       SCPRNM
     C*
     C                   Z-ADD     PCTPHY        SCTPHY
     C                   Z-ADD     PCTPHM        SCTPHM
     C                   Z-ADD     PCTPHY        KKFEIN
     C                   Z-ADD     PCTPHM        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C  N99              MOVEL(P)  PDNAME        SCTPNM
     C   99              MOVEL(P)  *BLANKS       SCTPNM
     C*
     C                   Z-ADD     PCFEIN        SCFEIN
     C                   Z-ADD     PCMOD         SCMOD
     C                   Z-ADD     PCFEIN        KKFEIN
     C                   Z-ADD     PCMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C  N99              MOVEL(P)  PDNAME        SCPRNM
     C   99              MOVEL(P)  *BLANKS       SCPRNM
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSBNM
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCGRNM
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C  N99              Z-ADD     EMEMP#        SCEMP#
     C  N99              Z-ADD     EMDIV         SCDIV
     C  N99              MOVEL(P)  EMNAME        SCEMNM
     C   99              Z-ADD     *ZEROS        SCEMP#
     C   99              Z-ADD     *ZEROS        SCDIV
     C   99              MOVEL(P)  *BLANKS       SCEMNM
     C*
     C                   MOVEL(P)  LCROFL        SCROFL
     C*
     C                   MOVEL(P)  LCRONM        SCRONM
     C                   MOVEL(P)  LCROAT        SCROAT
     C                   MOVEL(P)  LCROA1        SCROA1
     C                   MOVEL(P)  LCROA2        SCROA2
     C                   MOVEL(P)  LCROCT        SCROCT
     C                   MOVEL(P)  LCROST        SCROST
     C                   MOVEL     LCROZP        SCROZ1
     C                   MOVE      LCROZP        SCROZ2
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* SCR01 VARIABLES
     C*
     C                   MOVEL(P)  SCDIUS        LCDIUS
     C*** BEGIN CHG *** 05/26/99 **************************************
     C**********           Z-ADDSCDIDT    LCDIDT
     C                   Z-ADD     SCDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCDIDT
     C*** END   CHG *** 05/26/99 **************************************
     C                   MOVEL(P)  SCRCFL        LCRCFL
     C                   MOVEL(P)  SCRPFL        LCRPFL
     C                   MOVEL(P)  SCRTFL        LCRTFL
     C                   MOVEL(P)  SCRSFL        LCRSFL
     C                   MOVEL(P)  SCRGFL        LCRGFL
     C                   MOVEL(P)  SCREFL        LCREFL
     C                   MOVEL(P)  SCROFL        LCROFL
     C                   MOVEL(P)  SCRONM        LCRONM
     C                   MOVEL(P)  SCROAT        LCROAT
     C                   MOVEL(P)  SCROA1        LCROA1
     C                   MOVEL(P)  SCROA2        LCROA2
     C                   MOVEL(P)  SCROCT        LCROCT
     C                   MOVEL(P)  SCROST        LCROST
     C                   MOVEL     SCROZ1        LCROZP
     C                   MOVE      SCROZ2        LCROZP
     C                   MOVEL(P)  SCRSAT        LCRSAT
     C                   MOVEL(P)  SCRGAT        LCRGAT
     C                   MOVEL(P)  SCREAT        LCREAT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   MOVEA     '0000'        *IN(31)
     C                   EXSR      DATCVT
     C                   SETON                                        06
     C*
     C     *INKJ         CASEQ     *ON           KJDO
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
     C* MAKE SURE THE CURSOR POSITIONS ON CLAIMANT SELECT.
     C*
     C                   SETOFF                                       06
     C*
     C*** GET SYSTEM INFO
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
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C                   MOVE      LCCO#         #1CO#
     C                   MOVE      LCSUB#        #1SUB#
     C                   MOVE      LCFND         #1FND
     C                   MOVE      LCFYR         #1FYR
     C                   MOVE      LCCASE        #1CASE
     C*
     C* TWO LINE CLAIM HEADER
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCHDL1        PARM      *BLANK        ##L1             78
     C     SCHDL2        PARM      *BLANK        ##L2             78
     C*
     C* ASSIGN DOCUMENT ID AND FOLDER
     C*
     C                   MOVEL(P)  LCDCID        SCDCID
     C                   MOVEL(P)  LCFLDR        SCFLDR
     C*
     C                   ENDSR                                                  PGMSTR
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
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        LCUSER
     C                   MOVEL     Q1PGM         LCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCDT
     C*
     C                   MOVEL     DSTIME        LCTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C     DOPGMN        IFNE      *BLANKS
     C*
     C                   CALL      DOPGMN                               99
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0          UXLCP1
     C                   PARM      @@RRN2        ##RRN2            9 0          UMPCP
     C                   PARM      @@RRN3        ##RRN3            9 0          SMDOP
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      UXLCP1
     C*
     C                   ENDSR
