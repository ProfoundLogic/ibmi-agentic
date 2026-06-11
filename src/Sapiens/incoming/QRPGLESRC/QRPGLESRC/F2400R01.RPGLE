     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H* PROGRAM: F2400R01                                             *
     H* DESCRIPTION: WORK WITH STATE FS RULES                         *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H* 01-06 - Error indicators display file controlled              *
     H* 11-29 - Used to position cursor after searching               *
     H* 31-39 - Used to position cursor after searching               *
     H* 42-52 - Used to position cursor after searching               *
     H* 53-59 - Error indicators display file controlled              *
     H* 61-67 - Error indicators display file controlled              *
     H* 68    - Used to position cursor after searching               *
     H* 71-89 - Error indicators copybook controlled                  *
     H*****************************************************************
     H* PROGRAMMER NOTES:                                             *
     H* 1.  When adding fields add to the I spec for PFR02 so         *
     H*     when copying rules all the field values will get          *
     H*     copied                                                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/08/23 C:079    Project: NY060                             *
     H*                    Added new fields FREOBCD1 and FREOBCD2     *
     H*                                                               *
     H*  06/03/19  C:212   Project : NY046                            *
     H*                    Added screen fields for Addon and Primary  *
     H*                    code and related processing                *
     H*                                                               *
     H*  06/20/18  C:079   Support : 99 71719                         *
     H*                    Change to have 3 columns for each screen   *
     H*                    Move fields from screen 4 to screen 3      *
     H*                                                               *
     H*  02/27/17  C:079   Project : FS497                            *
     H*                    Add FRCAPCSI ti I-specs for Copy           *
     H*                                                               *
     H*  12/09/16  C:309   Project : MS032                            *
     H*                    Added logic for field APC Status Indicator *
     H*                                                               *
     H*  07/26/16  C:079   Support : 42573/99                         *
     H*                    On copy call F2400R03 in Change mode       *
     H*                                                               *
     H*  05/30/16  C:309   Project : FS497                            *
     H*                    (Medicare OPPS status indicator Update)    *
     H*                    Added logic for new field Status Indicator *
     H*                                                               *
     H*  09/01/15  C:079   Support: 129/33851                         *
     H*                    Add Invalid code field                     *
     H*                                                               *
     H*  08/11/10  C:227   Project: IL001                             *
     H*                    ADD EDITS FOR PAY PER DIEM FIELD           *
     H*                                                               *
     H*  11/14/08  C:212   Project: TX043                             *
     H*                    ADD RELATED PROCESSING FOR NEW FIELD       *
     H*                    ADDED - ASC PMNT IND (SCASPI)              *
     H*                                                               *
     H*  10/16/08  C:079   Project: SY082                             *
     H*                    ADD MISSING FIELDS TO PFR02 SO COPY WORKS  *
     H*                                                               *
    H*  04/07/08  C:189   Project: TX039                             *
     H*                    EDIT FOR DRG RATE WILL BE FROM GENERAL     *
    H*                    TABLE                                      *
     H*                                                               *
     H*  04/03/08  C:198   Project: TX039                             *
     H*                    Added APC Pymt SI and Implant status field *
     H*                                                               *
    H*  11/02/06  C:182   Project: TX033                             *
     H*                    Add #DAYS FRM SVC DT FIELD                 *
    H*                                                               *
     H*  10/31/05  C:79    Project: GA019                             *
     H*                    Add search on outlier                      *
     H*                                                               *
     H*  01/11/05  C:79    Support: MC129 44887                       *
     H*                    Allow Y or D for FRCPTS                    *
     H*                                                               *
     H*  09/25/04  C:79    Project: TX027                             *
     H*                    ADD BILLED HPSA ELIGIBLE                   *
     H*                                                               *
     H*  02/27/04  C:79    Project: FL048                             *
     H*                    ADD COMPARED MULTIPLE UNITS                *
     H*                                                               *
     H*  11/05/03  C:79    Project: FS272                             *
     H*                    ADD BILLED ACC DATE FIELDS                 *
     H*                                                               *
     H*  11/01/03  C:79    Project: FS262                             *
     H*                    ADD BILLED ASSISTANT SURG FIELD            *
     H*                                                               *
     H*  10/31/03  C:79    Project: FS270                             *
     H*                    1. ADD CLIENT NUMBER                       *
     H*                    2. ADD STATUS                              *
     H*                                                               *
     H*  10/04/03  C:79    Project: MS012                             *
     H*                    SET STATUS = 'A' ON AN ADD                 *
     H*                                                               *
     H*  09/25/03  C: 079  Project: TX020                             *
     H*                    ADD 7 MODIFIER INDICATOR FIELDS            *
     H*                    CHANGE EDIT/SEARCH ON SCVAL                *
     H*                                                               *
     H*  09/15/03  C: 079  Project: TX020                             *
     H*                    ADD 3 BILLED MODIFIER FIELDS               *
     H*                                                               *
     H*  07/15/03  C: 079  Project: TX020                             *
     H*                    ADD LOGIC TO CHANGE THE STATUS             *
     H*                    ALLOW 'N' FOR COMPARED TO SEPARATE PROC    *
     H*                                                               *
     H*  04/07/03  C: 079  Project: PA020                             *
     H*                    ADD FIELD FRPPTV TO PFR02 FOR COPYING      *
     H*                                                               *
     H*  01/29/03  C: 079  PROJECT: FSMNT                             *
     H*                    1. LIFT EDIT ON PCAT                       *
     H*                    2. ADD EDIT ON ST/EFDT                     *
     H*                                                               *
     H*  10/17/02  C: 079  PROJECT: FS239                             *
     H*                    1. ADD A 4TH SCREEN TO MAKE ROOM FOR       *
     H*                       NEW FIELD TO PAY AWP                    *
     H*                    2. MOVE EOB CODE AND MESSAGES TO NEW       *
     H*                       SCREEN                                  *
     H*                    3. REMOVE ALL REVISION COMMENTS TO CLEAN   *
     H*                       UP PROGRAM                              *
     H*****************************************************************
     H*
     FF2400F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDFRP     UF A E             DISK
     F                                     RENAME(PFR01:DBRCDF)
     F                                     INFDS(DBFDS)
     FSTSTL1    IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEG ADD *** 01/29/03 ****************************************
     FFDFRL     IF   E           K DISK
     F                                     RENAME(PFR01:PFR03)
     F*** END ADD *** 01/29/03 ****************************************
     FFDFRL2    IF   E           K DISK
     F                                     RENAME(PFR01:PFR02)
     F*** BEG ADD *** 10/17/02 ****************************************
     F                                     INFDS(DBFDS1)
     F*** END ADD *** 10/17/02 ****************************************
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PTS01:PTS02)
     FFTTSP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     FFMRCP     IF   E             DISK
     F                                     RENAME(PRC01:PRC03)
     FFMRCL     IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     FFMPRL     IF   E           K DISK    USROPN
     FFMSVL1    IF   E           K DISK    USROPN
     FFTPSL     IF   E           K DISK
     F                                     RENAME(PPS01:PPS02)
     FFMRXP     IF   E             DISK
     f*** Beg Add *** 06/20/18 *************************************
     FFTMIL     IF   E           K DISK
     f*** End Add *** 06/20/18 *************************************
      /COPY SCOPYMT01D
     D*
     D QCA             S             56    DIM(3) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** BEG ADD *** 10/17/02 ****************************************
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*** BEG ADD *** 10/17/02 ****************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State FS Rules Maint-
     D                                     enance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D*    USING DATA STRUCTURE FOR DATABASE OVERIDE
     D*
     D                 DS
     D  @@CMD                  1     56
     D  XXST                  38     39
     D* USED TO OVERRIDE FMSVL1
     D                 DS
     D  @2CMD                  1     56
     D  X2ST                  40     41
     D  SCTOS                  3      5
     D                 DS
     D  KKSTTS                 1     10
     D  SCST                   1      2
     D  SCBTOS                 3      4

     I*
     IPFR02
     I              FRST                        X2ST
     I              FREFDT                      X2EFDT
     I              FREXDT                      X2EXDT
     I              FRRULE                      X2RULE
     I              FRPCAT                      X2PCAT
     I              FRDESC                      X2DESC
     I              FRPAGE                      X2PAGE
     I              FRINFO                      X2INFO
     I              FRPTY                       X2PTY
     I              FRSYS                       X2SYS
     I              FRSPCD                      X2SPCD
     I              FRBFCD                      X2BFCD
     I              FRBTCD                      X2BTCD
     I              FRBFMD                      X2BFMD
     I              FRBTMD                      X2BTMD
     I              FRBFPS                      X2BFPS
     I              FRBTPS                      X2BTPS
     I              FRBTOS                      X2BTOS
     I              FRBPRT                      X2BPRT
     I              FRCFCD                      X2CFCD
     I              FRCTCD                      X2CTCD
     I              FRCMOD                      X2CMOD
     I              FRCPOS                      X2CPOS
     I              FRCTOS                      X2CTOS
     I              FRCTOB                      X2CTOB
     I              FRCSDT                      X2CSDT
     I              FRCHDT                      X2CHDT
     I              FRCSPR                      X2CSPR
     I              FRCDPR                      X2CDPR
     I              FRCFDY                      X2CFDY
     I              FRCPDY                      X2CPDY
     I              FR#PRC                      X2#PRC
     I              FR#DAY                      X2#DAY
     I              FREOB                       X2EOB
     I              FRMSG                       X2MSG
     I              FRPMR                       X2PMR
     I              FRPCH                       X2PCH
     I              FRDIS                       X2DIS
     I              FRPCT                       X2PCT
     I              FRAMT                       X2AMT
     I              FRSTAT                      X2STAT
     I              FRUSER                      X2USER
     I              FRPGM                       X2PGM
     I              FRTIME                      X2TIME
     I              FRDT                        X2DT
     I              FRIUSR                      X2IUSR
     I              FRIPGM                      X2IPGM
     I              FRITIM                      X2ITIM
     I              FRIDT                       X2IDT
     I              FRPROF                      X2PROF
     I              FRTECH                      X2TECH
     I              FR#GPC                      X2#GPC
     I              FR#GDY                      X2#GDY
     I              FRCPTS                      X2CPTS
     I              FRBSEP                      X2BSEP
     I              FRCSEP                      X2CSEP
     I              FRBRT                       X2BRT
     I              FRBAST                      X2BAST
     I              FRCAST                      X2CAST
     I              FRPCVL                      X2PCVL
     I              FRTCVL                      X2TCVL
     I              FRVAL                       X2VAL
     I              FRPCPT                      X2PCPT
     I              FRBSPB                      X2BSPB
     I              FRUNIT                      X2UNIT
     I              FRAUNT                      X2AUNT
     I              FRCONV                      X2CONV
     I              FRFUD                       X2FUD
     I              FRTMDV                      X2TMDV
     I              FREFCD                      X2EFCD
     I              FRETCD                      X2ETCD
     I              FRCT                        X2CT
     I              FRNCT                       X2NCT
     I              FRHTOS                      X2HTOS
     I              FRMSG2                      X2MSG2
     I              FRMSG3                      X2MSG3
     I              FRMAJ                       X2MAJ
     I              FRCON2                      X2CON2
     I              FRFTM                       X2FTM
     I              FRTTM                       X2TTM
     I              FRMR                        X2MR
     I              FRHMRA                      X2HMRA
     I              FRRND                       X2RND
     I              FRPANE                      X2PANE
     I              FRPFAC                      X2PFAC
     I              FRPPDM                      X2PPDM
     I              FRCOMP                      X2COMP
     I              FRAVAL                      X2AVAL
     I              FRPAS2                      X2PAS2
     I              FRTANE                      X2TANE
     I              FRPTYP                      X2PTYP
     I              FRLOSS                      X2LOSS
     I              FRTLOS                      X2TLOS
     I              FRPGRP                      X2PGRP
     I              FRPPGP                      X2PPGP
     I              FRPOUT                      X2POUT
     I              FRCSBL                      X2CSBL
     I              FRPDIF                      X2PDIF
     I              FRCPRT                      X2CPRT
     I              FRBMRI                      X2BMRI
     I              FRCMRI                      X2CMRI
     I              FRPSRV                      X2PSRV
     I              FRPASC                      X2PASC
     I              FRPTTM                      X2PTTM
     I              FROVRD                      X2OVRD
     I              FRIGMD                      X2IGMD
     I              FRPPSC                      X2PPSC
     I              FRPDRG                      X2PDRG
     I              FRFTOB                      X2FTOB
     I              FRTTOB                      X2TTOB
     I              FRFEMA                      X2FEMA
     I              FRPOMA                      X2POMA
     I              FRMEMA                      X2MEMA
     I              FRSCMA                      X2SCMA
     I              FRMRMA                      X2MRMA
     I              FRDRMA                      X2DRMA
     I              FRSVAL                      X2SVAL
     I              FRPSNI                      X2PSNI
     I              FRPSNO                      X2PSNO
     I              FRPPHO                      X2PPHO
     I              FRPPHI                      X2PPHI
     I              FRPCRF                      X2PCRF
     I              FRPHHI                      X2PHHI
     I              FRPHHO                      X2PHHO
     I              FRPORC                      X2PORC
     I              FRPPOR                      X2PPOR
     I              FRPPIR                      X2PPIR
     I              FRBFUT                      X2BFUT
     I              FRBDEX                      X2BDEX
     I              FRCCOD                      X2CCOD
     I              FRFTOS                      X2FTOS
     I              FRFTSS                      X2FTSS
     I              FRTTOS                      X2TTOS
     I              FRTTSS                      X2TTSS
     I              FRPRX                       X2PRX
     I              FRPPTV                      X2PPTV
     I              FRBMD2                      X2BMD2
     I              FRBMD3                      X2BMD3
     I              FRBMD4                      X2BMD3
     I              FRPARE                      X2PARE
     I              FRPCIN                      X2PCIN
     I              FR50IN                      X250IN
     I              FR51IN                      X251IN
     I              FR62IN                      X262IN
     I              FR66IN                      X266IN
     I              FR80IN                      X280IN
     I              FRCLT#                      X2CLT#
     I              FRBSRG                      X2BSRG
     I              FRFADT                      X2FADT
     I              FRTADT                      X2TADT
     I              FRMUNT                      X2MUNT
     I              FRBFHE                      X2BFHE
     I              FR#DSD                      X2#DSD
     I              FRAPSI                      X2APSI
     I              FRIMST                      X2IMST
     I              FRASPI                      X2ASPI
     I              FRrel                       x2Rel
     I              FRproj                      x2Proj
     I              FRbInvl                     x2Binvl
     I              FRapcsi                     x2ApcSi
     I              FRcApcSi                    x2CapcSi
     I              FRbfad                      x2bfad
     I              FRctpc                      x2ctpc
     I*** Beg Add *** 05/08/23 ****************************************
     I              FReobCd1                    x2eobCd1
     I              FReobCd2                    x2eobCd2
     I*** End Add *** 05/08/23 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@RRN3            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##ST              2
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##RRN3            9 0
     C*================================================================
     C*  KLIST definitions
     C*
     C*  EOMB SEARCH
     C     EOBKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQKEY
     C*
     C     GTKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQID
     C                   KFLD                    QQKEY
     C*
     C     GTKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQID
     C*
     C*  PLACE OF SERVICE
     C     PSKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXPOS
     C*
     C*
     C*  TYPE OF SERVICE
     C*
     C     TSKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    KKSTTS
     C*
     C*  SPECIALTY CODE TABLE FILE
     C     SPKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQKEY
     C*
     C*  Procedure Code file
     C     PRKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    X1EFDT
     C*
     C*  Procedure Code file
     C     PRKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXCPT4
     C*
     C*  REASONABLE AND CUSTOMARY FILE
     C     RCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C                   KFLD                    X1EFDT
     C*
     C*  SERVICE CODE FILE
     C     SVKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXCMSV                         CHG 04/28/92
     C                   KFLD                    X1EFDT
     C*
     C*  MODIFIER KEY
     C     MDKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQKEY
     C*
     C*  Body part file
     C*
     C     PTKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQKEY
     C*  Type of Facility file
     C     FTKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQKEY
     C*
     C*
     C*  FILE SEARCH
     C     KEY3          KLIST
     C                   KFLD                    SCST
     C                   KFLD                    XXRULE
     C*
     C*  FILE SEARCH
     C     KEY4          KLIST
     C                   KFLD                    SCST
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C     FRKY          KLIST
     C                   KFLD                    K1ST
     C                   KFLD                    KKEFDT
     C*
     C*  EOMB SEARCH
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQKEY
     C*
     C*  MESSAGE DESCRIPTION
     C*
     C     MGKY          KLIST
     C                   KFLD                    XXMSG
     C*
     C*  MESSAGE DESCRIPTION
     C*
     C     MIKY          KLIST
     C                   KFLD                    KKMSG#
     C*
     C*================================================================
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*
     C     *LIKE         DEFINE    FRBTOS        XXTOS
     C     *LIKE         DEFINE    RCZIPA        XXZIPA                                        2
     C     *LIKE         DEFINE    SVCMSV        XXCMSV                                        2
     C     *LIKE         DEFINE    FRST          XXST1                                         2
     C     *LIKE         DEFINE    STKEY         KKST                                          2
     C     *LIKE         DEFINE    FREFDT        X1EFDT                                        2
     C     *LIKE         DEFINE    FREFDT        KKEFDT                                        2
     C     *LIKE         DEFINE    FRST          K1ST                                          2
     C     *LIKE         DEFINE    MIMSG#        KKMSG#                                        2
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*** BEG ADD *** 10/17/02 ****************************************
     C*===============================================================
     C* CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
     C*===============================================================
     C*
     C     CALLR         BEGSR
     C*
     C     SCRN#         IFEQ      #OFSCR
     C*
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C                   MOVE      DBRRN         ##RRN
     C                   MOVE      'C'           ##MODE
     C*
     C                   ELSE
     C     @@MODE        IFEQ      'P'
     C                   MOVE      'C'           ##MODE
     C*
     C                   MOVE      DBRRN         ##RRN
     C                   eval      ##rrn = @@rrn
     C*
     C                   ELSE
     C*
     C                   MOVE      @@RRN         ##RRN
     C                   MOVE      'D'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  CALLR
     C*================================================================
     C* CALLRT - RETURN FROM ADDITIONAL SCREEN PROGRAMS
     C*================================================================
     C     CALLRT        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'C'
     C     @@MODE        OREQ      'A'
     C     @@MODE        OREQ      'P'
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      'C'           @@MODE
     C                   MOVE      ##RRN         @@RRN
     C                   SETOFF                                       4041
     C                   ENDIF
     C*
     C                   EXSR      PRECHG
     C*
     C     @@MODE        WHENEQ    'D'
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   EXSR      PREDSP
     C*
     C                   OTHER
     C                   EXSR      PREOTH
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     FRSTAT        IFEQ      'I'
     C                   MOVE      'A'           FRSTAT
     C                   ELSE
     C                   MOVE      'I'           FRSTAT
     C                   ENDIF                                                  FRSTAT
     C*
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVEL     DSTIME        PRTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C     *INKL         IFEQ      *OFF
     C                   ADD       1             SCRN#
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* CLEAR INDICATORS
     C*
     C                   SETOFF                                       535455
     C                   SETOFF                                       565758
     C                   SETOFF                                       596162
     C                   SETOFF                                       636465
     C                   SETOFF                                       6667
     C*
     C*   CHECK EFFECTIVE DATE FOR VALIDITY AND BLANK
     C*
     C  N90SCEFDT        COMP      *ZEROS                                 71
     C  N90SCEFDT        COMP      *ZEROS                                 90
     C*
     C  N90SCEFDT        IFNE      *ZEROS
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071      71 POS CURS OR
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*ZEROS
     C*****************************************************************
     C  N90SCEXDT        IFNE      *ZEROS
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9072
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*BLANK
     C*
     C*****************************************************************
     C*
     C*   VERIFY CATEGORY IS NOT BLANK AND IS A VALID CATEGORY
     C*
     C  N90SCPCAT        IFEQ      *BLANKS
     C                   SETON                                        9073
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VERIFY DESCIPTION IS NOT BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        9074
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY SYSTEM IS NOT BLANK AND IS A VALID VALUE
     C*
     C  N90SCSYS         IFEQ      *BLANK
     C                   SETON                                        9075
     C                   ENDIF
     C*
     C  N90SCSYS         IFNE      *BLANK
     C                   MOVEL(P)  SCSYS         QQKEY
     C                   MOVEL(P)  'ATYP'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9075
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY CODE TYPE IS VALID
     C*
     C  N90SCCT          IFNE      *BLANK
     C                   MOVEL(P)  SCCT          QQKEY
     C                   MOVEL(P)  'CTYP'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9076
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY SPECIALTY CODE IS A VALID VALUE
     C*
     C  N90SCSPCD        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCSPCD        QQKEY
     C     SPKY          CHAIN     PSP01                              99
     C   99              SETON                                        9077
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY SPECIALTY CODE BLANK IS Y OR BLANK
     C*
     C  N90SCBSPB        IFNE      *BLANK
     C     SCBSPB        ANDNE     'Y'
     C*
     C                   SETON                                        9078
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/25/03 ****************************************
     C*
     C*   VERIFY PC/TC INDICATOR
     C*
     C  N90SCPCIN        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCPCIN        QQKEY
     C                   MOVEL(P)  'MIPC'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9001
     C*
     C                   ENDIF                                                  PCIN IFNE BLANK
     C*
     C*   VERIFY BILAT SURG INDICATOR
     C*
     C  N90SC50IN        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SC50IN        QQKEY
     C                   MOVEL(P)  'MI50'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9002
     C*
     C                   ENDIF                                                  50IN IFNE BLANK
     C*
     C*   VERIFY MULTI SURG INDICATOR
     C*
     C  N90SC51IN        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SC51IN        QQKEY
     C                   MOVEL(P)  'MI51'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9003
     C*
     C                   ENDIF                                                  51IN IFNE BLANK
     C*
     C*   VERIFY CO SURG INDICATOR
     C*
     C  N90SC62IN        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SC62IN        QQKEY
     C                   MOVEL(P)  'MI62'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9004
     C*
     C                   ENDIF                                                  62IN IFNE BLANK
     C*
     C*   VERIFY TEAM SURG INDICATOR
     C*
     C  N90SC66IN        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SC66IN        QQKEY
     C                   MOVEL(P)  'MI66'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9005
     C*
     C                   ENDIF                                                  66IN IFNE BLANK
     C*
     C*   VERIFY ASST SURG INDICATOR
     C*
     C  N90SC80IN        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SC80IN        QQKEY
     C                   MOVEL(P)  'MI80'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9006
     C*
     C                   ENDIF                                                  80IN IFNE BLANK
     C*
     C*
     C  N90SCFADT        IFNE      *ZEROS
     C                   Z-ADD     SCFADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFADT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9007
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*ZEROS
     C*
     C  N90SCTADT        IFNE      *ZEROS
     C                   Z-ADD     SCTADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTADT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9008
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*ZEROS
     C*
     C*****************************************************************
     C*
     C*   VERIFY BY REPORT FIELD IS BLANK OR VALID
     C*
     C  N90SCBRT         IFNE      *BLANK
     C                   MOVEL(P)  SCBRT         QQKEY
     C                   MOVEL(P)  'BYRP'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9079
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY MODIFIER VALUE IS Y, V,  OR BLANK
     C*
     C  N90SCVAL         IFNE      *BLANK
     C     SCVAL         ANDNE     'Y'
     C     SCVAL         ANDNE     'V'
     C*
     C                   SETON                                        9080
     C*
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VERIFY BILLED TO CODE
     C*
     C  N90SCBTCD        IFEQ      *BLANK
     C                   SETON                                        9081
     C                   ENDIF
     C*****************************************************************
     C*
     C*   Verify Rule use flag if not blank
     C*
     C  N90SCBMRI        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCBMRI        QQKEY
     C                   MOVEL(P)  'RLUS'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9082
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY AST PROC FLAG IS Y OR BLANK
     C*
     C  N90SCBAST        IFNE      *BLANK
     C     SCBAST        ANDNE     'Y'
     C*
     C                   SETON                                        9083
     C*
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VERIFY FOLLOW UP FLAG IS Y OR BLANK
     C*
     C  N90SCFUD         IFNE      *BLANK
     C     SCFUD         ANDNE     'Y'
     C*
     C                   SETON                                        9084
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   Verify follow up type flag
     C*
     C  N90SCBFUT        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCBFUT        QQKEY
     C                   MOVEL(P)  'FOLU'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9085
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VERIFY NO CUT FLAG IS Y OR BLANK
     C*
     C  N90SCNCT         IFNE      *BLANK
     C     SCNCT         ANDNE     'Y'
     C*
     C                   SETON                                        9086
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY SEPARATE PROC FLAG IS Y OR BLANK
     C*
     C  N90SCBSEP        IFNE      *BLANK
     C     SCBSEP        ANDNE     'Y'
     C*
     C                   SETON                                        9087
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   Verify Anesthesia calculation type
     C*
     C  N90SCTANE        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCTANE        QQKEY
     C                   MOVEL(P)  'ANTY'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9088
     C                   ENDIF
     C*****************************************************************
     C*
     C*   Verify Pharmacy Drug Type
     C*
     C  N90SCPTYP        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCPTYP        QQKEY
     C                   MOVEL(P)  'PROD'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9089
     C                   ENDIF
     C*
     C*****************************************************************
     C*   VERIFY PART CODE FIELD IS VALID
     C*
     C  N90SCBPRT        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCBPRT        QQKEY
     C     PTKY          CHAIN     PPA01                              99
     C   99              SETON                                        9053
     C                   ENDIF
     C*****************************************************************
     C*   Verify Peer Group field is blank or a valid facility code
     C*
     C  N90SCPGRP        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCPGRP        QQKEY
     C     FTKY          CHAIN     PFT01                              99
     C   99              SETON                                        9054
     C                   ENDIF
     C*****************************************************************
     C*
     C*   EDIT ASC VALUE FLAG
     C*
     C  N90SCAVAL        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCAVAL        QQKEY
     C                   MOVEL(P)  'VALU'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9055
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY BILLED DRG EXEMPT FLAG IS Y OR BLANK
     C*
     C  N90SCBDEX        IFNE      *BLANK
     C     SCBDEX        ANDNE     'Y'
     C*
     C                   SETON                                        9056
     C*
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   Verify Correct Coding flag
     C*
     C  N90SCCCOD        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCCCOD        QQKEY
     C                   MOVEL(P)  'CCRU'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9057
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VERIFY BILLED TYPE OF SERVICE IS VALID
     C*
     C  N90SCBTOS        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCBTOS        XXPOS
     C     TSKY          CHAIN     PTS02                              99
     C   99              SETON                                        9067
     C*
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VERIFY MRA/BILLED AMOUNT COMPARISON
     C*
     C  N90SCCOMP        IFNE      *BLANK
     C     SCCOMP        ANDNE     'Y'
     C*
     C                   SETON                                        9058
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY MEDICARE NUMBER MATCH FLAG IS Y,N OR BLANK
     C*
     C  N90SCMEMA        IFNE      *BLANK
     C     SCMEMA        ANDNE     'Y'
     C     SCMEMA        ANDNE     'N'
     C*
     C                   SETON                                        9059
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY MEDICARE NUMBER RCC MATCH FLAG IS Y,N OR BLANK
     C*
     C  N90SCMRMA        IFNE      *BLANK
     C     SCMRMA        ANDNE     'Y'
     C     SCMRMA        ANDNE     'N'
     C*
     C                   SETON                                        9061
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY DRG MATCH FLAG IS Y,N OR BLANK
     C*
     C  N90SCDRMA        IFNE      *BLANK
     C     SCDRMA        ANDNE     'Y'
     C     SCDRMA        ANDNE     'N'
     C*
     C                   SETON                                        9062
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY FEDERAL REGISTER MATCH FLAG IS Y,N OR BLANK
     C*
     C  N90SCFEMA        IFNE      *BLANK
     C     SCFEMA        ANDNE     'Y'
     C     SCFEMA        ANDNE     'N'
     C*
     C                   SETON                                        9063
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY PROSPECTIVE PAYMENT MATCH FLAG IS Y,N OR BLANK
     C*
     C  N90SCPOMA        IFNE      *BLANK
     C     SCPOMA        ANDNE     'Y'
     C     SCPOMA        ANDNE     'N'
     C*
     C                   SETON                                        9064
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY SERVICE CODE VALUE FLAG IS Y,N OR BLANK
     C*
     C  N90SCSVAL        IFNE      *BLANK
     C     SCSVAL        ANDNE     'Y'
     C     SCSVAL        ANDNE     'N'
     C*
     C                   SETON                                        9065
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY SERVICE CODE MATCH FLAG IS Y,N OR BLANK
     C*
     C  N90SCSCMA        IFNE      *BLANK
     C     SCSCMA        ANDNE     'Y'
     C     SCSCMA        ANDNE     'N'
     C*
     C                   SETON                                        9066
     C*
     C                   ENDIF
     C*****************************************************************
     C                   SETOFF                                       6970
     C  N90SCAPSI        IFNE      *BLANK
     C                   MOVEL(P)  'APSI'        QQID
     C                   MOVEL(P)  SCAPSI        QQKEY
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9069
     C                   ENDIF
     C*
     C  N90SCIMST        IFNE      *BLANK
     C                   MOVEL(P)  'IMST'        QQID
     C                   MOVEL(P)  SCIMST        QQKEY
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9070
     C                   ENDIF
     C*
     C                   SETOFF                                       91
     C  N90SCASPI        IFNE      *BLANK
     C                   MOVEL(P)  'ASPI'        QQID
     C                   MOVEL(P)  SCASPI        QQKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   ELSE
     C                   SETON                                        9091
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         KKEFDT
     C                   MOVE      SCST          K1ST
     C*
     C     FRKY          SETLL     PFR03                                  99
     C     *IN99         IFEQ      *OFF
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     1             SCRN#             1 0
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO CODE SAME PROCEDURE
     C*
     C  N90SCCPTS        IFNE      'Y'
     C     SCCPTS        ANDNE     'D'
     C     SCCPTS        ANDNE     *BLANK
     C*
     C                   SETON                                        9071
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO SEPARATE PROCEDURE
     C*
     C  N90SCCSEP        IFNE      'Y'
     C     SCCSEP        ANDNE     'N'
     C     SCCSEP        ANDNE     *BLANK
     C*
     C                   SETON                                        9072
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO ASTERISKS PROCEDURE
     C*
     C  N90SCCAST        IFNE      'Y'
     C     SCCAST        ANDNE     *BLANK
     C*
     C                   SETON                                        9073
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE HIGHER MRA
     C*
     C  N90SCHMRA        IFNE      'Y'
     C     SCHMRA        ANDNE     *BLANK
     C*
     C                   SETON                                        9074
     C                   ENDIF
     C*****************************************************************
     C*   VALIDATE TYPE OF STOP-LOSS
     C*
     C  N90SCTLOS        IFNE      *BLANK
     C*
     C*
     C                   MOVEL(P)  SCTLOS        QQKEY
     C                   MOVEL(P)  'SLTY'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9075
     C*
     C                   ENDIF                                                   END SCTLOS
     C*****************************************************************
     C*   VALIDATE COMPARED TO RULE USE
     C*
     C  N90SCCMRI        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCCMRI        QQKEY
     C                   MOVEL(P)  'RLUS'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9076
     C*
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO MAJOR
     C*
     C  N90SCMAJ         IFNE      'Y'
     C     SCMAJ         ANDNE     *BLANK
     C*
     C                   SETON                                        9077
     C                   ENDIF
     C*****************************************************************
     C*   VALIDATE COMPARED TO MODIFIER
     C*
     C*
     C  N90SCCMOD        IFNE      *BLANK
     C                   MOVEL(P)  SCCMOD        QQKEY
     C     MDKY          CHAIN     PPM01                              99
     C   99              SETON                                        9078
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO INGNORE MODIFIER
     C*
     C  N90SCIGMD        IFNE      'Y'
     C     SCIGMD        ANDNE     *BLANK
     C*
     C                   SETON                                        9079
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO HISTORY
     C*
     C  N90SCHSDT        IFNE      'Y'
     C     SCHSDT        ANDNE     *BLANK
     C*
     C                   SETON                                        9080
     C                   ENDIF
     C*****************************************************************
     C*   VALIDATE PLACE OF SERVICE EXISTS
     C*
     C*
     C  N90SCCPOS        IFGT      0
     C*
     C                   Z-ADD     SCCPOS        XXPOS
     C     PSKY          CHAIN     PPS02                              99
     C   99              SETON                                        9081
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO SAME DATE
     C*
     C  N90SCCSDT        IFNE      'Y'
     C     SCCSDT        ANDNE     *BLANK
     C*
     C                   SETON                                        9082
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO SAME PROVIDER
     C*
     C  N90SCCSPR        IFNE      'Y'
     C     SCCSPR        ANDNE     *BLANK
     C*
     C                   SETON                                        9083
     C                   ENDIF
     C*****************************************************************
     C*   VALIDATE BODY AREA
     C*
     C  N90SCCPRT        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCCPRT        QQKEY
     C*
     C     PTKY          CHAIN     PPA01                              99
     C*
     C   99              SETON                                        9084
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO DIFF PROVIDER
     C*
     C  N90SCCDPR        IFNE      'Y'
     C     SCCDPR        ANDNE     *BLANK
     C*
     C                   SETON                                        9085
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO SAME BILL
     C*
     C  N90SCCSBL        IFNE      'Y'
     C     SCCSBL        ANDNE     *BLANK
     C*
     C                   SETON                                        9086
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE COMPARED TO TYPE OF BILL
     C*
     C  N90SCCTOB        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCCTOB        QQKEY
     C                   MOVEL(P)  BTTBID        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9087
     C                   ENDIF
     C*
     C*****************************************************************
     C*   VALIDATE COMPARED TO IGNORE OVERRIDES
     C*
     C  N90SCOVRD        IFNE      *BLANK
     C     SCOVRD        ANDNE     'Y'
     C*
     C                   SETON                                        9088
     C*
     C                   ENDIF                                                   END SCOVRD
     C*****************************************************************
     C*   VALIDATE COMPARED TO NUMBER OF PROC GREATER THAN
     C*
     C  N90SC#GPC        IFNE      *BLANK
     C     SC#GPC        ANDNE     'Y'
     C*
     C                   SETON                                        9089
     C*
     C                   ENDIF                                                   END SC#GPC
     C*****************************************************************
     C*   VALIDATE COMPARED TO NUMBER OF DAYS GREATER THAN
     C*
     C  N90SC#GDY        IFNE      *BLANK
     C     SC#GDY        ANDNE     'Y'
     C*
     C                   SETON                                        9053
     C*
     C                   ENDIF                                                   END SC#GDY
     C*****************************************************************
     C                   Eval      erCAPCSI = *Off
     C                   Eval      msCAPCSI = *Blanks
     C  N90SCCAPCSI      IFNE      *BLANK
     C                   MOVEL(P)  'APSI'        QQID
     C                   MOVEL(P)  SCCAPCSI      QQKEY
     C     GTKY          CHAIN     PGT01                              99
     C                   If        *IN99 = *On
     C                   SETON                                        90
     C                   Eval      erCAPCSI = *On
     C                   Eval      msCAPCSI = 'MC50000'
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     2             SCRN#
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*****************************************************************
     C*
     C* VALIDATE PAY MRA FIELD
     C*
     C  N90SCPMR         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCPMR         QQKEY
     C                   MOVEL(P)  'MRTY'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9071
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY PROF COMP FIELD
     C*
     C  N90SCPROF        IFNE      *BLANKS
     C     SCPROF        ANDNE     'Y'
     C*
     C                   SETON                                        9072
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PROF COMP VALUE FIELD
     C*
     C  N90SCPCVL        IFNE      *BLANKS
     C     SCPCVL        ANDNE     'Y'
     C*
     C                   SETON                                        9073
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE TECH COMP FIELD
     C*
     C  N90SCTECH        IFNE      *BLANKS
     C     SCTECH        ANDNE     'Y'
     C*
     C                   SETON                                        9074
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE TECH COMP VALUE FIELD
     C*
     C  N90SCTCVL        IFNE      *BLANKS
     C     SCTCVL        ANDNE     'Y'
     C*
     C                   SETON                                        9075
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE DISALLOW FIELD
     C*
     C  N90SCDIS         IFNE      *BLANKS
     C     SCDIS         ANDNE     'Y'
     C*
     C                   SETON                                        9076
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY CHARGED FIELD
     C*
     C  N90SCPCH         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCPCH         QQKEY
     C                   MOVEL(P)  'CHTY'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9077
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY ANES/MI/HR/RNT FIELD
     C*
     C  N90SCPANE        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCPANE        QQKEY
     C                   MOVEL(P)  'ANRU'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9078
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE ROUNDING FIELD
     C*
     C  N90SCRND         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCRND         QQKEY
     C                   MOVEL(P)  'ANRD'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9079
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY RX FIELD
     C*
     C  N90SCPRX         IFNE      'Y'
     C     SCPRX         ANDNE     *BLANK
     C*
     C                   SETON                                        9080
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY CPT CODE VALUE
     C*
     C  N90SCPCPT        IFNE      *BLANKS
     C*
     C                   MOVEL     SCPCPT        XXCPT4
     C*
     C     PRKY1         CHAIN     PPR01                              99
     C*
     C   99              SETON                                        9081
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY PROVIDER SERVICE CODE FIELD
     C*
     C  N90SCPPSC        IFNE      'Y'
     C     SCPPSC        ANDNE     *BLANK
     C*
     C                   SETON                                        9082
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY INPATIENT PER DIEM FIELD
     C*
     C*
     C  N90SCPPDM        IFNE      *BLANKS
     C                   MOVEL(P)  SCPPDM        QQKEY
     C                   MOVEL(P)  'TYPD'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9083
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*   VALIDATE PAY PROVIDER SERVICE CODE FIELD
     C*
     C  N90SCPSRV        IFNE      'Y'
     C     SCPSRV        ANDNE     *BLANK
     C*
     C                   SETON                                        9084
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY PEER GROUP FIELD
     C*
     C  N90SCPPGP        IFNE      'Y'
     C     SCPPGP        ANDNE     *BLANK
     C*
     C                   SETON                                        9085
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE OUTLIER PAYMENT FIELD
     C*
     C*
     C     *IN90         IFNE      *ON
     C     SCPOUT        ANDNE     *BLANK
     C*
     C                   MOVEL(P)  SCPOUT        QQKEY
     C                   MOVEL(P)  'OUTL'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9086
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY DIFFERENCE OF CHARGED - MRA FIELD
     C*
     C  N90SCPDIF        IFNE      *BLANK
     C     SCPDIF        ANDNE     'Y'
     C*
     C                   SETON                                        9087
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY DRG AMOUNT FIELD
     C*
     C*
     C*
     C  N90SCPDRG        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCPDRG        QQKEY
     C                   MOVEL(P)  'TDRG'        QQID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9088
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY CORF AMOUNT FIELD
     C*
     C  N90SCPCRF        IFNE      'Y'
     C     SCPCRF        ANDNE     *BLANK
     C*
     C                   SETON                                        9089
     C                   ENDIF
     C*****************************************************************
     C*****************************************************************
     C*
     C*   VALIDATE PAY OUTPATIENT RCC FIELD
     C*
     C  N90SCPORC        IFNE      'Y'
     C     SCPORC        ANDNE     *BLANK
     C*
     C                   SETON                                        9053
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VALIDATE PAY ASC AMOUNT FIELD
     C*
     C  N90SCPASC        IFNE      'Y'
     C     SCPASC        ANDNE     *BLANK
     C*
     C                   SETON                                        9054
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY RX INPATIENT
     C*
     C  N90SCPPHI        IFNE      'Y'
     C     SCPPHI        ANDNE     *BLANK
     C*
     C                   SETON                                        9055
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY RX INPATIENT RCC
     C*
     C  N90SCPPIR        IFNE      'Y'
     C     SCPPIR        ANDNE     *BLANK
     C*
     C                   SETON                                        9056
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY RX OUTPATIENT
     C*
     C  N90SCPPHO        IFNE      'Y'
     C     SCPPHO        ANDNE     *BLANK
     C*
     C                   SETON                                        9057
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY RX OUTPATIENT RCC
     C*
     C  N90SCPPOR        IFNE      'Y'
     C     SCPPOR        ANDNE     *BLANK
     C*
     C                   SETON                                        9058
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY SKILLED NURSE INPATIENT
     C*
     C  N90SCPSNI        IFNE      'Y'
     C     SCPSNI        ANDNE     *BLANK
     C*
     C                   SETON                                        9059
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY SKILLED NURSE OUTPATIENT
     C*
     C  N90SCPSNO        IFNE      'Y'
     C     SCPSNO        ANDNE     *BLANK
     C*
     C                   SETON                                        9061
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY HOME HEALTH INPATIENT
     C*
     C  N90SCPHHI        IFNE      'Y'
     C     SCPHHI        ANDNE     *BLANK
     C*
     C                   SETON                                        9062
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY HOME HEALTH OUTPATIENT
     C*
     C  N90SCPHHO        IFNE      'Y'
     C     SCPHHO        ANDNE     *BLANK
     C*
     C                   SETON                                        9063
     C                   ENDIF
     C*****************************************************************
     C*
     C* VALIDATE PAY FACILITY FEE FIELD
     C*
     C  N90SCPFAC        IFNE      'Y'
     C     SCPFAC        ANDNE     *BLANK
     C*
     C                   SETON                                        9064
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     3             SCRN#
     C*
     C                   ENDSR                                                  END SCRND
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      @@ST          SCST
     C*
     C*   SEARCH FOR NEXT RULE NUMBER PER EFFECTIVE DATE
     C*
     C                   Z-ADD     99999         XXRULE            9 0
     C     KEY3          SETGT     PFR02
     C     KEY4          READPE    PFR02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     X2RULE        ADD       1             SCRULE
     C                   ELSE
     C                   Z-ADD     1             SCRULE
     C                   ENDIF                                                    *IN99
     C*
     C                   MOVE      *BLANKS       SCEFDT
     C                   MOVE      *BLANKS       SCPCAT
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'A'           SCSTAT
     C                   Z-ADD     *ZEROS        SCPAGE
     C                   MOVE      *BLANK        SCSYS
     C                   MOVE      *BLANKS       SCSTDS
     C                   MOVE      *BLANKS       SCINFO
     C                   MOVE      *BLANKS       SCSPCD
     C                   MOVE      *BLANKS       SCPCDS
     C                   MOVE      *BLANKS       SCEXDT
     C                   MOVE      *BLANKS       SCPTY
     C                   Z-ADD     *ZEROS        SCAMT
     C                   Z-ADD     *ZEROS        SCPCT
     C                   MOVE      *BLANK        SCPROF
     C                   MOVE      *BLANK        SCTECH
     C                   Z-ADD     *ZEROS        SCTMDV
     C                   MOVE      *BLANK        SCFUD
     C                   MOVE      *BLANK        SCCT
     C                   Z-ADD     *ZEROS        SCCONV
     C                   Z-ADD     *ZEROS        SCCON2
     C                   Z-ADD     *ZEROS        SCFTM
     C                   Z-ADD     *ZEROS        SCTTM
     C                   MOVE      *BLANK        SCCPTS
     C                   MOVE      *BLANK        SCCSEP
     C                   MOVE      *BLANK        SCPCVL
     C                   MOVE      *BLANK        SCTCVL
     C                   MOVE      *BLANK        SCBSEP
     C                   MOVE      *BLANK        SCBRT
     C                   MOVE      *BLANK        SCMAJ
     C                   MOVE      *BLANK        SCNCT
     C                   MOVE      *BLANK        SCPCPT
     C                   MOVE      *BLANK        SCBSPB
     C                   Z-ADD     *ZEROS        SCUNIT
     C                   Z-ADD     *ZEROS        SCAUNT
     C                   MOVE      *BLANK        SCBAST
     C                   MOVE      *BLANK        SCCAST
     C                   MOVE      *BLANK        SCVAL
     C*
     C*    CLEARING BILLED FIELDS
     C*
     C                   MOVE      *BLANKS       SCBFCD
     C                   MOVE      *BLANKS       SCBTCD
     C                   MOVE      *BLANKS       SCBFMD
     C                   MOVE      *BLANKS       SCBTMD
     C                   Z-ADD     *ZEROS        SCBFPS
     C                   Z-ADD     *ZEROS        SCBTPS
     C                   MOVE      *BLANK        SCBTOS
     C                   MOVE      *BLANKS       SCBPRT
     C                   MOVE      *BLANKS       SCBFPD
     C                   MOVE      *BLANKS       SCBTPD
     C                   MOVE      *BLANKS       SCCOMP
     C                   MOVE      *BLANKS       SCAVAL
     C                   MOVE      *BLANKS       SCTANE
     C                   MOVE      *BLANKS       SCPTYP
     C                   Z-ADD     *ZERO         SCLOSS
     C                   MOVE      *BLANKS       SCTLOS
     C                   MOVE      *BLANKS       SCPGRP
     C                   MOVE      *BLANKS       SCCT
     C                   MOVE      *BLANKS       SCBMRI
     C                   MOVE      *BLANKS       SCBFUT
     C                   MOVE      *BLANKS       SCBDEX
     C                   MOVE      *BLANKS       SCCCOD
     C                   MOVE      *BLANKS       SCFTOS
     C                   MOVE      *BLANKS       SCFTSS
     C                   MOVE      *BLANKS       SCTTOS
     C                   MOVE      *BLANKS       SCTTSS
     C                   MOVE      *BLANKS       SCBMD2
     C                   MOVE      *BLANKS       SCBMD3
     C                   MOVE      *BLANKS       SCBMD4
     C                   MOVE      *BLANKS       SCPCIN
     C                   MOVE      *BLANKS       SC50IN
     C                   MOVE      *BLANKS       SC51IN
     C                   MOVE      *BLANKS       SC62IN
     C                   MOVE      *BLANKS       SC66IN
     C                   MOVE      *BLANKS       SC80IN
     C                   Z-ADD     *ZEROS        SCFADT
     C                   Z-ADD     *ZEROS        SCTADT
     C                   MOVE      *BLANK        SCBFHE
     C*
     C*    CLEARING COMPARING FIELDS
     C*
     C                   MOVE      *BLANKS       SCCFCD
     C                   MOVE      *BLANKS       SCCTCD
     C                   MOVE      *BLANKS       SCCMOD
     C                   Z-ADD     *ZEROS        SCCPOS
     C                   MOVE      *BLANKS       SCCTOB
     C                   MOVE      *BLANK        SCCSDT
     C                   MOVE      *BLANK        SCHSDT
     C                   MOVE      *BLANK        SCCSPR
     C                   MOVE      *BLANK        SCCDPR
     C                   Z-ADD     *ZEROS        SCCFDY
     C                   Z-ADD     *ZEROS        SCCPDY
     C                   Z-ADD     *ZEROS        SC#PRC
     C                   Z-ADD     *ZEROS        SC#DAY
     C                   MOVE      *BLANK        SC#GPC
     C                   MOVE      *BLANK        SC#GDY
     C                   MOVE      *BLANKS       SCEFCD
     C                   MOVE      *BLANKS       SCETCD
     C                   MOVE      *BLANKS       SCHMRA
     C                   MOVE      *BLANKS       SCCSBL
     C                   MOVE      *BLANKS       SCCPRT
     C                   MOVE      *BLANKS       SCCMRI
     C                   MOVE      *ZEROS        SCPTTM
     C                   MOVE      *ZEROS        SCTRMT
     C                   MOVE      *BLANK        SCOVRD
     C                   MOVE      *BLANK        SCIGMD
     C                   MOVE      *BLANK        SCMUNT
     C*
     C*    CLEARING PROCESSING INFORMATION
     C*
     C                   MOVE      *BLANK        SCPMR
     C                   MOVE      *BLANK        SCPCH
     C                   MOVE      *BLANKS       SCDIS
     C                   MOVE      *BLANKS       SCRND
     C                   MOVE      *BLANKS       SCPFAC
     C                   MOVE      *BLANKS       SCPPDM
     C                   MOVE      *BLANKS       SCPANE
     C                   MOVE      *BLANKS       SCPPGP
     C                   MOVE      *BLANKS       SCPOUT
     C                   MOVE      *BLANKS       SCPDIF
     C                   MOVE      *BLANKS       SCPSRV
     C                   MOVE      *BLANKS       SCPASC
     C                   MOVE      *BLANKS       SCPRX
     C                   MOVE      *BLANKS       SC#DSD
     C                   MOVE      *BLANKS       SCPPTV
     C                   MOVE      *BLANKS       SCPARE
       //*** Beg Chg *** 05/08/23 ********************************************
     C**************     MOVE      *BLANK        SCEOB
     C**************     MOVE      *BLANKS       SCEOBD
       scEobCd1 = *blanks;
       scEobCd2 = *blanks;
       scEobD1  = *blanks;
       scEobD2  = *blanks;
       //*** End Chg *** 05/08/23 ********************************************
     C                   MOVE      *BLANK        SCMSG
     C                   MOVE      *BLANK        SCMSG2
     C                   MOVE      *BLANK        SCMSG3
     C                   MOVE      *BLANKS       SCMSGD
     C                   MOVE      *BLANKS       SCMSD2
     C                   MOVE      *BLANKS       SCMSD3
     C                   MOVE      *BLANKS       SCMR
     C                   MOVE      *BLANKS       SCPAS2
     C**** End Add **** 06/20/18 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      FRST          SCST
     C*
     C     *IN41         IFEQ      *ON
     C*
     C*   SEARCH FOR NEXT RULE NUMBER PER EFFECTIVE DATE IF COPY
     C                   Z-ADD     99999         XXRULE
     C     KEY3          SETGT     PFR02                              99
     C                   READP     PFR02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     X2RULE        ADD       1             SCRULE
     C                   ELSE
     C                   Z-ADD     1             SCRULE
     C                   ENDIF                                                    *IN99
     C*
     C                   ELSE
     C                   Z-ADD     FRRULE        SCRULE
     C                   ENDIF
     C*
     C                   Z-ADD     FREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*** BEG ADD *** 10/31/03 ****************************************
     C                   MOVE      FRSTAT        SCSTAT
     C*** END ADD *** 10/31/03 ****************************************
     C                   MOVE      FRPCAT        SCPCAT
     C                   MOVE      FRDESC        SCDESC
     C                   Z-ADD     FRPAGE        SCPAGE
     C                   MOVE      FRSYS         SCSYS
     C                   MOVE      FRINFO        SCINFO
     C*** BEG ADD *** 10/31/03 ****************************************
     C                   Z-ADD     FRCLT#        SCCLT#
     C*** END ADD *** 10/31/03 ****************************************
     C                   Z-ADD     FREXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   MOVE      FRPTY         SCPTY
     C                   Z-ADD     FRAMT         SCAMT
     C                   Z-ADD     FRPCT         SCPCT
     C                   MOVEL     FRPROF        SCPROF
     C                   MOVEL     FRTECH        SCTECH
     C                   Z-ADD     FRTMDV        SCTMDV
     C                   MOVEL     FRFUD         SCFUD
     C                   MOVEL     FRCT          SCCT
     C*** BEG DEL *** 10/04/02 ****************************************
     C******************** MOVELFRHTOS    SCHTS1
     C******************** MOVE FRHTOS    SCHTS2
     C*** END DEL *** 10/04/02 ****************************************
     C                   Z-ADD     FRCONV        SCCONV
     C                   Z-ADD     FRCON2        SCCON2
     C                   Z-ADD     FRFTM         SCFTM
     C                   Z-ADD     FRTTM         SCTTM
     C                   MOVEL     FRSPCD        SCSPCD
     C                   MOVEL     FRCPTS        SCCPTS
     C                   MOVEL     FRCSEP        SCCSEP
     C                   MOVEL     FRPCVL        SCPCVL
     C                   MOVEL     FRTCVL        SCTCVL
     C                   MOVEL     FRBSEP        SCBSEP
     C                   MOVEL     FRBRT         SCBRT
     C                   MOVEL     FRMAJ         SCMAJ
     C                   MOVEL     FRNCT         SCNCT
     C                   MOVEL     FRPCPT        SCPCPT
     C                   MOVEL     FRBSPB        SCBSPB
     C                   Z-ADD     FRUNIT        SCUNIT
     C                   Z-ADD     FRAUNT        SCAUNT
     C                   MOVEL     FRBAST        SCBAST
     C                   MOVEL     FRCAST        SCCAST
     C                   MOVEL     FRVAL         SCVAL
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELFRMR      SCMR
     C*** END DEL *** 10/17/02 ****************************************
     C*
     C*    BILLED FIELDS
     C*
     C                   MOVEL     FRBFMD        SCBFMD
     C                   MOVEL     FRBTMD        SCBTMD
     C                   MOVEL     FRBFCD        SCBFCD
     C                   MOVEL     FRBTCD        SCBTCD
     C                   Z-ADD     FRBFPS        SCBFPS
     C                   Z-ADD     FRBTPS        SCBTPS
     C                   MOVEL     FRBTOS        SCBTOS
     C                   MOVEL     FRBPRT        SCBPRT
     C*** BEG ADD ***** 04/27/99 ***********************************************
     C                   MOVEL     FRCOMP        SCCOMP
     C                   MOVEL     FRAVAL        SCAVAL
     C*** END ADD ***** 04/27/99 ***********************************************
     C*** BEG ADD 09/09/99 ********************************************
     C                   MOVE      FRTANE        SCTANE
     C*** END ADD 09/09/99 ********************************************
     C*** BEG ADD ***** 09/29/99 ***********************************************
     C                   MOVE      FRPTYP        SCPTYP
     C*** BEG ADD ***** 09/29/99 ***********************************************
     C*** BEG ADD ***** 11/04/99 ***********************************************
     C                   Z-ADD     FRLOSS        SCLOSS
     C                   MOVE      FRTLOS        SCTLOS
     C                   MOVE      FRPGRP        SCPGRP
     C*** BEG ADD ***** 11/04/99 ***********************************************
     C*** BEG ADD ***** 04/08/00 ***********************************************
     C                   MOVE      FRBMRI        SCBMRI
     C*** END ADD ***** 04/08/00 ***********************************************
     C*** BEG ADD ***** 07/13/01 ***********************************************
     C                   MOVE      FRFTOB        SCFTOB
     C                   MOVE      FRTTOB        SCTTOB
     C                   MOVE      FRFEMA        SCFEMA
     C                   MOVE      FRPOMA        SCPOMA
     C                   MOVE      FRMEMA        SCMEMA
     C                   MOVE      FRSCMA        SCSCMA
     C*** BEG ADD ***** 04/03/08 ***********************************************
     C***** Beg Chg *** 05/30/16 ***********************************************
     C*****              MOVE      FRAPSI        SCAPSI
     C                   MOVE      FRAPCSI       SCAPSI
     C***** End Chg *** 05/30/16 ***********************************************
    C***** Beg Add *** 12/09/16 ***********************************************
     C                   MOVE      FRCAPCSI      SCCAPCSI
    C***** End Add *** 12/09/16 ***********************************************
     C*** END ADD ***** 04/03/08 ***********************************************
     C*** BEG ADD ***** 11/14/08 ***********************************************
     C                   MOVEL     FRASPI        SCASPI
     C*** END ADD ***** 11/14/08 ***********************************************
     C                   MOVE      FRMRMA        SCMRMA
     C                   MOVE      FRDRMA        SCDRMA
     C                   MOVE      FRSVAL        SCSVAL
     C*** END ADD ***** 07/13/01 ***********************************************
     C*** BEG ADD ***** 10/11/01 ***********************************************
     C                   MOVE      FRBFUT        SCBFUT
     C*** END ADD ***** 10/11/01 ***********************************************
     C*** BEG ADD ***** 11/29/01 ***********************************************
     C                   MOVE      FRBDEX        SCBDEX
     C*** END ADD ***** 11/29/01 ***********************************************
     C*** BEG ADD *** 01/16/02 ****************************************
     C                   MOVE      FRCCOD        SCCCOD
     C*** END ADD *** 01/16/02 ****************************************
     C*** BEG ADD *** 10/04/02 ****************************************
     C                   MOVE      FRFTOS        SCFTOS
     C                   MOVE      FRFTSS        SCFTSS
     C                   MOVE      FRTTOS        SCTTOS
     C                   MOVE      FRTTSS        SCTTSS
     C*** END ADD *** 10/04/02 ****************************************
     C*** BEG ADD *** 09/15/03 ****************************************
     C                   MOVE      FRBMD2        SCBMD2
     C                   MOVE      FRBMD3        SCBMD3
     C                   MOVE      FRBMD4        SCBMD4
     C*** END ADD *** 09/15/03 ****************************************
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      FRPCIN        SCPCIN
     C                   MOVE      FR50IN        SC50IN
     C                   MOVE      FR51IN        SC51IN
     C                   MOVE      FR62IN        SC62IN
     C                   MOVE      FR66IN        SC66IN
     C                   MOVE      FR80IN        SC80IN
     C*** END ADD *** 09/25/03 ****************************************
     C*** BEG ADD *** 11/01/03 ****************************************
     C                   MOVE      FRBSRG        SCBSRG
     C*** END ADD *** 11/01/03 ****************************************
     C*** BEG ADD *** 11/05/03 ****************************************
     C*
     C                   Z-ADD     FRFADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFADT
     C*
     C                   Z-ADD     FRTADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTADT
     C*** END ADD *** 11/05/03 ****************************************
     C*** BEG ADD *** 09/25/04 ****************************************
     C                   MOVE      FRBFHE        SCBFHE
     C*** END ADD *** 09/25/04 ****************************************
     C*** Beg Add *** 09/01/15 ****************************************
     C                   Eval      scBInvl = FRbInvl
     C*** End Add *** 09/01/15 ****************************************
     C***** Beg Add *** 06/03/19 ***************************************
     C                   Eval      scBFAD = FRbfad
     C                   If        FRbfad = *blanks
     C                   Eval      scBFAD = 'N'
     C                   EndIf
     C                   Eval      scCTPC = FRctpc
     C                   If        FRctpc = *blanks
     C                   Eval      scCTPC = 'N'
     C                   EndIf
     C***** End Add *** 06/03/19 ***************************************
     C*
     C*    COMPARING FIELDS
     C*
     C                   MOVEL     FRCPOS        SCCPOS
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELFRCTOS    SCCTOS
     C*** END DEL *** 10/17/02 ****************************************
     C                   MOVEL     FRCTOB        SCCTOB
     C                   MOVEL     FRCSDT        SCCSDT
     C                   MOVEL     FRCHDT        SCHSDT
     C                   MOVEL     FRCSPR        SCCSPR
     C                   Z-ADD     FRCFDY        SCCFDY
     C                   Z-ADD     FRCPDY        SCCPDY
     C                   Z-ADD     FR#PRC        SC#PRC
     C                   Z-ADD     FR#DAY        SC#DAY
     C                   MOVEL     FR#GPC        SC#GPC
     C                   MOVEL     FR#GDY        SC#GDY
     C                   MOVEL     FRCFCD        SCCFCD
     C                   MOVEL     FRCTCD        SCCTCD
     C                   MOVEL     FRCMOD        SCCMOD
     C                   MOVEL     FRCDPR        SCCDPR
     C*** BEG ADD ***** 03/24/98 ***********************************************
     C                   MOVEL     FREFCD        SCEFCD
     C                   MOVEL     FRETCD        SCETCD
     C*** END ADD ***** 03/24/98 ***********************************************
     C*** BEG ADD ***** 11/24/98 ***********************************************
     C                   MOVEL     FRHMRA        SCHMRA
     C*** END ADD ***** 11/24/98 ***********************************************
     C*** BEG ADD ***** 11/16/99 ***********************************************
     C                   MOVEL     FRCSBL        SCCSBL
     C*** END ADD ***** 11/16/99 ***********************************************
     C*** BEG ADD ***** 02/16/00 ***********************************************
     C                   MOVEL     FRCPRT        SCCPRT
     C*** END ADD ***** 02/16/00 ***********************************************
     C*** BEG ADD ***** 04/08/00 ***********************************************
     C                   MOVE      FRCMRI        SCCMRI
     C*** END ADD ***** 04/08/00 ***********************************************
     C*** BEG ADD ***** 11/01/00 ***********************************************
     C                   Z-ADD     FRPTTM        SCPTTM
     C*** END ADD ***** 11/01/00 ***********************************************
     C*** BEG ADD ***** 09/01/00 ***********************************************
     C                   Z-ADD     FRTRMT        SCTRMT
     C*** END ADD ***** 09/01/00 ***********************************************
     C*** BEG ADD ***** 01/04/01 ***********************************************
     C                   MOVE      FROVRD        SCOVRD
     C*** END ADD ***** 01/04/01 ***********************************************
     C*** BEG ADD ***** 12/08/00 ***********************************************
     C                   MOVE      FRIGMD        SCIGMD
     C*** END ADD ***** 12/08/00 ***********************************************
     C*** BEG ADD ***** 02/27/04 ***********************************************
     C                   MOVE      FRMUNT        SCMUNT
     C*** END ADD ***** 02/27/04 ***********************************************
     C*
     C*    PROCESSING INFORMATION
     C*
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELFREOB     SCEOB
     C******************** MOVELFRMSG     SCMSG
     C******************** MOVELFRMSG2    SCMSG2
     C******************** MOVELFRMSG3    SCMSG3
     C*** END DEL *** 10/17/02 ****************************************
     C                   MOVEL     FRPMR         SCPMR
     C                   MOVEL     FRPCH         SCPCH
     C                   MOVEL     FRDIS         SCDIS
     C**** BEG ADD **** 10/16/98 **************************************
     C                   MOVE      FRRND         SCRND
     C**** END ADD **** 10/16/98 **************************************
     C**** BEG ADD **** 12/16/98 **************************************
     C                   MOVE      FRPFAC        SCPFAC
     C                   MOVE      FRPPDM        SCPPDM
     C**** END ADD **** 12/16/98 **************************************
     C*** BEG ADD **** 12/02/98 ***************************************
     C                   MOVE      FRPANE        SCPANE
     C*** END ADD **** 12/02/98 ***************************************
     C*** BEG ADD **** 07/02/99 ***************************************
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVE FRPAS2    SCPAS2
     C*** END DEL *** 10/17/02 ****************************************
     C*** END ADD **** 07/02/99 ***************************************
     C*** BEG ADD **** 11/04/99 ***************************************
     C                   MOVE      FRPPGP        SCPPGP
     C*** END ADD **** 11/04/99 ***************************************
     C*** BEG ADD ***** 12/02/99 ***********************************************
     C                   MOVE      FRPOUT        SCPOUT
     C*** END ADD ***** 12/02/99 ***********************************************
     C*** BEG ADD ***** 01/03/00 ***********************************************
     C                   MOVE      FRPDIF        SCPDIF
     C*** END ADD ***** 01/03/00 ***********************************************
     C*** BEG ADD ***** 02/16/00 ***********************************************
     C                   MOVEL     FRPSRV        SCPSRV
     C*** END ADD ***** 02/16/00 ***********************************************
     C*** BEG ADD ***** 09/26/00 ***********************************************
     C                   MOVEL     FRPASC        SCPASC
     C*** END ADD ***** 09/26/00 ***********************************************
     C*** BEG ADD ***** 07/13/01 ***********************************************
     C                   MOVE      FRPPSC        SCPPSC
     C                   MOVE      FRPDRG        SCPDRG
     C                   MOVE      FRPSNI        SCPSNI
     C                   MOVE      FRPSNO        SCPSNO
     C                   MOVE      FRPPHO        SCPPHO
     C                   MOVE      FRPPHI        SCPPHI
     C                   MOVE      FRPCRF        SCPCRF
     C                   MOVE      FRPHHI        SCPHHI
     C                   MOVE      FRPHHO        SCPHHO
     C                   MOVE      FRPORC        SCPORC
     C                   MOVE      FRPPIR        SCPPIR
     C                   MOVE      FRPPOR        SCPPOR
     C*** END ADD ***** 07/13/01 ***********************************************
     C*** BEG ADD ***** 10/17/02 ***********************************************
     C                   MOVE      FRPRX         SCPRX
     C*** END ADD ***** 10/17/02 ***********************************************
    C*** BEG ADD ***** 11/02/06 ***********************************************
     C                   MOVE      FR#DSD        SC#DSD
    C*** END ADD ***** 11/02/06 ***********************************************
    C*** Beg Add ***** 06/20/18 ***********************************************
     C*
       //*** Beg Chg *** 05/08/23 ***********************************************
     C***************    MOVEL     FREOB         SCEOB
       scEobCd1 = FReobCd1;
       scEobCd2 = FReobCd2;
       //*** End Chg *** 05/08/23 ***********************************************
     C                   MOVEL     FRMSG         SCMSG
     C                   MOVEL     FRMSG2        SCMSG2
     C                   MOVEL     FRMSG3        SCMSG3
     C                   MOVE      FRMR          SCMR
     C                   MOVE      FRPAS2        SCPAS2
     C                   MOVE      FRPPTV        SCPPTV
     C                   MOVE      FRPARE        SCPARE
    C*** End Add ***** 06/20/18 ***********************************************
     C*
     C*    SEARCHING FOR DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       SCSTDS
     C                   MOVE      *BLANKS       SCPCDS
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVE *BLANKS   SCBFDS
     C******************** MOVE *BLANKS   SCBTDS
     C*** END DEL *** 10/17/02 ****************************************
     C                   MOVE      *BLANKS       SCBTPD
     C                   MOVE      *BLANKS       SCBFPD
     C*** BEG DEL *** 07/13/01 ****************************************
     C******************** MOVE *BLANKS   SCBTSD
     C******************** MOVE *BLANKS   SCFBMD
     C******************** MOVE *BLANKS   SCTBMD
     C*** END DEL *** 07/13/01 ****************************************
     C*** Beg Del *** 06/20/18 ****************************************
     C****************   MOVE      *BLANKS       SCCMDD
     C*** End Del *** 06/20/18 ****************************************
     C*** Beg Del *** 06/20/18 ****************************************
     C****************   MOVE      *BLANKS       SCCPSD
     C*** End Del *** 06/20/18 ****************************************
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVE *BLANKS   SCCTSD
     C******************** MOVE *BLANKS   SCCFDS
     C******************** MOVE *BLANKS   SCCTDS
     C*** END DEL *** 10/17/02 ****************************************
     C*** BEG DEL *** 11/01/00 ****************************************
     C******************** MOVE *BLANKS   SCCTBD
     C*** END DEL *** 11/01/00 ****************************************
     C*** BEG ADD *** 10/17/02 ****************************************
     C*** Beg Del *** 06/20/18 ****************************************
     C****************   MOVE      *BLANKS       SCCTBD
     C*** End Del *** 06/20/18 ****************************************
     C*** Beg Del *** 06/20/18 ****************************************
     C****************   MOVE      *BLANKS       SCCPTD
     C*** End Del *** 06/20/18 ****************************************
     C*** END ADD *** 10/17/02 ****************************************
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVE *BLANKS   SCEOBD
     C*** END DEL *** 10/17/02 ****************************************
     C*** BEG DEL *** 07/13/01 ****************************************
     C******************** MOVE *BLANKS   SCMSGD
     C******************** MOVE *BLANKS   SCMSD2
     C******************** MOVE *BLANKS   SCMSD3
     C*** END DEL *** 07/13/01 ****************************************
     C*** Beg Add *** 06/20/18 ****************************************
       //*** Beg Chg *** 05/08/23**************************************
     C*************      MOVE      *BLANKS       SCEOBD
       scEOBd1 = *blanks;
       scEOBd2 = *blanks;
       //*** End Chg *** 05/08/23**************************************
     C                   MOVE      *BLANKS       SCMSGD
     C                   MOVE      *BLANKS       SCMSD2
     C                   MOVE      *BLANKS       SCMSD3
     C*** End Add *** 06/20/18 ****************************************
     C*
     C*   LOAD DESCRIPTIONS
     C                   EXSR      LDDES
     C                   ENDIF                                                  *IN40
     C*
     C                   TIME                    DFDTTM
     C*** BEG ADD *** 10/17/02 ****************************************
     C     @@MODE        IFEQ      'D'
     C     *INKL         ANDEQ     *ON
     C                   SUB       1             SCRN#
     C                   ENDIF
     C*** END ADD *** 10/17/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      SCST          FRST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   Z-ADD     XXDATE        FREFDT
     C                   Z-ADD     SCRULE        FRRULE
     C                   MOVE      SCPCAT        FRPCAT
     C                   MOVE      SCDESC        FRDESC
     C                   Z-ADD     SCPAGE        FRPAGE
     C                   MOVEL     SCSYS         FRSYS
     C                   MOVEL     SCINFO        FRINFO
     C*** BEG ADD *** 10/31/03 ****************************************
     C                   Z-ADD     SCCLT#        FRCLT#
     C*** END ADD *** 10/31/03 ****************************************
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     XXDATE        FREXDT
     C                   MOVE      SCPTY         FRPTY
     C                   Z-ADD     SCAMT         FRAMT
     C                   Z-ADD     SCPCT         FRPCT
     C                   MOVEL     SCPROF        FRPROF
     C                   MOVEL     SCTECH        FRTECH
     C                   Z-ADD     SCTMDV        FRTMDV
     C                   MOVEL     SCFUD         FRFUD
     C                   MOVEL     SCCT          FRCT
     C*** BEG DEL *** 10/04/02 ****************************************
     C******************** MOVELSCHTS1    FRHTOS
     C******************** MOVE SCHTS2    FRHTOS
     C*** END DEL *** 10/04/02 ****************************************
     C                   Z-ADD     SCCONV        FRCONV
     C                   Z-ADD     SCCON2        FRCON2
     C                   Z-ADD     SCFTM         FRFTM
     C                   Z-ADD     SCTTM         FRTTM
     C                   MOVEL     SCSPCD        FRSPCD
     C                   MOVEL     SCSPCD        FRSPCD
     C                   MOVEL     SCCPTS        FRCPTS
     C                   MOVEL     SCCSEP        FRCSEP
     C                   MOVEL     SCPCVL        FRPCVL
     C                   MOVEL     SCTCVL        FRTCVL
     C                   MOVEL     SCBSEP        FRBSEP
     C                   MOVEL     SCBRT         FRBRT
     C                   MOVEL     SCMAJ         FRMAJ
     C                   MOVEL     SCNCT         FRNCT
     C                   MOVEL     SCPCPT        FRPCPT
     C                   MOVEL     SCBSPB        FRBSPB
     C                   Z-ADD     SCUNIT        FRUNIT
     C                   Z-ADD     SCAUNT        FRAUNT
     C                   MOVEL     SCBAST        FRBAST
     C                   MOVEL     SCCAST        FRCAST
     C                   MOVEL     SCVAL         FRVAL
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELSCMR      FRMR
     C*** END DEL *** 10/17/02 ****************************************
     C**** BEG ADD **** 10/16/98 **************************************
     C                   MOVE      SCRND         FRRND
     C**** END ADD **** 10/16/98 **************************************
     C*
     C*    BILLED FIELDS
     C*
     C                   MOVEL     SCBFCD        FRBFCD
     C                   MOVEL     SCBTCD        FRBTCD
     C                   Z-ADD     SCBFPS        FRBFPS
     C                   Z-ADD     SCBTPS        FRBTPS
     C                   MOVEL     SCBTOS        FRBTOS
     C                   MOVEL     SCBPRT        FRBPRT
     C                   MOVEL     SCBFMD        FRBFMD
     C                   MOVEL     SCBTMD        FRBTMD
     C*** BEG ADD ***** 04/27/99 ***********************************************
     C                   MOVEL     SCCOMP        FRCOMP
     C                   MOVEL     SCAVAL        FRAVAL
     C*** END ADD ***** 04/27/99 ***********************************************
     C*** BEG ADD 09/09/99 ********************************************
     C                   MOVE      SCTANE        FRTANE
     C*** END ADD 09/09/99 ********************************************
     C*** BEG ADD ***** 09/29/99 ***********************************************
     C                   MOVE      SCPTYP        FRPTYP
     C*** BEG ADD ***** 09/29/99 ***********************************************
     C*** BEG ADD ***** 11/04/99 ***********************************************
     C                   Z-ADD     SCLOSS        FRLOSS
     C                   MOVE      SCTLOS        FRTLOS
     C                   MOVE      SCPGRP        FRPGRP
     C*** END ADD ***** 11/04/99 ***********************************************
     C*** BEG ADD ***** 04/08/00 ***********************************************
     C                   MOVE      SCBMRI        FRBMRI
     C*** END ADD ***** 04/08/00 ***********************************************
     C*** BEG ADD ***** 07/13/01 ***********************************************
     C                   MOVE      SCFTOB        FRFTOB
     C                   MOVE      SCTTOB        FRTTOB
     C                   MOVE      SCFEMA        FRFEMA
     C                   MOVE      SCPOMA        FRPOMA
     C                   MOVE      SCMEMA        FRMEMA
     C                   MOVE      SCSCMA        FRSCMA
     C*** BEG ADD ***** 04/03/08 ***********************************************
     C***** Beg Chg *** 05/30/16 ***********************************************
     C*****              MOVE      SCAPSI        FRAPSI
     C                   MOVE      SCAPSI        FRAPCSI
     C***** End Chg *** 05/30/16 ***********************************************
    C***** Beg Add *** 12/09/16 ***********************************************
     C                   MOVE      SCCAPCSI      FRCAPCSI
    C***** End Add *** 12/09/16 ***********************************************
     C                   MOVE      SCIMST        FRIMST
     C*** END ADD ***** 04/03/08 ***********************************************
     C*** BEG ADD ***** 11/14/08 ***********************************************
     C                   MOVEL     SCASPI        FRASPI
     C*** END ADD ***** 11/14/08 ***********************************************
     C                   MOVE      SCMRMA        FRMRMA
     C                   MOVE      SCDRMA        FRDRMA
     C                   MOVE      SCSVAL        FRSVAL
     C*** END ADD ***** 07/13/01 ***********************************************
     C*** BEG ADD ***** 10/11/01 ***********************************************
     C                   MOVE      SCBFUT        FRBFUT
     C*** END ADD ***** 10/11/01 ***********************************************
     C*** BEG ADD ***** 11/29/01 ***********************************************
     C                   MOVE      SCBDEX        FRBDEX
     C*** END ADD ***** 11/29/01 ***********************************************
     C*** BEG ADD *** 01/16/02 ****************************************
     C                   MOVE      SCCCOD        FRCCOD
     C*** END ADD *** 01/16/02 ****************************************
     C*** BEG ADD *** 10/04/02 ****************************************
     C                   MOVE      SCFTOS        FRFTOS
     C                   MOVE      SCFTSS        FRFTSS
     C                   MOVE      SCTTOS        FRTTOS
     C                   MOVE      SCTTSS        FRTTSS
     C*** END ADD *** 10/04/02 ****************************************
     C*** BEG ADD *** 09/15/03 ****************************************
     C                   MOVE      SCBMD2        FRBMD2
     C                   MOVE      SCBMD3        FRBMD3
     C                   MOVE      SCBMD4        FRBMD4
     C*** END ADD *** 09/15/03 ****************************************
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      SCPCIN        FRPCIN
     C                   MOVE      SC50IN        FR50IN
     C                   MOVE      SC51IN        FR51IN
     C                   MOVE      SC62IN        FR62IN
     C                   MOVE      SC66IN        FR66IN
     C                   MOVE      SC80IN        FR80IN
     C*** END ADD *** 09/25/03 ****************************************
     C*** BEG ADD *** 11/01/03 ****************************************
     C                   MOVE      SCBSRG        FRBSRG
     C*** END ADD *** 11/01/03 ****************************************
     C*** BEG ADD *** 11/05/03 ****************************************
     C                   Z-ADD     SCFADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRFADT
     C                   Z-ADD     SCTADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRTADT
     C*** END ADD *** 11/05/03 ****************************************
     C*** BEG ADD *** 09/25/04 ****************************************
     C                   MOVE      SCBFHE        FRBFHE
     C*** END ADD *** 09/25/04 ****************************************
     C*** Beg Add *** 09/01/15 ****************************************
     C                   Eval      FRBInvl = scbInvl
     C*** End Add *** 09/01/15 ****************************************
     C***** Beg Add *** 06/03/19 ***************************************
     C                   Eval      FRbfad = scBFAD
     C                   Eval      FRctpc = scCTPC
     C***** End Add *** 06/03/19 ***************************************
     C*
     C*    COMPARING FIELDS
     C*
     C                   MOVEL     SCCPOS        FRCPOS
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELSCCTOS    FRCTOS
     C*** END DEL *** 10/17/02 ****************************************
     C                   MOVEL     SCCTOB        FRCTOB
     C                   MOVEL     SCCSDT        FRCSDT
     C                   MOVEL     SCHSDT        FRCHDT
     C                   MOVEL     SCCSPR        FRCSPR
     C                   Z-ADD     SCCFDY        FRCFDY
     C                   Z-ADD     SCCPDY        FRCPDY
     C                   Z-ADD     SC#PRC        FR#PRC
     C                   Z-ADD     SC#DAY        FR#DAY
     C                   MOVEL     SC#GPC        FR#GPC
     C                   MOVEL     SC#GDY        FR#GDY
     C                   MOVEL     SCCFCD        FRCFCD
     C                   MOVEL     SCCTCD        FRCTCD
     C                   MOVEL     SCCMOD        FRCMOD
     C                   MOVEL     SCCDPR        FRCDPR
     C*** BEG ADD ***** 03/24/98 ***********************************************
     C                   MOVEL     SCEFCD        FREFCD
     C                   MOVEL     SCETCD        FRETCD
     C*** END ADD ***** 03/24/98 ***********************************************
     C*** BEG ADD ***** 11/24/98 ***********************************************
     C                   MOVE      SCHMRA        FRHMRA
     C*** END ADD ***** 11/24/98 ***********************************************
     C*** BEG ADD ***** 11/16/99 ***********************************************
     C                   MOVE      SCCSBL        FRCSBL
     C*** END ADD ***** 11/16/99 ***********************************************
     C*** BEG ADD ***** 02/16/00 ***********************************************
     C                   MOVE      SCCPRT        FRCPRT
     C*** END ADD ***** 02/16/00 ***********************************************
     C*** BEG ADD ***** 04/08/00 ***********************************************
     C                   MOVE      SCCMRI        FRCMRI
     C*** END ADD ***** 04/08/00 ***********************************************
     C*** BEG ADD ***** 11/01/00 ***********************************************
     C                   Z-ADD     SCPTTM        FRPTTM
     C*** END ADD ***** 11/01/00 ***********************************************
     C*** BEG ADD ***** 09/01/00 ***********************************************
     C                   Z-ADD     SCTRMT        FRTRMT
     C*** END ADD ***** 09/01/00 ***********************************************
     C*** BEG ADD ***** 01/04/01 ***********************************************
     C                   MOVE      SCOVRD        FROVRD
     C*** END ADD ***** 01/04/01 ***********************************************
     C*** BEG ADD ***** 12/08/00 ***********************************************
     C                   MOVE      SCIGMD        FRIGMD
     C*** END ADD ***** 12/08/00 ***********************************************
     C*** BEG ADD ***** 02/27/04 ***********************************************
     C                   MOVE      SCMUNT        FRMUNT
     C*** END ADD ***** 02/27/04 ***********************************************
     C*
     C*    PROCESSING INFORMATION
     C*
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELSCEOB     FREOB
     C******************** MOVELSCMSG     FRMSG
     C******************** MOVELSCMSG2    FRMSG2
     C******************** MOVELSCMSG3    FRMSG3
     C*** END DEL *** 10/17/02 ****************************************
     C                   MOVEL     SCPMR         FRPMR
     C                   MOVEL     SCPCH         FRPCH
     C                   MOVEL     SCDIS         FRDIS
     C**** BEG ADD **** 12/16/98 **************************************
     C                   MOVE      SCPFAC        FRPFAC
     C                   MOVE      SCPPDM        FRPPDM
     C**** END ADD **** 12/16/98 **************************************
     C*** BEG ADD **** 12/02/98 ***************************************
     C                   MOVE      SCPANE        FRPANE
     C*** END ADD **** 12/02/98 ***************************************
     C*** BEG ADD **** 07/02/99 ***************************************
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVE SCPAS2    FRPAS2
     C*** END DEL *** 10/17/02 ****************************************
     C*** END ADD **** 07/02/99 ***************************************
     C*** BEG ADD ***** 11/04/99 ***********************************************
     C                   MOVE      SCPPGP        FRPPGP
     C*** END ADD ***** 11/04/99 ***********************************************
     C*** BEG ADD ***** 12/02/99 ***********************************************
     C                   MOVE      SCPOUT        FRPOUT
     C*** END ADD ***** 12/02/99 ***********************************************
     C*** BEG ADD ***** 01/03/00 ***********************************************
     C                   MOVE      SCPDIF        FRPDIF
     C*** END ADD ***** 01/03/00 ***********************************************
     C*** BEG ADD ***** 02/16/00 ***********************************************
     C                   MOVE      SCPSRV        FRPSRV
     C*** END ADD ***** 02/16/00 ***********************************************
     C*** BEG ADD ***** 09/26/00 ***********************************************
     C                   MOVEL     SCPASC        FRPASC
     C*** END ADD ***** 09/26/00 ***********************************************
     C*** BEG ADD ***** 07/13/01 ***********************************************
     C                   MOVE      SCPPSC        FRPPSC
     C                   MOVE      SCPDRG        FRPDRG
     C                   MOVE      SCPSNI        FRPSNI
     C                   MOVE      SCPSNO        FRPSNO
     C                   MOVE      SCPPHO        FRPPHO
     C                   MOVE      SCPPHI        FRPPHI
     C                   MOVE      SCPHHI        FRPHHI
     C                   MOVE      SCPHHO        FRPHHO
     C                   MOVE      SCPORC        FRPORC
     C                   MOVE      SCPPIR        FRPPIR
     C                   MOVE      SCPPOR        FRPPOR
     C*** END ADD ***** 07/13/01 ***********************************************
     C*** BEG ADD ***** 10/17/02 ***********************************************
     C                   MOVE      SCPRX         FRPRX
     C                   MOVE      SCPCRF        FRPCRF
     C*** END ADD ***** 10/17/02 ***********************************************
    C*** BEG ADD ***** 11/02/06 ***********************************************
     C                   MOVE      SC#DSD        FR#DSD
    C*** END ADD ***** 11/02/06 ***********************************************
    C*** Beg Add ***** 06/20/18 ***********************************************
     C*
       //*** Beg Chg ***** 05/08/23 ********************************************
       //**********      MOVEL     SCEOB         FREOB
       FReobCd1 = scEobCd1;
       FReobCd2 = scEobCd2;
       //*** End Chg ***** 05/08/23 ********************************************
     C                   MOVEL     SCMSG         FRMSG
     C                   MOVEL     SCMSG2        FRMSG2
     C                   MOVEL     SCMSG3        FRMSG3
     C                   MOVE      SCMR          FRMR
     C                   MOVE      SCPAS2        FRPAS2
     C                   MOVE      SCPPTV        FRPPTV
     C                   MOVE      SCPARE        FRPARE
    C*** End Add ***** 06/20/18 ***********************************************
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
     C* CLEAR ERROR INDICATORS
     C*
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   SETOFF                                       010203
     C                   SETOFF                                       040506
     C*** END ADD *** 09/25/03 ****************************************
     C*** BEG ADD *** 11/05/03 ****************************************
     C                   SETOFF                                       0708
     C*** END ADD *** 11/05/03 ****************************************
     C                   SETOFF                                       535455
     C                   SETOFF                                       565758
     C                   SETOFF                                       596162
     C                   SETOFF                                       636465
     C                   SETOFF                                       6667
     C*
     C**********************************
     C*   SET OFF POSITIONING INDICATORS
     C**********************************
     C*** BEG ADD *** 11/01/03 ****************************************
     C                   SETOFF                                       10
     C*** END ADD *** 11/01/03 ****************************************
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   SETOFF                                       111213
     C                   SETOFF                                       141516
     C*** END ADD *** 09/25/03 ****************************************
     C*** BEG ADD *** 09/15/03 ****************************************
     C                   SETOFF                                       171819
     C*** END ADD *** 09/15/03 ****************************************
     C                   SETOFF                                       202122
     C                   SETOFF                                       232425
     C                   SETOFF                                       262728
     C                   SETOFF                                       293132
     C                   SETOFF                                       333435
     C                   SETOFF                                       363738
     C                   SETOFF                                       3942
     C                   SETOFF                                       434445
     C                   SETOFF                                       464748
     C                   SETOFF                                       495051
     C                   SETOFF                                       5268
     C*
     C*******************
     C* F10=SEARCH      *
     C*******************
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C* SET OFF POSITIONING INDICATORS
     C*
     C*** Beg Add *** 06/20/18 ***********************************
     C                   eval      isEob = *off
       //*** Beg Add *** 05/08/23 ********************************
       isEob2 = *off;
       //*** End Add *** 05/08/23 ********************************
     C                   eval      isMsg1 = *off
     C                   eval      isMsg2 = *off
     C                   eval      isMsg3 = *off
     C                   eval      isPayPtVs = *off
     C                   eval      isMiUrEtc = *off
     C                   eval      isMultRed = *off
     C                   eval      is2ndPass = *off
     C                   eval      isApcSi   = *off
     C                   eval      isImSt    = *off
     C                   eval      isAscPi   = *off
     C                   eval      isInvCd   = *off
     C                   eval      isDayFrmSv = *off
     C                   eval      isCmpApcSi = *off
     C*** End Add *** 06/20/18 ***********************************
     C*
     C*================================================================
     C* SCR01
     C*================================================================
     C***********************
     C*   PROCEDURE CATEGORY*
     C***********************
     C*
     C     HFLD          IFEQ      'SCPCAT'
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      'PCAT'        QQID
     C                   PARM      *BLANKS       QQKEY
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C     QQKEY         IFEQ      *BLANK
     C                   SETON                                        20
     C*
     C                   ELSE
     C                   SETON                                        21
     C*
     C                   MOVEL(P)  QQKEY         SCPCAT
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCPCDS
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  END CATEGORY
     C*
     C***********************
     C*   SYSTEM TYPE       *
     C***********************
     C*
     C     HFLD          IFEQ      'SCSYS '
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'ATYP'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCSYS
     C                   MOVEL     GTDESC        SCSYSD
     C                   SETON                                        9023
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9022
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C***********************
     C*   CODE TYPE         *
     C***********************
     C*
     C     HFLD          IFEQ      'SCCT  '
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'CTYP'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCCT
     C                   MOVEL     GTDESC        SCCTD
     C                   SETON                                        9024
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9023
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C***********************
     C*   SPECIALTY CODE    *
     C***********************
     C*
     C     HFLD          IFEQ      'SCSPCD'
     C*
     C                   CALL      'F2335R'
     C                   PARM      '2'           QQCALL
     C                   PARM      *BLANKS       QQKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCSPCD
     C                   SETON                                        9025
     C                   ELSE
     C                   SETON                                        9024
     C*
     C                   ENDIF                                                  ##KEY NE BLANKS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** BEG ADD *** 09/25/03 ****************************************
     C*
     C******************************
     C*   PC/TC INDICATOR SEARCH
     C******************************
     C*
     C     HFLD          IFEQ      'SCPCIN'
     C*
     C                   MOVEL(P)  'MIPC'        ##TBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPCIN
     C                   SETON                                        9012
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9011
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ENDIF                                                  HFLD IFEQ SCPCIN
     C*
     C******************************
     C*   BILAT SURG INDICATOR SEARCH
     C******************************
     C*
     C     HFLD          IFEQ      'SC50IN'
     C*
     C                   MOVEL(P)  'MI50'        ##TBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC50IN
     C                   SETON                                        9026
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9012
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ENDIF                                                  HFLD IFEQ SC50IN
     C*
     C******************************
     C*   MULTI SURG INDICATOR SEARCH
     C******************************
     C*
     C     HFLD          IFEQ      'SC51IN'
     C*
     C                   MOVEL(P)  'MI51'        ##TBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC51IN
     C                   SETON                                        9014
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9013
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ENDIF                                                  HFLD IFEQ SC51IN
     C*
     C******************************
     C*   CO SURG INDICATOR SEARCH
     C******************************
     C*
     C     HFLD          IFEQ      'SC62IN'
     C*
     C                   MOVEL(P)  'MI62'        ##TBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC62IN
     C                   SETON                                        9015
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9014
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ENDIF                                                  HFLD IFEQ SC62IN
     C*
     C******************************
     C*   TEAM SURG INDICATOR SEARCH
     C******************************
     C*
     C     HFLD          IFEQ      'SC66IN'
     C*
     C                   MOVEL(P)  'MI66'        ##TBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC66IN
     C                   SETON                                        9016
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9015
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ENDIF                                                  HFLD IFEQ SC66IN
     C*
     C******************************
     C*   ASST SURG INDICATOR SEARCH
     C******************************
     C*
     C     HFLD          IFEQ      'SC80IN'
     C*
     C                   MOVEL(P)  'MI80'        ##TBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC80IN
     C                   SETON                                        9010
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9016
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ENDIF                                                  HFLD IFEQ SC80IN
     C*** END ADD *** 09/25/03 ****************************************
     C***********************
     C*   BY REPORT
     C***********************
     C*
     C     HFLD          IFEQ      'SCBRT'
     C*
     C                   MOVEL(P)  'BYRP'        QQID              4
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANKS       QQRTCD            1
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCBRT
     C                   SETON                                        9027
     C*
     C                   ELSE
     C                   SETON                                        9026
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD=SCBRT
     C*** BEG ADD *** 04/03/08 ****************************************
     C***********************
     C*   APC SI
     C***********************
     C*
     C     HFLD          IFEQ      'SCAPSI'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'APSI'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCAPSI
     C*** Beg Add *** 06/20/18 ******************************************
     C                   SETON                                        90
     C                   Eval      isImSt = *on
     C*** End Add *** 06/20/18 ******************************************
     C*** Beg Del *** 06/20/18 ******************************************
     C****************   ELSE
     C****************   SETON                                        90
     C*** End Del *** 06/20/18 ******************************************
     C                   ENDIF
     C                   ELSE
     C*** Beg Add *** 06/20/18 ******************************************
     C                   SETON                                        90
     C                   Eval      isApcSi = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C                   ENDIF                                                  HFLD = SCAPSI
     C***********************
     C*   IMPLANT STATUS
     C***********************
     C*
     C     HFLD          IFEQ      'SCIMST'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'IMST'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCIMST
     C*** Beg Add *** 06/20/18 ****************************************
     C                   Eval      *in90 = *on
     C                   Eval      isAscPi = *on
     C*** End Add *** 06/20/18 ****************************************
     C                   ENDIF
     C*** Beg Add *** 06/20/18 ****************************************
     C                   Else
     C                   Eval      *in90 = *on
     C                   Eval      isImSt  = *on
     C*** End Add *** 06/20/18 ****************************************
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C                   ENDIF                                                  HFLD=SCIMST
     C*** END ADD *** 04/03/08 ****************************************
     C*
     C*** BEG ADD *** 11/14/08 ****************************************
     C***********************
     C*   ASC PMNT IND
     C***********************
     C*
     C     HFLD          IFEQ      'SCASPI'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'ASPI'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCASPI
     C*** Beg Add *** 06/20/18 ****************************************
     C                   Eval      *in90 = *on
     C                   Eval      isInvCd = *on
     C*** End Add *** 06/20/18 ****************************************
     C                   ELSE
     C*** Beg Del *** 06/20/18 ****************************************
     C**************     SETON                                        9091
     C*** End Del *** 06/20/18 ****************************************
     C                   ENDIF
     C*** Beg Add *** 06/20/18 ****************************************
     C                   Else
     C                   Eval      *in90 = *on
     C                   Eval      isAscPi = *on
     C*** End Add *** 06/20/18 ****************************************
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C                   ENDIF                                                  HFLD = SCASPI
     C*** END ADD *** 11/14/08 ****************************************
     C*
     C*================================================================
     C* SCR02
     C*================================================================
     C*
     C***********************
     C*   PROCEDURE CODE    *
     C***********************
     C*
     C     HFLD          IFEQ      'SCBFCD'
     C     HFLD          OREQ      'SCBTCD'
     C     HFLD          OREQ      'SCCFCD'
     C     HFLD          OREQ      'SCCTCD'
     C     HFLD          OREQ      'SCEFCD'
     C     HFLD          OREQ      'SCETCD'
     C     HFLD          OREQ      'SCPCPT'
     C*
     C                   EXSR      PCSCH
     C*
     C*  DETERMINE WHICH CALLED THE SEARCH AND RETURN CORRECT DESC
     C*
     C     QQCPT4        IFNE      *BLANKS
     C*
     C     HFLD          IFEQ      'SCBFCD'
     C                   MOVEL     QQCPT4        SCBFCD
     C*
     C                   SETON                                        28
     C*
     C                   ENDIF                                                   END SCBFCD
     C*
     C     HFLD          IFEQ      'SCBTCD'
     C                   MOVEL     QQCPT4        SCBTCD
     C*
     C                   SETON                                        31
     C*
     C                   ENDIF                                                   END SCBTCD
     C*
     C     HFLD          IFEQ      'SCCFCD'
     C                   MOVEL     QQCPT4        SCCFCD
     C                   SETON                                        21
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCCTCD'
     C                   MOVEL     QQCPT4        SCCTCD
     C*
     C                   SETON                                        22
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCEFCD'
     C*
     C                   MOVEL     QQCPT4        SCEFCD
     C                   SETON                                        24
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCETCD'
     C*
     C                   MOVEL     QQCPT4        SCETCD
     C                   SETON                                        25
     C*
     C                   ENDIF                                                  HFLD = SCETCD
     C*
     C     HFLD          IFEQ      'SCPCPT'
     C*
     C                   MOVEL     QQCPT4        SCPCPT
     C                   SETON                                        28
     C*
     C                   ENDIF                                                  HFLD = SCETCD
     C*
     C                   ELSE                                                   QQCPT4 = BLANKS
     C*
     C     HFLD          IFEQ      'SCBFCD'
     C*
     C                   SETON                                        27
     C*
     C                   ENDIF                                                   END SCBFCD
     C*
     C     HFLD          IFEQ      'SCBTCD'
     C*
     C                   SETON                                        32
     C*
     C                   ENDIF                                                   END SCBTCD
     C*
     C     HFLD          IFEQ      'SCCFCD'
     C*
     C                   SETON                                        20
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCCTCD'
     C*
     C                   SETON                                        21
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCEFCD'
     C*
     C                   SETON                                        23
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCETCD'
     C*
     C                   SETON                                        24
     C*
     C                   ENDIF                                                  HFLD = SCETCD
     C*
     C     HFLD          IFEQ      'SCPCPT'
     C*
     C                   SETON                                        27
     C*
     C                   ENDIF                                                  HFLD = SCETCD
     C*
     C                   ENDIF                                                  QQCPT NE BLANKS
     C*
     C                   ENDIF                                                  HFLD = SCBFCD
     C*
     C***********************************
     C*  PROCEDURE MODIFIER SEARCH      *
     C***********************************
     C*
     C     HFLD          IFEQ      'SCBFMD'
     C     HFLD          OREQ      'SCBTMD'
     C     HFLD          OREQ      'SCCMOD'
     C*** BEG ADD *** 09/15/03 ****************************************
     C     HFLD          OREQ      'SCBMD2'
     C     HFLD          OREQ      'SCBMD3'
     C     HFLD          OREQ      'SCBMD4'
     C*** END ADD *** 09/15/03 ****************************************
     C*
     C                   CALL      'F2315R'
     C                   PARM      '2'           QQCALL
     C                   PARM      ' '           QQKC
     C                   PARM      ' '           QQKL
     C                   PARM      SCST          QQST              2
     C                   PARM      *BLANKS       QQKEY            10
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         XXMOD             2
     C*
     C     HFLD          IFEQ      'SCBFMD'
     C                   MOVEL(P)  XXMOD         SCBFMD
     C                   SETON                                        29
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCBTMD'
     C                   MOVEL(P)  XXMOD         SCBTMD
     C                   SETON                                        33
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCCMOD'
     C                   MOVEL(P)  XXMOD         SCCMOD
     C                   SETON                                        30
     C                   ENDIF
     C*** BEG ADD *** 09/15/03 ****************************************
     C*
     C     HFLD          IFEQ      'SCBMD2'
     C                   MOVEL(P)  XXMOD         SCCMOD
     C                   SETON                                        18
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCBMD3'
     C                   MOVEL(P)  XXMOD         SCCMOD
     C                   SETON                                        19
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCBMD4'
     C                   MOVEL(P)  XXMOD         SCCMOD
     C                   SETON                                        13
     C                   ENDIF
     C*** END ADD *** 09/15/03 ****************************************
     C*
     C                   ELSE
     C*
     C     HFLD          IFEQ      'SCBFMD'
     C                   SETON                                        28
     C                   ENDIF                                                  HFLD IFEQ SCBFMD
     C*
     C     HFLD          IFEQ      'SCBTMD'
     C                   SETON                                        31
     C                   ENDIF                                                  HFLD IFEQ SCBFMD
     C*
     C     HFLD          IFEQ      'SCCMOD'
     C                   SETON                                        29
     C                   ENDIF                                                  HFLD IFEQ SCBFMD
     C*** BEG ADD *** 09/15/03 ****************************************
     C*
     C     HFLD          IFEQ      'SCBMD2'
     C                   SETON                                        17
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCBMD3'
     C                   SETON                                        18
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCBMD4'
     C                   SETON                                        19
     C                   ENDIF
     C*** END ADD *** 09/15/03 ****************************************
     C*
     C                   ENDIF                                                  QQKEY NE BLANKS
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  HFLD IFEQ SCBFM
     C*
     C************************
     C*   FROM TYPE OF SERVICE
     C************************
     C*
     C     HFLD          IFEQ      'SCFTOS'
     C*
     C                   MOVEL(P)  'SVTY'        QQID              4
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANKS       QQRTCD            1
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCFTOS
     C                   SETON                                        34
     C*
     C                   ELSE
     C                   SETON                                        33
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD=SCFTOS
     C*
     C************************
     C*   TO TYPE OF SERVICE
     C************************
     C*
     C     HFLD          IFEQ      'SCTTOS'
     C*
     C                   MOVEL(P)  'SVTY'        QQID              4
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANKS       QQRTCD            1
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCTTOS
     C                   SETON                                        35
     C*
     C                   ELSE
     C                   SETON                                        34
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD=SCTTOS
     C*
     C************************
     C*   FROM TYPE OF BILL
     C************************
     C*
     C     HFLD          IFEQ      'SCFTOB'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      BTTBID        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCFTOB
     C                   SETON                                        37
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        36
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCFTOB
     C*
     C************************
     C*   TO TYPE OF BILL
     C************************
     C*
     C     HFLD          IFEQ      'SCTTOB'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      BTTBID        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCTTOB
     C                   SETON                                        38
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        37
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCTTOB
     C*
     C************************
     C*   BILLED F/U TYPE
     C************************
     C*
     C     HFLD          IFEQ      'SCBFUT'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'FOLU'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCBFUT
     C                   SETON                                        43
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        42
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCBFUT
     C*
     C************************
     C*   DRUG TYPE
     C************************
     C*
     C     HFLD          IFEQ      'SCPTYP'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'PROD'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPTYP
     C                   SETON                                        46
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        45
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCPTYP
     C*
     C*
     C***************************
     C*   VALUE IN ANES, MRA, ETC
     C***************************
     C*
     C     HFLD          IFEQ      'SCAVAL'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'VALU'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCAVAL
     C                   SETON                                        49
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        48
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCAVAL
     C***********************
     C*   ANESTHESIA TYPE
     C***********************
     C*
     C     HFLD          IFEQ      'SCTANE'
     C*
     C                   MOVEL(P)  'ANTY'        QQID              4
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANKS       QQRTCD            1
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCTANE
     C                   SETON                                        45
     C*
     C                   ELSE
     C                   SETON                                        44
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD=SCTANE
     C*
     C***********************
     C*   PLACE OF SERVICE  *
     C***********************
     C     HFLD          IFEQ      'SCBFPS'
     C     HFLD          OREQ      'SCBTPS'
     C     HFLD          OREQ      'SCCPOS'
     C*
     C                   CALL      'F2370R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      ' '           QQKC              1
     C                   PARM      ' '           QQKL              1
     C                   PARM                    SCST              2
     C                   PARM      *BLANK        QQPOS             3
     C*
     C     QQPOS         IFNE      *BLANKS
     C*
     C     PSKY          CHAIN     PPS02                              99
     C                   MOVEL(P)  PSDESC        XXPSDS           40
     C*
     C     HFLD          IFEQ      'SCBFPS'
     C                   MOVEL(P)  QQPOS         SCBFPS
     C                   SETON                                        39
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCBTPS'
     C                   MOVEL(P)  QQPOS         SCBTPS
     C                   SETON                                        68
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCCPOS'
     C                   SETON                                        32
     C                   MOVEL(P)  QQPOS         SCCPOS
     C*
     C                   MOVEL     QQPOS         XXPOS             3 0
     C     PSKY          CHAIN     PPS02                              99
     C*
     C*** Beg Del *** 06/20/18 ************************************
     C*****************  MOVEL(P)  PSDESC        SCCPSD
     C*** End Del *** 06/20/18 ************************************
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     HFLD          IFEQ      'SCBFPS'
     C                   SETON                                        38
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCBTPS'
     C                   SETON                                        39
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCCPOS'
     C                   SETON                                        31
     C                   ENDIF
     C*
     C                   ENDIF                                                  QQPOS NE BLANKS
     C*
     C                   ENDIF                                                  END HLFD
     C*
     C******************
     C*   TOS SEARCH   *
     C******************
     C*
     C     HFLD          IFEQ      'SCBTOS'
     C*
     C                   CALL      'F2365R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCST          ##ST
     C*
     C     ##RRN         CHAIN     PTS01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     2             SUBST(P)  TSKEY:3       SCBTOS
     C*** BEG CHG *** 09/15/03 ****************************************
     C******************** SETON                     51
     C                   SETON                                        17
     C*** END CHG *** 09/15/03 ****************************************
     C*
     C                   ELSE
     C                   SETON                                        50
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ SCBTO
     C*
     C********************
     C*  PART CODE SEARCH*
     C********************
     C     HFLD          IFEQ      'SCBPRT'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCST          QQST
     C*
     C                   CALL      'F2325R'
     C                   PARM      '2'           QQCALL
     C                   PARM      ' '           QQKC
     C                   PARM      ' '           QQKL
     C                   PARM                    QQST
     C                   PARM      *BLANKS       QQKEY
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL(P)  QQKEY         SCBPRT
     C                   SETON                                        47
     C*
     C                   ELSE
     C                   SETON                                        46
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C************************
     C* BILLED RULE USE SEARCH
     C************************
     C     HFLD          IFEQ      'SCBMRI'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'RLUS'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCBMRI
     C                   SETON                                        36
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        35
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C************************
     C* PEER GROUP
     C************************
     C*
     C     HFLD          IFEQ      'SCPGRP'
     C*
     C                   CALL      'F2355R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCST
     C                   PARM      *BLANKS       QQKEY
     C                   PARM                    QQDESC           40
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCPGRP
     C                   SETON                                        48
     C*
     C                   ELSE
     C                   SETON                                        47
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C****************************
     C* CORRECT CODING FLAG SEARCH
     C****************************
     C*
     C     HFLD          IFEQ      'SCCCOD'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'CCRU'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCCCOD
     C                   SETON                                        50
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C                   SETON                                        52
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCCCOD
     C*
     C****************************
     C* STOP LOSS TYPE SEARCH
     C****************************
     C*
     C     HFLD          IFEQ      'SCTLOS'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'SLTY'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCTLOS
     C                   SETON                                        27
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        26
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCTLOS
     C*
     C***********************
     C*   TYPE OF BILL      *
     C***********************
     C     HFLD          IFEQ      'SCCTOB'
     C                   MOVEL(P)  BTTBID        QQID              4
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM      *BLANKS       QQKEY
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C*
     C                   MOVEL     QQKEY         SCCTOB
     C                   SETON                                        36
     C*
     C                   ELSE
     C                   SETON                                        35
     C*
     C                   ENDIF                                                  QQKEY NE BLANKS
     C*
     C                   ENDIF                                                  END CATEGORY
     C*
     C***************************
     C*   COMPARED TO PART SEARCH
     C***************************
     C*
     C     HFLD          IFEQ      'SCCPRT'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCST          QQST
     C*
     C                   CALL      'F2325R'
     C                   PARM      '2'           QQCALL
     C                   PARM      ' '           QQKC
     C                   PARM      ' '           QQKL
     C                   PARM                    QQST
     C                   PARM      *BLANKS       QQKEY
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL(P)  QQKEY         SCCPRT
     C                   SETON                                        34
     C*
     C* GET BODY AREA DESCRIPTION
     C*
     C     PTKY          CHAIN     PPA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*** Beg Del *** 06/20/18 ****************************************
     C****************** MOVEL     PADESC        SCCPTD
     C*** End Del *** 06/20/18 ****************************************
     C*
     C                   ELSE
     C*** Beg Del *** 06/20/18 ****************************************
     C*************      MOVE      *BLANKS       SCCPTD
     C*** End Del *** 06/20/18 ****************************************
     C*
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C                   ELSE
     C                   SETON                                        33
     C*
     C                   ENDIF                                                  QQKEY NE BLANK
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C***************************
     C*  COMPARED RULE USE SEARCH
     C***************************
     C*
     C     HFLD          IFEQ      'SCCMRI'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'RLUS'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCCMRI
     C                   SETON                                        28
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        27
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*================================================================
     C* SCR03
     C*================================================================
     C***********************************
     C*  PAY MRA SEARCH
     C***********************************
     C*
     C     HFLD          IFEQ      'SCPMR '
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'MRTY'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPMR
     C                   SETON                                        21
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        20
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCPMR
     C***********************
     C*  PAY CHARGED SEARCH
     C***********************
     C     HFLD          IFEQ      'SCPCH '
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'CHTY'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPCH
     C                   SETON                                        23
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        22
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCPCH
     C*
     C***********************************
     C*  PAY ANESTHESIA SEARCH
     C***********************************
     C*
     C     HFLD          IFEQ      'SCPANE'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'ANRU'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPANE
     C                   SETON                                        24
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        23
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCPANE
     C*
     C***********************************
     C*  ANESTHESIA ROUNDING TYPE
     C***********************************
     C*
     C     HFLD          IFEQ      'SCRND '
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'ANRD'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCRND
     C                   SETON                                        26
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        25
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCRND
     C*** BEG ADD *** 10/31/05 ****************************************
     C***********************
     C*   CODE TYPE         *
     C***********************
     C*
     C     HFLD          IFEQ      'SCPOUT'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'OUTL'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPOUT
     C                   SETON                                        9046
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9045
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** END ADD *** 10/31/05 ****************************************
     C*** BEG ADD *** 04/07/08 ****************************************
     C***********************
     C*   DRG RATE   SEARCH *
     C***********************
     C*
     C     HFLD          IFEQ      'SCPDRG'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'TDRG'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPDRG
     C                   SETON                                        9067
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9068
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** END ADD *** 04/07/08 ****************************************
     C*** BEG ADD *** 08/11/10 ****************************************
     C***********************************
     C*  PAY INPATIENT PER DIEM
     C***********************************
     C*
     C     HFLD          IFEQ      'SCPPDM'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'TYPD'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPPDM
     C                   SETON                                        95
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        92
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDIF                                                  HFLD=SCPPDM
     C*
     C*** END ADD *** 08/11/10 ****************************************
     C*
    C*** Beg Add *** 12/09/16 ****************************************
     C***********************
     C*   SCCAPCSI
     C***********************
     C*
     C     HFLD          IFEQ      'SCCAPCSI'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      'APSI'        ##TBID            4
     C*
     C     ##RRN         IfNe      *Zeros
     C     ##RRN         Chain     PGT02                              99
     C     *IN99         IfEq      *OFF
     C                   Movel     GTkey         scCAPCSI
     C*** Beg Add *** 06/20/18 ***************************************
     C                   Eval      isDayFrmSv = *on
     C                   Eval      *in90 = *on
     C*** End Add *** 06/20/18 ***************************************
     C                   Else
     C                   SetOn                                        90
     C                   Eval      erCAPCSI = *On
     C                   Eval      msCAPCSI = 'MC50000'
     C                   EndIf
     C*** Beg Add *** 06/20/18 ***************************************
     C                   Else
     C                   Eval      isCmpApcSi = *on
     C                   Eval      *in90 = *on
     C*** End Add *** 06/20/18 ***************************************
     C*
     C                   EndIf                                                  ##RRN <> 0
     C                   EndIf                                                  HFLD = SCCAPCSI
    C*** End Add *** 12/09/16 ****************************************
     C*** Beg Add *** 06/20/18 ****************************************
     C*
     C***********************************
     C*  PT PER VISIT SEARCH
     C***********************************
     C*
     C     HFLD          IFEQ      'SCPPTV'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'TYPT'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPPTV
     C                   SETON                                        90
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      isMiUrEtc = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        90
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      isPayPtVs = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF
     C*
     C***********************************
     C*  REGION SEARCH RURAL,URBAN,ETC  *
     C***********************************
     C*
     C     HFLD          IFEQ      'SCPARE'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'ZRAR'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPARE
     C                   SETON                                        90
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      isMultRed = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        90
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      isMiUrEtc = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF
     C*
     C***********************************
     C*  EOB CODE SEARCH                *
     C***********************************
     C*
       //*** Beg Chg *** 05/08/23**************************************
     C**** HFLD          IFEQ      'SCEOB'
       If Hfld = 'SCEOBCD1';
       //*** End Chg *** 05/08/23**************************************
     C*
     C                   CALL      'F2330R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM      '0'           QQKC              1
     C     *INKL         PARM      '0'           QQKL              1
     C                   PARM      @@ST          QQST              2
     C                   PARM      *BLANKS       QQKEY            10
     C*
     C     QQKEY         IFEQ      *BLANKS
     C*
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      isEOB = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ELSE
     C*
       //*** Beg Chg *** 05/08/23**************************************
     C***********        MOVEL(P)  QQKEY         SCEOB
       scEobCd1 = qqKey;
       //*** End Chg *** 05/08/23**************************************
     C*
     C     EBKY          CHAIN     PEB01                              60
       //*** Beg Chg *** 05/08/23**************************************
     C****************   MOVEL     EBDESC        SCEOBD
       scEOBd1 = EBdesc;
       //*** End Chg *** 05/08/23**************************************
     C*
     C*** Beg Add *** 06/20/18 ******************************************
     C*** Beg Chg *** 05/08/23 ******************************************
     C***********        Eval      isMsg1 = *on
       isEob2 = *on;
     C*** End Chg *** 05/08/23 ******************************************
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ENDIF                                                  QQKEY IFEQ BLAN
     C*
     C                   ENDIF                                                  HFLD IFEQ SCEOB
       //*** Beg Add *** 05/08/23**************************************
       If Hfld = 'SCEOBCD2';
       //
     C                   CALL      'F2330R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM      '0'           QQKC              1
     C     *INKL         PARM      '0'           QQKL              1
     C                   PARM      @@ST          QQST              2
     C                   PARM      *BLANKS       QQKEY            10
       //
          If qqKey = *blanks;
             isEob2 = *on;
          Else;
             scEobCd2 = qqKey;
             Chain ebKy peb01;
             scEOBd2 = EBdesc;
             isMsg1 = *on;
          Endif;
       Endif;
     C***********************************
     C*  ERROR MESSAGES                 *
     C***********************************
     C*
     C     HFLD          IFEQ      'SCMSG'
     C     HFLD          OREQ      'SCMSG2'
     C     HFLD          OREQ      'SCMSG3'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      'F2385R'
     C                   PARM      '2'           QQCALL
     C                   PARM      @@RRN3        QQRRN             9 0
     C                   PARM                    QQTYPE            1
     C                   PARM      SCST          QQST              2
     C                   PARM      *BLANKS       QQMSG#            7
     C*
     C* GET MESSAGE DESCRIPTION
     C*
     C     QQMSG#        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  QQMSG#        KKMSG#
     C*
     C     MIKY          CHAIN     PMI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  MIDESC        XXDESC
     C                   ELSE
     C                   MOVE      *BLANKS       XXDESC
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCMSG'
     C                   MOVEL(P)  QQMSG#        SCMSG
     C                   MOVEL(P)  XXDESC        SCMSGD
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      isMsg2 = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C     HFLD          WHENEQ    'SCMSG2'
     C                   MOVEL(P)  QQMSG#        SCMSG2
     C                   MOVEL(P)  XXDESC        SCMSD2
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      isMsg3 = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C     HFLD          WHENEQ    'SCMSG3'
     C                   MOVEL(P)  QQMSG#        SCMSG3
     C                   MOVEL(P)  XXDESC        SCMSD3
     C*** Beg Add *** 06/20/18 ******************************************
     C                   Eval      *in20 = *on
     C*** End Add *** 06/20/18 ******************************************
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCMSG'
     C*** Beg Add *** 06/20/18 ******************************************
     C                   eval      isMsg1 = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C     HFLD          WHENEQ    'SCMSG2'
     C*** Beg Add *** 06/20/18 ******************************************
     C                   eval      isMsg2 = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C     HFLD          WHENEQ    'SCMSG3'
     C*** Beg Add *** 06/20/18 ******************************************
     C                   eval      isMsg3 = *on
     C*** End Add *** 06/20/18 ******************************************
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  QQMSG# NE BLANK
     C*
     C                   ENDIF                                                  HFLD = SCMSG
     C*** End Add *** 06/20/18 *****************************************
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *INKJ
     C*
     C                   ENDSR
     C*================================================================
     C* LDDES -- Load Description fields
     C*================================================================
     C     LDDES         BEGSR
     C*** BEG ADD *** 10/17/02 ****************************************
     C*
     C***********************
     C*   SYSTEM TYPE       *
     C***********************
     C     SCSYS         IFNE      *BLANKS
     C                   MOVEL(P)  'ATYP'        QQID
     C                   MOVEL(P)  SCSYS         QQKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCSYSD
     C   99              MOVEL(P)  *BLANKS       SCSYSD
     C*
     C                   ENDIF                                                  SCSYS NE BLANKS
     C*
     C***********************
     C*   CODE TYPE         *
     C***********************
     C     SCCT          IFNE      *BLANKS
     C                   MOVEL(P)  'CTYP'        QQID
     C                   MOVEL(P)  SCCT          QQKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCCTD
     C   99              MOVEL(P)  *BLANKS       SCCTD
     C*
     C                   ENDIF                                                  SCCT  NE BLANKS
     C*** END ADD *** 10/17/02 ****************************************
     C*
     C***********************
     C*   TYPE OF BILL      *
     C***********************
     C*
     C     SCCTOB        IFNE      *BLANKS
     C*** BEGIN CHG *** 11/08/00 **************************************
     C**********           MOVE 'BLTY'    QQID    4
     C                   MOVEL(P)  BTTBID        QQID
     C*** END   CHG *** 11/08/00 **************************************
     C                   MOVEL     SCCTOB        QQKEY
     C*** BEG DEL *** 11/01/00 ****************************************
     C********** GTKY      CHAINPGT01                60
     C**N60*****           MOVELGTDESC    SCCTBD    P
     C*** END DEL *** 11/01/00 ****************************************
     C*** BEG ADD *** 10/17/02 ****************************************
     C     GTKY          CHAIN     PGT01                              99
     C*** Beg Del *** 06/20/18 ****************************************
     C**N99***           MOVEL(P)  GTDESC        SCCTBD
     C*** End Del *** 06/20/18 ****************************************
     C*** END ADD *** 10/17/02 ****************************************
     C*
     C                   ENDIF                                                  END CATEGORY
     C*
     C***********************
     C*   PROCEDURE CODE    *
     C***********************
     C*
     C     SCBFCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCBFCD        QQCPT4           11
     C                   EXSR      CHKPRC
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELXXDESC    SCBFDS    P
     C*** END DEL *** 10/17/02 ****************************************
     C                   ENDIF
     C*
     C     SCBTCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCBTCD        QQCPT4
     C                   EXSR      CHKPRC
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELXXDESC    SCBTDS    P
     C*** END DEL *** 10/17/02 ****************************************
     C                   ENDIF
     C*
     C     SCCFCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCFCD        QQCPT4
     C                   EXSR      CHKPRC
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELXXDESC    SCCFDS    P
     C*** END DEL *** 10/17/02 ****************************************
     C                   ENDIF
     C*
     C     SCCTCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCTCD        QQCPT4
     C                   EXSR      CHKPRC
     C*** BEG DEL *** 10/17/02 ****************************************
     C******************** MOVELXXDESC    SCCTDS
     C*** END DEL *** 10/17/02 ****************************************
     C                   ENDIF
     C*
     C***********************
     C*   PROCEDURE CATEGORY*
     C***********************
     C*
     C     SCPCAT        IFNE      *BLANKS
     C*
     C                   MOVE      'PCAT'        QQID
     C                   MOVEL(P)  SCPCAT        QQKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCPCDS
     C*
     C                   ENDIF                                                  END CATEGORY
     C***********************
     C*   PLACE OF SERVICE  *
     C***********************
     C     SCBFPS        IFGT      0
     C                   Z-ADD     SCBFPS        XXPOS             3 0
     C     PSKY          CHAIN     PPS02                              99
     C  N99              MOVEL(P)  PSDESC        SCBFPD           40
     C                   ENDIF
     C*
     C     SCBTPS        IFGT      0
     C                   Z-ADD     SCBTPS        XXPOS
     C     PSKY          CHAIN     PPS02                              99
     C  N99              MOVEL(P)  PSDESC        SCBTPD           40
     C                   ENDIF
     C*
     C     SCCPOS        IFGT      0
     C                   Z-ADD     SCCPOS        XXPOS
     C     PSKY          CHAIN     PPS02                              99
     C*** BEG CHG *** 10/17/02 ****************************************
     C**N99*************** MOVELPSDESC    SCCPSD 40 P
     C*** Beg Del *** 06/20/18 ************************************
     C**N99****          MOVEL(P)  PSDESC        SCCPSD
     C*** eND Del *** 06/20/18 ************************************
     C*** END CHG *** 10/17/02 ****************************************
     C                   ENDIF
     C***********************
     C*   BODY AREA         *
     C***********************
     C     SCCPRT        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCPRT        QQKEY
     C*
     C     PTKY          CHAIN     PPA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*** Beg Del *** 06/20/18 ****************************************
     C****************   MOVEL(P)  PADESC        SCCPTD
     C*** End Del *** 06/20/18 ****************************************
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCCPRT NE BLANK
     C*
     C***********************
     C*   SPECIALTY CODE    *
     C***********************
     C*
     C     SCSPCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCSPCD        QQKEY
     C     SPKY          CHAIN     PSP01                              99
     C*
     C                   ENDIF                                                  HFLD
     C******************
     C*   TOS SEARCH   *
     C******************
     C*
     C*** BEG DEL *** 07/13/01 ****************************************
     C***********SCBTOS*   IFNE *BLANKS
     C******************
     C******************   MOVELSCBTOS    QQKEY     P
     C***********TSKY***   CHAINPTS02                60
     C**N60*************** MOVE TSDESC    SCBTSD    P
     C******************   END                             END SCBTOS
     C*** END DEL *** 07/13/01 ****************************************
     C*
     C*** BEG DEL *** 10/17/02 ****************************************
     C**********
     C********** SCCTOS    IFNE *BLANKS
     C**********
     C**********           MOVELSCCTOS    QQKEY     P
     C********** TSKY      CHAINPTS01                60
     C**N60*****           MOVE TSDESC    SCCTSD    P
     C**********           END                             END SCBTOS
     C*** END DEL *** 10/17/02 ****************************************
     C*
     C***********************************
     C*  EOB CODE SEARCH                *
     C***********************************
     C*** BEG DEL *** 10/17/02 ****************************************
     C**********
     C********** SCEOB     IFNE *BLANKS
     C**********
     C**********           MOVELSCEOB     QQKEY     P
     C********** EOBKY     CHAINPEB01                60
     C**********           MOVELEBDESC    SCEOBD
     C**********           ENDIF
     C**********
     C**********
     C***********************************
     C***ERROR*MESSAGES                 *
     C***********************************
     C********** SCMSG     IFNE *BLANKS
     C**********           MOVELSCMSG     XXMSG   7 P
     C********** MGKY      CHAINPMI01                60
     C****BEG*DEL *** 07/13/01 ****************************************
     C******************** MOVELMIDESC    SCMSGD    P
     C****END*DEL *** 07/13/01 ****************************************
     C**********           ENDIF
     C**********
     C********** SCMSG2    IFNE *BLANKS
     C**********           MOVELSCMSG2    XXMSG     P
     C********** MGKY      CHAINPMI01                60
     C****BEG*DEL *** 07/13/01 ****************************************
     C******************** MOVELMIDESC    SCMSD2    P
     C****END*DEL *** 07/13/01 ****************************************
     C**********           ENDIF
     C**********
     C********** SCMSG3    IFNE *BLANKS
     C**********           MOVELSCMSG3    XXMSG     P
     C********** MGKY      CHAINPMI01                60
     C****BEG*DEL *** 07/13/01 ****************************************
     C******************** MOVELMIDESC    SCMSD3    P
     C****END*DEL *** 07/13/01 ****************************************
     C**********           ENDIF
     C*** END DEL *** 10/17/02 ****************************************
     C***********************************
     C*  PROCEDURE MODIFIERS            *
     C***********************************
     C     SCBFMD        IFNE      *BLANKS
     C                   MOVEL(P)  SCBFMD        QQKEY
     C     MDKY          CHAIN     PPM01                              99
     C*** BEG DEL *** 07/13/01 ****************************************
     C******************** MOVELPMDESC    SCFBMD    P
     C*** END DEL *** 07/13/01 ****************************************
     C                   ENDIF
     C*
     C     SCBTMD        IFNE      *BLANK
     C                   MOVEL(P)  SCBTMD        QQKEY
     C     MDKY          CHAIN     PPM01                              99
     C                   MOVEL     PMDESC        SCTBMD           40
     C                   ENDIF
     C*
     C     SCCMOD        IFNE      *BLANK
     C                   MOVEL(P)  SCCMOD        QQKEY
     C     MDKY          CHAIN     PPM01                              99
     C*** BEG CHG *** 12/08/00 ****************************************
     C******************** MOVELPMDESC    SCCMDD 40
     C*** Beg Del *** 06/20/18 ****************************************
     C*****************  MOVEL     PMDESC        SCCMDD
     C*** End Del *** 06/20/18 ****************************************
     C*** END CHG *** 12/08/00 ****************************************
     C                   ENDIF
     C*** BEG ADD *** 10/17/02 ****************************************
     C*
     C* GET DESCRIPTION FOR STATE
     C*
     C                   MOVEL     ##ST          KKST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STNAME        SCSTDS
     C                   ENDIF
     C*
     C* GET DESCRIPTION FOR PROCEDURE CATEGORY
     C*
     C                   MOVEL(P)  SCPCAT        QQKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCPCDS
     C***********************************
     C*  EOB CODE DESCRIPTIONS          *
     C***********************************
     C*** Beg Add *** 06/20/18 ****************************************
       //*** Beg Chg *** 05/08/23 *************************************
     C**** SCEOB         IFNE      *BLANKS
     C****
     C****               MOVEL(P)  SCEOB         QQKEY
     C**** EOBKY         CHAIN     PEB01                              60
     C****               MOVEL     EBDESC        SCEOBD
     C****               ENDIF
       If scEobCd1 <> *blanks;
          qqKey = scEobCd1;
          Chain EobKy peb01;
          If %Found(FTABL);
             scEobd1 = EBdesc;
          Endif;
       Endif;
       If scEobCd2 <> *blanks;
          qqKey = scEobCd2;
          Chain EobKy peb01;
          If %Found(FTABL);
             scEobd2 = EBdesc;
          Endif;
       Endif;
       //*** End Chg *** 05/08/23 *************************************
     C
     C***********************************
     C*  ERROR MESSAGES                 *
     C***********************************
     C     SCMSG         IFNE      *BLANKS
     C                   MOVEL(P)  SCMSG         XXMSG             7
     C     MGKY          CHAIN     PMI01                              60
     C                   MOVEL(P)  MIDESC        SCMSGD
     C                   ENDIF
     C*
     C     SCMSG2        IFNE      *BLANKS
     C                   MOVEL(P)  SCMSG2        XXMSG             7
     C     MGKY          CHAIN     PMI01                              60
     C                   MOVEL(P)  MIDESC        SCMSD2
     C                   ENDIF
     C*
     C     SCMSG3        IFNE      *BLANKS
     C                   MOVEL(P)  SCMSG3        XXMSG             7
     C     MGKY          CHAIN     PMI01                              60
     C                   MOVEL(P)  MIDESC        SCMSD3
     C                   ENDIF
     C*** End Add *** 06/20/18 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* CHECK SYSTEM TYPE TO DETERMINE WHICH PROCEDURE TO USE
     C*================================================================
     C     CHKPRC        BEGSR
     C*
     C** CHECK SYSTEM TYPE
     C     SCSYS         IFEQ      'P'
     C     SCCT          IFEQ      'R'
     C                   EXSR      RCDES
     C                   ELSE
     C                   EXSR      CPTDES
     C                   ENDIF                                                  SCCT = R
     C                   ELSE
     C     SCSYS         IFEQ      'H'
     C                   EXSR      SEVDES
     C                   ENDIF                                                  SCSYS = H
     C                   ENDIF                                                  SCSYS = P
     C*
     C                   ENDSR
     C*================================================================
     C* CPT DESCRIPTION AND ERROR CHECKING
     C*================================================================
     C     CPTDES        BEGSR
     C*
     C** GET DESCRIPTION
     C                   MOVEL     QQCPT4        XXCPT4            5
     C*
     C                   MOVE      SCEFDT        ZZDATE            8 0
     C                   Z-ADD     ZZDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1EFDT
     C     PRKY          SETGT     PPR01
     C     NXTPR1        TAG
     C                   READP     PPR01                                  99
     C     *IN99         IFEQ      '0'
     C     PRCO#         ANDEQ     QQCO#
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        IFNE      'I'
     C                   MOVEL(P)  PRDSC1        XXDESC           50
     C                   ELSE
     C                   GOTO      NXTPR1
     C                   END
     C                   ELSE
     C*** BEG CHG *** 04/26/99 ****************************************
     C                   MOVEL     *BLANK        XXDESC
     C                   MOVEL(P)  'UNLISTED'    XXDESC
     C*** END CHG *** 04/26/99 ****************************************
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* PROCEDURE CODE SEARCHES FOR R&C AND CPT4 AND SERCICE CODE
     C*================================================================
     C     PCSCH         BEGSR
     C*
     C                   MOVE      *BLANKS       QQCPT4
     C*
     C** CHECK SYSTEM TYPE
     C*
     C     SCSYS         IFEQ      'P'
     C     SCCT          IFEQ      'R'
     C*
     C                   CALL      'F2310R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM      *ZEROS        QQRRN             9 0
     C*
     C     QQRRN         IFNE      *ZEROS
     C*
     C     QQRRN         CHAIN     PRC03                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCCPT4        QQCPT4
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   ENDIF                                                  QQRRN NE 0
     C*
     C                   ELSE
     C*
     C                   CALL      'F2305R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQST              2
     C                   PARM      *BLANKS       XXCPT4
     C                   PARM      *BLANKS       QQDESC           40
     C*
     C                   MOVEL(P)  XXCPT4        QQCPT4
     C*
     C                   ENDIF                                                  SCCT = R
     C*
     C                   ELSE
     C     SCSYS         IFEQ      'H'
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVE      *ZEROS        XXFDAT            7 0
     C                   CALL      'F2320R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQST              2
     C                   PARM      *BLANKS       QQCMSV           15
     C                   PARM      *BLANKS       QQCDSC           40
     C*
     C                   MOVEL(P)  QQCMSV        QQCPT4
     C*
     C                   ELSE
     C*
     C* PHARMACY SEARCH
     C*
     C     SCSYS         IFEQ      'X'
     C*
     C                   CALL      'F2350R'
     C                   PARM      '2'           QQCALL
     C                   PARM      ' '           QQKC
     C                   PARM      ' '           QQKL
     C                   PARM      *ZEROS        QQRRN
     C*
     C     QQRRN         IFNE      *ZEROS
     C     QQRRN         CHAIN     PRX01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         QQCPT4
     C                   ENDIF
     C*
     C                   ENDIF                                                  QQRRN <> 0
     C*
     C                   ENDIF                                                  SCSYS = X
     C*
     C                   ENDIF                                                  SCSYS = H
     C*
     C                   ENDIF                                                  SCSYS = P
     C*
     C                   ENDSR
     C*================================================================
     C* CODE TYPE = R, START WITH CODES IN REASONABLE & CUSTOMERY RANGE
     C*================================================================
     C     RCDES         BEGSR
     C*
     C                   MOVE      '292'         XXZIPA
     C                   MOVEL     QQCPT4        XXCPT4
     C*
     *** BEG CHG *** 05/12/98 *****************************************
     C******************** MOVE SCEFDT    XXDATE
     C*******************  Z-ADDXXDATE    SRDTI
     C                   MOVE      SCEFDT        ZZDATE
     C                   Z-ADD     ZZDATE        SRDTI
     *** END CHG *** 05/12/98 *****************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1EFDT
     C     RCKY          SETGT     PRC02
     C     NXTRC1        TAG
     C                   READP     PRC02                                  99
     C     *IN99         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCCPT4        ANDEQ     XXCPT4
     C     RCSTAT        IFNE      'I'
     C                   MOVEL(P)  RCDSC1        XXDESC           50
     C                   ELSE
     C                   GOTO      NXTRC1
     C                   END
     C                   ELSE
     C                   MOVEL     *BLANK        XXDESC
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* SYSTEM TYPE = H, START WITH CODES IN SERVICE CODE RANGE
     C*================================================================
     C     SEVDES        BEGSR
     C*
     C** GET DESCRIPTION
     C*
     *** BEG CHG *** 05/12/98 *****************************************
     C******************** MOVE SCEFDT    XXDATE
     C*******************  Z-ADDXXDATE    SRDTI
     C                   MOVE      SCEFDT        ZZDATE
     C                   Z-ADD     ZZDATE        SRDTI
     *** END CHG *** 05/12/98 *****************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1EFDT
     C                   MOVE      QQCPT4        XXCMSV
     C     SVKY          SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  99
     C     *IN99         IFEQ      '0'
     C     SVCO#         ANDEQ     QQCO#
     C     SVCMSV        ANDEQ     QQCPT4
     C     SVSTAT        IFNE      'I'
     C                   MOVEL(P)  SVDESC        XXDESC           50
     C                   ELSE
     C                   GOTO      NXTSV1
     C                   END
     C                   ELSE
     C                   MOVEL     *BLANK        XXDESC
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   CLOSE     FMPRL
     C                   CLOSE     FTABL
     C                   CLOSE     FMSVL1
     C                   MOVE      *BLANKS       OPNFLG
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'P'                                    41
     C*
     C                   MOVEL     @@CO#         QQCO#             3 0
     C*** BEGIN ADD *** 11/08/00 **************************************
     C     @@ST          CAT(P)    'BT':0        BTTBID            4
     C                   MOVEL(P)  BTTBID        QQID
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        QQID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        BTTBID
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 11/08/00 **************************************
     C*
     C*   CRTOVERIDES PER STATE
     C*
     C*** OVERRIDE DATABASE FILE FMPRL
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(1)        @@CMD
     C                   MOVEL     @@ST          XXST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*
     C*** OVERRIDE DATABASE FILE FTABL
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     @@ST          XXST
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*
     C*** OVERRIDE DATABASE FILE FMSVL1
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(3)        @2CMD
     C                   MOVEL     @@ST          X2ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @2CMD
     C                   PARM                    LENGTH           15 5
     C*
     C     OPNFLG        IFEQ      *BLANKS
     C                   OPEN      FMPRL
     C                   OPEN      FTABL
     C                   OPEN      FMSVL1
     C                   MOVE      'Y'           OPNFLG            1
     C                   ENDIF
     C*** BEG ADD *** 10/17/02 ****************************************
     C                   MOVE      @@ST          ##ST
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@RRN3        ##RRN3
     C*
     C                   Z-ADD     0             SCRN#
     C*
     C*** END ADD *** 10/17/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*** BEG ADD *** 07/15/03 ****************************************
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*** END ADD *** 07/15/03 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   MOVEL(P)  Q1USER        FRUSER
     C                   MOVEL(P)  Q1PGM         FRPGM
     C                   MOVEL     DFTIME        FRTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     XXDATE        FRDT
     C*
     C                   UPDATE    DBRCDF
     C*
     C*** BEG ADD *** 10/17/02 ****************************************
     C*** Beg Del *** 06/20/18 ****************************************
     C*****************  EXSR      CALLR
     C*** End Del *** 06/20/18 ****************************************
     C*** END ADD *** 10/17/02 ****************************************
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*** BEG CHG *** 10/17/02 ****************************************
     C******************** CLEARDBRCDF
     C  N41              CLEAR                   DBRCDF
     C*** END CHG *** 10/17/02 ****************************************
     C                   EXSR      MVSCDB
     C                   MOVEL(P)  Q1USER        FRUSER
     C                   MOVEL(P)  Q1PGM         FRPGM
     C                   MOVEL     DFTIME        FRTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     XXDATE        FRDT
     C                   MOVEL(P)  Q1USER        FRIUSR
     C                   MOVEL(P)  Q1PGM         FRIPGM
     C                   MOVEL     DFTIME        FRITIM
     C                   Z-ADD     XXDATE        FRIDT
     C*** BEG ADD *** 10/04/03 ****************************************
     C                   MOVEL     'A'           FRSTAT
     C*** END ADD *** 10/04/03 ****************************************
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FDFRP
     C*
     C                   MOVE      '1'           OUTFLG
     C*
     C*** BEG ADD *** 10/17/02 ****************************************
     C*** Beg Del *** 06/20/18 ****************************************
     C*****************  EXSR      CALLR
     C*** End Del *** 06/20/18 ****************************************
     C*** END ADD *** 10/17/02 ****************************************
     C*
     C                   ENDSR
**  QCMDEXC TABLE
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
