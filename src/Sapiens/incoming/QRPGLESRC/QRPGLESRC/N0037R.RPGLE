     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N0037R - WORK WITH NETWORK CONTRACTS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the Provider        **
     H**     Network info                                            **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F0032R - Work with Provider Master File                 **
     H**   CALLS TO:                                                 **
     H**     N0037R01 - Work With Network Contract Information       **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:    1/15/98      PROJECT: 60MBS                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 02/14/23 C:079 Support: 114/107109
     H*                Display correct description
     H*
     H* 06/09/20 C:212 Project : SY420
     H*                Mask SSN
     H*                Recompile only
     H*
     H* 04/05/11 C:198 Support: 76328
     H*                Fixed the display of network code description
     H*
     H* 08/12/10 C:198 Project: 114MWS
     H*                Incorporated the ability not to display discount
     H*                types DR/DS if state is not set up for medicare
     H*                based PPO Processing.
     H*
     H* 05/19/03 C:62 Project: NW076
     H*               DON'T ALLOW 2=CHANGE OR 20=CHG STATUS IS
     H*               CONTRACT IS INACTIVE
     H*
     H* 12/20/02 C:79 Project: NW075
     H*               ADD PARM TO PASS MANUAL FEE SCHEDULE FLAG
     H*
     H* 10/25/02 C:62 Project: MC044 34665
     H*               -CHANGE SF1PAG FROM 10 TO 8 TO MATCH WHAT
     H*                ACTUALLY DISPLAYS ON THE SCREEN
     H*               -EXTEND KEY FOR PAGING
     H*
     H* 04/24/00 C:62 Project: MC229
     H*               CHANGE WHAT NETWORK CONTRACTS WILL BE SHOWN FOR
     H*               SELECTION.
     H*
     H* 02/08/00 C:31 Project: NWMNT (BASE ONLY)
     H*               NTWK Maintenance
     H*               1. Add column for "Manual FS" flag
     H*
     H* 07/15/99 C:62 PROJ:MC209
     H*               ADD SUBSET AND PARM FOR CALL FROM PROVIDER BILLS.
     H*
     H* 07/09/99 C:62 PROJ:MC209
     H*               ADD FOLD/TRUNCATE.
     H*
     H* 03/03/98 C:62 PROJ:MC145
     H*               ** DROPPED DISCOUNT % IN WORK WITH.
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      01   Used on the PAGEDOWN keyword
     H**      02   Used on the PAGEUP keyword
     H**      03   Used to condition SFLCLR
     H**     N03   Used to condition SFLDSPCTL
     H**      04   Used to condition SFLDSP
     H**      05   Used to condition DSPATR(RI) on the sel field
     H**     N06   Used to condition DSPATR(PC) on the pos to field
     H**      07   Used to condition DSPATR(PC) on the sel field
     H**      08   Used to condition SFLNXTCHG
     H**      09   Reserved for maintenance/inquiry conditions
     H**     N10   Used to condition SFLEND (on message subfile)
     H**      12   Used to condition SFLDROP/SFLFOLD
     H**      13   Used to condition SFLCLR (on confirm delete)
     H**     N13   Used to condition SFLDSPCTL (on confirm delete)
     H**      14   Used to condition SFLDSP (on confirm delete)
     H**      15   Used to condition SFLEND (on confirm delete)
     H**      21   View 1 - Network code sequence
     H**      22 - 29 are reserved for other views
     H**      60   Used for chaining
     H**      71   Reserved for screen error indicator
     H**      73 - 89 are reserved for screen error indicators
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FMS4P  (PFRCDF) Provider Network Contract              **
     H**      FMS4L1 (DBRCDF) Provider Network Contract              **
     H**                      (FEIN/MOD/NWCD/CNT#)                   **
     H**      FMPDL  (PPD01)  Provider Naster File (FEIN/MOD)        **
     H*****************************************************************
     H*
     FN0037F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMS4P     IF   E             DISK
     F                                     RENAME(PS401:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEGIN CHG *** 10/25/02 **************************************
     F**********FMS4L1  IF  E           K        DISK
     FFMS4L6    IF   E           K DISK
     F*** END   CHG *** 10/25/02 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PS401:DBRCDF)
     F*  Provider Master file
     FFMPDL     IF   E           K DISK
     F*** BEGIN ADD *** 07/09/99 **************************************
     FNMBDLD    IF   E           K DISK
     F*** END   ADD *** 07/09/99 **************************************
     F*** BEGIN ADD *** 07/09/99 **************************************
     FFMF1L     IF   E           K DISK
     FNMNWP     IF   E             DISK
     FNMNWL     IF   E           K DISK
     F                                     RENAME(PNW01:PNW02)
     FFTGTL     IF   E           K DISK
     F*** END   ADD *** 07/09/99 **************************************
     F*** Beg Add *** 08/12/10 ****************************************
     FSTSTL     IF   E           K DISK
     F*** End Add *** 08/12/10 ****************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*** BEGIN ADD *** 07/15/99 **************************************
     D*** END   ADD *** 07/15/99 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Network Contracts')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('N0037R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D*** BEGIN CHG *** 10/25/02 **************************************
     D**********    10                    C         SF1PAG
     D SF1PAG          C                   CONST(8)
     D*** END   CHG *** 10/25/02 **************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For message number sequence
     D* ---------------------------------------------------------------
     D                 DS
     D  X1DATA                 1     73
     D  X1NWCD                 1      5
     D  X1CNT#                 7     21
     D  X1SYS                 24     24
     D  X1STYP                28     30
     D  X1BGMM                34     35
     D  X1BGF1                36     36
     D  X1BGDD                37     38
     D  X1BGF2                39     39
     D  X1BGYY                40     43
     D  X1ENMM                45     46
     D  X1ENF1                47     47
     D  X1ENDD                48     49
     D  X1ENF2                50     50
     D  X1ENYY                51     54
     D*** BEGIN DEL *** 07/09/99 **************************************
     D**********                             57  59 X1DISC
     D*** END   DEL *** 07/09/99 **************************************
     D*** BEGIN CHG *** 03/03/98 **************************************
     D**********                             63  66 X1DILL
     D**********                             67  67 X1DIF1
     D**********                             68  69 X1DIRR
     D**********                             72  72 X1STAT
     D*** BEGIN CHG *** 02/08/00 **************************************
     D********                               63  63 X1STAT
     D  X1FSFL                60     60
     D  X1STAT                69     69
     D*** END CHG *** 02/08/00 ****************************************
     D*** END   CHG *** 03/03/98 **************************************
     D*
     D* ---------------------------------------------------------------
     D* Work area for dates and discount
     D* ---------------------------------------------------------------
     D                 DS
     D  X2BGDT                 1      8
     D  X2BGMM                 1      2
     D  X2BGDD                 3      4
     D  X2BGYY                 5      8
     D*
     D                 DS
     D  X2ENDT                 1      8
     D  X2ENMM                 1      2
     D  X2ENDD                 3      4
     D  X2ENYY                 5      8
     D*** BEGIN DEL *** 03/03/98 **************************************
     D*
     D**********  DS
     D**********                              1   6 X2DISP
     D**********                              1   4 X2DILL
     D**********                              5   6 X2DIRR
     D*** END   DEL *** 03/03/98 **************************************
     D*** BEGIN ADD *** 07/15/99 **************************************
     D*
     D                 DS
     D  FN                     1     50
     D                                     DIM(10)                              FILE NETWORK  s
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D*
     D                 DS
     D  SN                     1     50
     D                                     DIM(10)                              SCRN NETWORK  s
     D  SCNW01                 1      5
     D  SCNW02                 6     10
     D  SCNW03                11     15
     D  SCNW04                16     20
     D  SCNW05                21     25
     D  SCNW06                26     30
     D  SCNW07                31     35
     D  SCNW08                36     40
     D  SCNW09                41     45
     D  SCNW10                46     50

     I*** END   ADD *** 07/15/99 **************************************

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*                                                    '1' = Inquiry
     C*                                                    '2' = Search
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQNWCD            5
     C                   PARM                    QQCNT#           15
     C*** BEGIN ADD *** 07/15/99 **************************************
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQTYPE            1
     C*** END   ADD *** 07/15/99 **************************************
     C*** BEG ADD *** 12/20/02 ****************************************
     C                   PARM                    QQFSFL            1            MANUAL FS
     C*** END ADD *** 12/20/02 ****************************************
     C*** Beg Add *** 08/12/10 ****************************************
     C                   PARM                    @@AST             2            Adj. state
     C                   PARM                    @@DSTY            2            disc code type
     C*** End Add *** 08/12/10 ****************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    QQSTCD            1
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
     C*          FMS4L1 (DBRCDF)  Provider Network Contract
     C*
     C*                           (FEIN/MOD)
     C     KEY1          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*                           (FEIN/MOD/NWCD/CNT#)
     C     KEY2          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKNWCD
     C                   KFLD                    KKCNT#
     C*** BEGIN ADD *** 10/25/02 **************************************
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBGDT
     C*** END   ADD *** 10/25/02 **************************************
     C*
     C*          FMPDL  (PPD01)  Provider Naster File (FEIN/MOD)
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*** BEGIN ADD *** 07/15/99 **************************************
     C*
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C*
     C     S4KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    FN(X)
     C*
     C     NWKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSNTWK
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END   ADD *** 07/15/99 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    S4FEIN        KKFEIN
     C     *LIKE         DEFINE    S4MOD         KKMOD
     C     *LIKE         DEFINE    S4NWCD        KKNWCD
     C     *LIKE         DEFINE    S4CNT#        KKCNT#
     C*** BEGIN ADD *** 07/15/99 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END   ADD *** 07/15/99 **************************************
     C*** BEGIN ADD *** 10/25/02 **************************************
     C     *LIKE         DEFINE    S4SYS         KKSYS
     C     *LIKE         DEFINE    S4BGDT        KKBGDT
     C*** END   ADD *** 10/25/02 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*** BEGIN ADD *** 07/15/99 **************************************
     C     SSNTWK        IFNE      *BLANKS
     C     SSSYS         ORNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C*
     C                   SETON                                        11
     C*
     C                   ENDIF
     C*** END   ADD *** 07/15/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 07/15/99 **************************************
     C*
     C* MAKE SURE IF NETWORK IS SELECTED, IT EXISTS FOR THE GROUP
     C*
     C     SCSEL         IFEQ      '1'
     C*
     C                   Z-ADD     1             X
     C*
     C     SCNWCD        LOOKUP    FN(X)                                  99
     C*
     C  N99              SETON                                        9071
     C*
     C                   ENDIF
     C*** END   ADD *** 07/15/99 **************************************
     C*** BEGIN ADD *** 05/19/03 **************************************
     C*
     C* IF STATUS IS INACTIVE THEN DO NOT ALLOW CHANGE OR STATUS CHANGE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSTAT        ANDEQ     'I'
     C                   SELECT
     C     SCSEL         WHENEQ    '2'
     C                   SETON                                        7290
     C     SCSEL         WHENEQ    '20'
     C                   SETON                                        7390
     C                   ENDSL
     C                   ENDIF
     C*** END   ADD *** 05/19/03 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C  N90
     CAN KJ              EXSR      KJDO
     C*
     C  N90SSNTWK        IFNE      *BLANKS
     C*
     C     NWKY          CHAIN     PNW02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        8890
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  NWNAME        SCNWDS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C*
     C                   SETON                                        8990
     C*
     C                   ENDIF
     C*
     C  N90              SETOFF                                       3132
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*** BEGIN ADD *** 07/15/99 **************************************
     C     Q1PRMS        IFGT      7
     C*
     C* POPULATE GROUP NETWORK SCREEN FIELDS
     C*
     C     F1KY          CHAIN     PF101                              99
     C*
     C                   MOVEA     FN            SN
     C                   Z-ADD     QQFND         SCFND
     C                   SETON                                        16
     C*
     C* GET SYSTEM TYPE
     C*
     C     QQTYPE        IFEQ      'H'
     C*
     C                   MOVEL(P)  'H'           SSSYS
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'P'           SSSYS
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  SSSYS         KKKEY
     C                   MOVEL(P)  'SYS'         KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  GTDESC        SCSYDS
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCSYDS
     C*
     C                   ENDIF                                                  *IN99 = Off
     C*
     C*** BEGIN ADD *** 04/24/00 **************************************
     C     ##NWSL        IFNE      '1'
     C*** END   ADD *** 04/24/00 **************************************
     C*
     C* FIND HIGHEST PRIORITY NETWORK CODE
     C*
     C     1             DO        10            X                 2 0
     C*
     C     FN(X)         IFEQ      *BLANKS
     C*
     C                   MOVEL     FN(1)         SSNTWK
     C                   LEAVE
     C*
     C                   ELSE
     C*
     C     S4KY          CHAIN     DBRCDF                             99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     SSSYS         IFEQ      S4SYS
     C*
     C  N99              MOVEL     FN(X)         SSNTWK
     C  N99              LEAVE
     C*
     C                   ENDIF
     C*
     C     S4KY          READE     DBRCDF                                 99
     C*
     C                   ENDDO
     C*
     C     SSNTWK        IFNE      *BLANKS
     C*
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     NWKY          CHAIN     PNW02                              99
     C*
     C  N99              MOVEL(P)  NWNAME        SCNWDS
     C   99              MOVEL(P)  *BLANKS       SCNWDS
     C*** BEGIN ADD *** 04/24/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 04/24/00 **************************************
     C*
     C                   ENDIF
     C*
     C* IF CALL AS SEARCH DEFAULT STATUS TO 'A'
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*** END   ADD *** 07/15/99 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQFEIN        KKFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   MOVE      QQMOD         KKMOD
     C                   MOVE      *LOVAL        KKNWCD
     C                   MOVE      *LOVAL        KKCNT#
     C*** BEGIN ADD *** 10/25/02 **************************************
     C                   MOVE      *LOVAL        KKSYS
     C                   MOVE      *LOVAL        KKBGDT
     C*** END   ADD *** 10/25/02 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKNWCD
     C   21              MOVE      SCCNT#        KKCNT#
     C*** BEGIN ADD *** 10/25/02 **************************************
     C                   MOVE      *LOVAL        KKSYS
     C                   MOVE      *LOVAL        KKBGDT
     C*** END   ADD *** 10/25/02 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      QQFEIN        KKFEIN
     C                   MOVE      QQMOD         KKMOD
     C                   MOVEL     X1NWCD        KKNWCD
     C                   MOVE      X1CNT#        KKCNT#
     C*** BEGIN ADD *** 10/25/02 **************************************
     C                   MOVE      SCSYS         KKSYS
     C                   Z-ADD     SCBGDT        KKBGDT
     C*** END   ADD *** 10/25/02 **************************************
     C                   ENDSR
     C*================================================================
     C* KJDO - F10 = SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SSNTWK'
     C*
     C                   CALL      'N2325R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        3190
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PNW01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     NWNWCD        SSNTWK
     C                   MOVEL     NWNAME        SCNWDS
     C*
     C                   ENDIF                                                  *IN60 = Off
     C*
     C                   SETON                                        32
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
     C                   CLEAR                   X1DATA
     C*** BEGIN ADD *** 10/25/02 **************************************
     C                   MOVE      S4SYS         SCSYS
     C                   Z-ADD     S4BGDT        SCBGDT
     C*** END   ADD *** 10/25/02 **************************************
     C*** BEGIN ADD *** 05/19/03 **************************************
     C                   MOVEL     S4STAT        SCSTAT
     C*** END   ADD *** 05/19/03 **************************************
     C                   MOVEL(P)  S4NWCD        X1NWCD
     C                   MOVEL(P)  S4NWCD        SCNWCD
     C                   MOVEL(P)  S4CNT#        X1CNT#
     C                   MOVEL(P)  S4CNT#        SCCNT#
     C                   MOVEL(P)  S4SYS         X1SYS
     C                   MOVEL(P)  S4STYP        X1STYP
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         X2BGDT
     C                   MOVEL(P)  X2BGMM        X1BGMM
     C                   MOVEL(P)  X2BGDD        X1BGDD
     C                   MOVEL(P)  X2BGYY        X1BGYY
     C     X2BGDT        IFNE      *BLANKS
     C                   MOVEL     '/'           X1BGF1
     C                   MOVEL     '/'           X1BGF2
     C                   ENDIF                                                  BGDT <> blanks
     C                   Z-ADD     S4ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         X2ENDT
     C                   MOVEL(P)  X2ENMM        X1ENMM
     C                   MOVEL(P)  X2ENDD        X1ENDD
     C                   MOVEL(P)  X2ENYY        X1ENYY
     C     X2ENDT        IFNE      *BLANKS
     C                   MOVEL     '/'           X1ENF1
     C                   MOVEL     '/'           X1ENF2
     C                   ENDIF                                                  BGDT <> blanks
     C*** BEGIN DEL *** 07/09/99 **************************************
     C**********           MOVELS4DISC    X1DISC    P
     C*** END   DEL *** 07/09/99 **************************************
     C*** BEGIN DEL *** 03/03/98 **************************************
     C**********           MOVELS4DISP    X2DISP    P
     C**********           MOVELX2DILL    X1DILL    P
     C**********           MOVELX2DIRR    X1DIRR    P
     C**********           MOVEL'.'       X1DIF1
     C*** END   DEL *** 03/03/98 **************************************
     C                   MOVEL(P)  S4STAT        X1STAT
     C*** BEGIN ADD *** 02/08/00 **************************************
     C                   MOVEL(P)  S4FSFL        X1FSFL
     C*** END ADD *** 02/08/00 ****************************************
     C*** BEG ADD *** 12/20/02 ****************************************
     C     Q1PRMS        IFGT      10
     C                   MOVE      S4FSFL        QQFSFL
     C                   ENDIF
     C*** END ADD *** 12/20/02 ****************************************
     C                   MOVEL(P)  X1DATA        SCDATA
     C*** BEGIN ADD *** 07/09/99 **************************************
     C                   MOVEL(P)  S4DISC        SCDAT2
     C*
     C*             Get Discount code description
     C*
     C*** Beg Chg *** 04/05/11 ****************************************
     C********   S4DISC    CHAINPBD01                99
     C*
       //*** Beg Del *** 02/14/23 *********************************************
     C**** S4DISC        SETGT     PBD01                              99
     C**** S4DISC        READPE    PBD01                                  99
       //*** End Del *** 02/14/23 *********************************************
     C*** End Chg *** 04/05/11 ****************************************
       //*** Beg Del *** 02/14/23 *********************************************
     C******IN99         IFEQ      *OFF
     C*
     C******             MOVE      BDCOM1        SCDAT2
     C*
     C******             ENDIF                                                  *IN60 = Off
       //*** End Del *** 02/14/23 *********************************************
     C*** END   ADD *** 07/09/99 **************************************
       //*** Beg Add *** 02/14/23 *********************************************
       Setgt (S4disc:S4bgDt) pbd01;
       Readpe (S4disc) pbd01;
       Dow not %Eof(NMBDLD);
          If BDstat <> 'I';
             scDat2 = BDcom1;
             Leave;
          Endif;
          Readpe (S4disc) pbd01;
       Enddo;
       //*** End Add *** 02/14/23 *********************************************
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C*          Process option 20 - Change status
     C*          ------------------------------------------------------
     C     OTHOPT        BEGSR
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF                                                  SEL = '20'
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** BEGIN ADD *** 08/26/99 **************************************
     C     QQNWCD        IFEQ      *BLANKS
     C     *INKL         ANDEQ     *OFF
     C     @@CALL        ANDEQ     '2'
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           RSTPGM
     C*
     C                   ELSE
     C*
     C                   SETON                                        LR
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETON                                        LR
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 08/26/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     PDKY          CHAIN     PPD01                              60
     C  N60              MOVEL     PDNAME        SCNAME
     C                   MOVEL(P)  HD(2)         SCHDL2
     C*** BEGIN ADD *** 04/24/00 **************************************
     C                   CALL      'SA009R'
 1   C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
 2   C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
 3   C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
 4   C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
 5   C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
 6   C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
 7   C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
 8   C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
 9   C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
10   C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
11   C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
12   C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
13   C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C*
     C                   MOVE      QQNWCD        XXNWCD            5
     C                   MOVEL     *BLANKS       QQNWCD
     C*** END   ADD *** 04/24/00 **************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C     *IN21         IFEQ      *ON
     C                   MOVEL(P)  'DELETED'     X1CNT#
     C                   MOVEL     *BLANK        X1NWCD
     C                   MOVEL     *BLANK        X1SYS
     C                   MOVEL     *BLANK        X1STYP
     C                   MOVEL     *BLANK        X1BGMM
     C                   MOVEL     *BLANK        X1BGDD
     C                   MOVEL     *BLANK        X1BGYY
     C                   MOVEL     *BLANK        X1BGF1
     C                   MOVEL     *BLANK        X1BGF2
     C                   MOVEL     *BLANK        X1ENMM
     C                   MOVEL     *BLANK        X1ENDD
     C                   MOVEL     *BLANK        X1ENYY
     C                   MOVEL     *BLANK        X1ENF1
     C                   MOVEL     *BLANK        X1ENF2
     C*** BEGIN DEL *** 03/03/98 **************************************
     C**********           MOVEL*BLANK    X1DILL
     C**********           MOVEL*BLANK    X1DIF1
     C**********           MOVEL*BLANK    X1DIRR
     C*** END   DEL *** 03/03/98 **************************************
     C                   MOVEL     *BLANK        X1STAT
     C                   ENDIF                                                  *IN21 = On
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   MOVEL     SCNWCD        QQNWCD
     C                   MOVEL     SCCNT#        QQCNT#
     C***
     C*** Beg Add *** 08/12/10 ****************************************
     C     Q1PRMS        IFGT      12
     C     3             SUBST     SCDAT2        XXDISC            3
     C     S4DISC        CHAIN     PBD01                              99
     C  N99              MOVEL     BDTYPE        @@DSTY
     C                   ENDIF
     C*** End Add *** 08/12/10 ****************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*          S4FEIN    IFNE KKFEIN
     C*          S4MOD     ORNE KKMOD
     C*                    MOVE *OFF      MEETSS
     C*                    ENDIF                           FEIN <> FEIN
     C*
     C*** BEGIN ADD *** 07/15/99 **************************************
     C     SSNTWK        IFNE      *BLANKS
     C     SSNTWK        ANDNE     S4NWCD
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF                                                  FEIN <> FEIN
     C*
     C     SSSYS         IFNE      *BLANKS
     C     SSSYS         ANDNE     S4SYS
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF                                                  FEIN <> FEIN
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     S4STAT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF                                                  FEIN <> FEIN
     C*** BEGIN ADD *** 04/24/00 **************************************
     C     ##NWSL        IFEQ      '1'
     C     XXNWCD        ANDEQ     '*SPEC'
     C     SSNTWK        OREQ      *BLANKS
     C     XXNWCD        ANDEQ     '*SPEC'
     C     ##NWSL        OREQ      ' '
     C     SSNTWK        ANDNE     *BLANKS
     C     XXNWCD        ANDEQ     '*SPEC'
     C*
     C* SUBSET TO ALL NETWORKS SUPPORTED BY GROUP
     C*
     C                   Z-ADD     1             X
     C                   SETOFF                                       17
     C*
     C     S4NWCD        LOOKUP    FN(X)                                  17
     C*
     C     *IN17         IFEQ      *OFF
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 04/24/00 **************************************
     C*** Beg Add *** 08/12/10 ****************************************
     C*
     C     Q1PRMS        IFGT      11
     C*
     C                   MOVEL     @@AST         STKEY            10
     C     STKEY         CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STMPPO        ANDNE     'Y'
     C     *IN99         IFEQ      *OFF
     C     S4DISC        CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C     BDTYPE        IFEQ      'DR'
     C     BDTYPE        OREQ      'DS'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  BDTYPE = DR/DS
     C                   ENDIF                                                  chain on NMBDP
     C                   ENDIF                                                  STMPPO <> 'Y'
     C                   ENDIF                                                  chain on STSTP
     C*
     C                   ENDIF                                                  q1prms > 11
     C*** End Add *** 08/12/10 ****************************************
     C*** END   ADD *** 07/15/99 **************************************
     C*
     C                   ENDSR
** Column headings
Opt  Ntwk  Contract #      Sys  type  Begin Date  End Date   FS flag    Sts
                                Sub-   Contract   Contract   Manual
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
