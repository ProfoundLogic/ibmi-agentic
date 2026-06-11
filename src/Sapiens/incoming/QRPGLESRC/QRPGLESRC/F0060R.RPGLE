     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0060R - WORK WITH GROUPS   C:62 12/05/97                    *
     H*                                                               *
     H*  CALLED FROM: MANY PLACES                                     *
     H*  CALLS TO:                                                    *
     H*               F0060R01                                        *
     H*               F0060R02                                        *
     H*               F0060R03                                        *
     H*               F0061R                                          *
     H*               F0063R                                          *
     H*               F0120R01 - WCMC Control Information             *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 12/18/24 C:342 Project: Phase 2 MC447
     H*                This program is no longer in use.
     H*                Pass-through this program to new screen.
     H*                Add extra entry parm to avoid Pass-through of old screen
     H*
     H*  03/30/09 C:198 SUPPORT: MC330                                *
     H*                 MADE AN ENTRY FOR OPTION 18 IN COMPILE TIME   *
     H*                 ARRAY AT THE BOTTOM OF THE PROGRAM             *
     H*                                                               *
     H*  06/01/07 C:62  PROJECT: MC099 58565                          *
     H*                 CORRECT MULTI-GROUP FLAG USAGE.               *
     H*                                                               *
     H*  08/13/06 C:62  PROJECT: 139MEM                               *
     H*                 ADD OPTION 18=GROUP SERVICE CODE BILLINGS     *
     H*                                                               *
     H*  12/31/02 C:62  PROJECT: MC254                                *
     H*                 ADD OPTION 90=OUTCOME/SAVINGS CODES           *
     H*                                                               *
     H*  10/29/02 C:62  PROJECT: MC253                                *
     H*                 ADD MULTI GROUP RESTRICTIONS LOGIC            *
     H*                                                               *
     H*  08/25/00 C:62  PROJECT: MC095 24409                          *
     H*                 MAKE OPTION "7' NO LONGER CONDITIONED ON A/R. *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  12/28/99 C:62  PROJECT:PR001                                 *
     H*                 ADD ADDITIONAL VIEW FOR GROUP SHORT NAME      *
     H*                                                               *
     H*  06/30/99 C:62  PROJECT:MC206                                 *
     H*                 ADDED LOGIC TO DEAL WITH DIFFERENT CONDITIONS *
     H*                 OF CALL FROM MENU VS OTHER PROGRAMS.          *
     H*                                                               *
     H*  04/13/99 C:56  CHANGE  COMPANY API TO FA007R PER MIKE FLOWER *
     H*                 FA011R WAS A DUPLICATE OF FA007R              *
     H*                                                               *
     H*  04/02/99 C:62 PROJECT: MC095 17425                           *
     H*                IF OPTION '9' IS USED, MAKE SURE F0061R IS THE *
     H*                ONLY PROGRAM CALLED.                           *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED OPTION 14=DARTS TO PROGRAM               *
     H*                                                               *
     H*  10/09/98 C:53                                                *
     H*                REMOVE CONDITIONING ON OPTION 9                *
     H*                                                               *
     H*  10/02/98 C:112 PROJECT: MC152                                *
     H*                MAKE OPTION 16 -  GROUP EOMB COMMMENTS         *
     H*                                                               *
     H*  08/06/98 C:94                                                *
     H*                MAKE OPT11 - GROUP T/M RATES CONDITIONAL ON    *
     H*                SFBPF/FBAL01, POS.9                            *
     H*                MAKE FUNCTION 12 AVAILABLE                     *
     H*                                                               *
     H*  07/20/98 C:62 PROJECT: MC177                                 *
     H*                CHECK SMUPL TO DETERMINE IF USER IS RESTRICTED *
     H*                AND IF SO ONLY ALLOW THE USER TO SEE THE SUB-  *
     H*                SCRIBER AND/OR THE GROUP THEY ARE RESTRICTED TO*
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*    *IN16 = IS USED TO INDICATE IF GROUP IS RESTRICTED.        *
     H*                                                               *
     H*****************************************************************
     FF0060F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFN01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMFNL3    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PFN01:DBRCDF2)
     F*** BEGIN ADD *** 12/28/99 **************************************
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMFNL2    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PFN01:DBRCDF3)
     F*** END   ADD *** 12/28/99 **************************************
     FFMF1L     IF   E           K DISK
     FFMSMP     IF   E             DISK
     FFMSML     IF   E           K DISK
     F                                     RENAME(PSM01:PSM02)
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FSTSTL     IF   E           K DISK
     FFMCXL     IF   E           K DISK
     F*** BEGIN CHG *** 12/28/99 **************************************
     F**********                                    KINFDS DBFDS3
     F                                     INFDS(DBFDS4)
     F*** END   CHG *** 12/28/99 **************************************
     F*** BEGIN ADD *** 07/20/98 **************************************
     FSMUPL     IF   E           K DISK
     F*** END   ADD *** 07/20/98 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(3) CTDATA PERRCD(1)              Column headings
     D*** BEG CHG *** 12/01/98 ***************************************
     D*** BEG CHG *** 03/30/09 ***************************************
     D**********          OF      1  22  1   OFD    35   Options, Function
     D**********          OF      1  20  1   OFD    35   Options, Function
     D OF              S              1    DIM(23) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(23) ALT(OF)
     D*** END CHG *** 03/30/09 ***************************************
     D*** END CHG *** 12/01/98 ***************************************
     D NZ              S              1    DIM(3)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Groups')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0060R01')
     D ACPGM1          C                   CONST('F0060R02')
     D ACPGM2          C                   CONST('F0060R03')
     D ACPGM3          C                   CONST('F0061R')
     D ACPGM4          C                   CONST('F0120R01')
     D***              Modes:  S = Display single pgm (NOT workflow series)
     D***              Modes:  T = Change single pgm (NOT workflow series)
     D*** BEG CHG *** 04/13/99 ****************************************
     D**********    'FA011R'              C         COMAPI
     D COMAPI          C                   CONST('FA007R')
     D*** END CHG *** 04/13/99 ****************************************
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D*** BEGIN ADD *** 06/30/99 **************************************
     D CANFKY          C                   CONST('12F12=Cancel        -
     D                                            YYN')

     I*** END   ADD *** 06/30/99 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
      **** Begin Add **** 12/18/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 12/18/24 ******************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*
     C*  Work with State information
     C*
     C     ACDPL1        PLIST
     C*** BEGIN CHG *** 04/02/99 **************************************
     C**********           PARM @@CALL    ##CALL  1
     C                   PARM                    ##CALL            1
     C*** END   CHG *** 04/02/99 **************************************
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C*  WCMC Control information
     C     ACDPL4        PLIST
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      XXRRN         ##RRN             9 0
     C                   PARM      SCSUB#        QQSUB#            5 0
     C                   PARM      SCFND         QQFND             3 0
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
     C*** BEGIN ADD *** 04/02/99 **************************************
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C*
     C                   MOVEL     'S'           XXMODE            1
     C                   MOVEL     '1'           @@MODE            1
     C*
     C     @@MODE        WHENEQ    'T'
     C*
     C                   MOVEL     'T'           XXMODE
     C                   MOVEL     'C'           @@MODE
     C*
     C     @@MODE        WHENEQ    'A'
     C*
     C                   MOVEL     'A'           XXMODE            1
     C                   MOVEL     'C'           @@MODE
     C*
     C                   OTHER
     C*
     C                   MOVEL     'O'           XXMODE
     C*
     C                   ENDSL
     C*** END   ADD *** 04/02/99 **************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     RSTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C*
     C     RSTPLS        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'M'           ##APPL
     C                   PARM      *OFF          ##RTCD
     C                   PARM      *BLANK        ##AUTH
     C                   PARM      *ZERO         #1FND
     C                   PARM      *ZERO         #1EMP#
     C                   PARM      SMSUB#        #1SUB#
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKNAME
     C*** BEGIN ADD *** 12/28/99 **************************************
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKSHNM
     C*** END   ADD *** 12/28/99 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           KKKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    FNFND
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*** Key list to WCMC Control File
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     UPKY          KLIST
     C                   KFLD                    KKUSR
     C*** END   ADD *** 07/20/98 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    FNNAME        KKNAME
     C     *LIKE         DEFINE    STKEY         KKST
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C*** END   ADD *** 07/20/98 **************************************
     C*** BEGIN ADD *** 12/28/99 **************************************
     C     *LIKE         DEFINE    FNSHNM        KKSHNM
     C*** END   ADD *** 12/28/99 **************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *LIKE         DEFINE    UPFND         #1FND
     C     *LIKE         DEFINE    UPEMP#        #1EMP#
     C     *LIKE         DEFINE    UPSUB#        #1SUB#
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C     *LIKE         DEFINE    @@CALL        SAVECL
     C     *LIKE         DEFINE    *IN09         SVIN09
     C     *LIKE         DEFINE    UPMRSU        ##MRSU
     C     *LIKE         DEFINE    UPMRGR        ##MRGR
     C*================================================================
     C* INITIALIZE
     C*================================================================
     C*
     C                   MOVE      'Y'           XX1ST             1
     C                   MOVEL(P)  Q1USER        SCUSER
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   Z-ADD     *ZEROS        XXRRN             9 0
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN17         IFEQ      *ON
     C                   SELECT
     C*
     C*  Check for user authorization to OPT 2 = change.
     C*
     C     SCSEL         WHENEQ    '2'
     C     SCAUTH        ANDEQ     'I'
     C     ##RCDS        ANDEQ     *ON
     C                   SETON                                        73  90
     C*
     C     SCSEL         WHENEQ    '20'
     C     SCAUTH        ANDEQ     'I'
     C                   SETON                                        74  90
     C*
     C                   ENDSL
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C*** BEGIN ADD *** 10/29/02 **************************************
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
     C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
     C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
     C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
     C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
     C*
     C                   SETOFF                                       17
     C     ##MGRI        IFEQ      'Y'
     C                   SETON                                        17
     C                   ENDIF
     C*
     C     XX1ST         IFEQ      'Y'
     C                   MOVE      'N'           XX1ST
     C                   MOVE      @@CALL        SAVECL
     C*
     C*** END   ADD *** 10/29/02 **************************************
     C*** BEGIN CHG *** 07/20/98 **************************************
     C*
     C* Initialize subscriber restriction indicator to *zero.
     C*
     C                   SETOFF                                       16
     C*
     C                   MOVEL(P)  Q1USER        KKUSR
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*** BEGIN ADD *** 10/29/02 **************************************
     C*
     C* Check for active user authorization
     C*
     C                   CALL      'SA008R'      RSTPL                  99
     C                   MOVE      UPMRSU        ##MRSU
     C*** BEGIN CHG *** 06/01/07 **************************************
     C**********           MOVE UPMRGR    ##MRGR
     C                   MOVE      *BLANKS       ##MRGR
     C     UPMRSU        IFEQ      'Y'
     C     UPMRGR        OREQ      'Y'
     C     UPMREM        OREQ      'Y'
     C                   MOVEL     'Y'           ##MRGR
     C                   ENDIF
     C*** END   CHG *** 06/01/07 **************************************
     C*
     C* Do NOT check active user authorization to Group/Subscriber when
     C* called as Search from S0009R02
     C*
     C     @@CALL        IFEQ      '*'
     C                   MOVE      '2'           @@CALL
     C                   MOVE      'N'           ##MRSU
     C                   MOVE      'N'           ##MRGR
     C                   MOVE      *OFF          ##RCDS
     C                   ENDIF                                                  @@CALL
     C*
     C                   ENDIF                                                  XX1ST
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C*** BEGIN ADD *** 10/29/02 **************************************
     C********** UPMRGR    IFEQ 'Y'
     C********** UPFND     ANDNE*ZEROS
     C********** *IN99     ANDEQ*OFF
     C     ##MRGR        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     UPFND         ANDNE     *ZEROS
     C     *IN99         ANDEQ     *OFF
     C     *IN17         OREQ      *OFF
     C*** BEGIN CHG *** 06/01/07 **************************************
     C********** ##MRGR    ANDEQ'Y'
     C     UPMRGR        ANDEQ     'Y'
     C*** END   CHG *** 06/01/07 **************************************
     C     UPFND         ANDNE     *ZEROS
     C     *IN99         ANDEQ     *OFF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   Z-ADD     UPSUB#        KKSUB#
     C                   Z-ADD     UPFND         KKFND
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   MOVE      *LOVAL        KKFND
     C*
     C                   ENDIF
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C                   MOVE      *LOVAL        KKNAME
     C*** BEGIN ADD *** 12/28/99 **************************************
     C                   MOVE      *LOVAL        KKSHNM
     C*** END   ADD *** 12/28/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*** BEGIN CHG *** 07/20/98 **************************************
     C   21
     CANN16              MOVE      SCPST1        KKFND
     C***21*****           MOVE SCPST1    KKFND
     C*** END   CHG *** 07/20/98 **************************************
     C   22              MOVEL     SCPST2        KKNAME
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23              MOVEL     SCPST3        KKSHNM
     C*** END   ADD *** 12/28/99 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVE      SCFND         KKFND
     C   22              MOVEL     SCNAME        KKNAME
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23              MOVEL(P)  SCSHNM        KKSHNM
     C*** END   ADD *** 12/28/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNAST         SCAST
     C                   MOVE      FNFND         XXFND
     C                   MOVEA     XXFND         NZ(1)
     C                   Z-ADD     1             Y                 1 0
     C                   MOVEL     '0'           SNUM              1
     C*
     C     Y             DOWLE     3
     C     SNUM          ANDEQ     '0'
     C*
     C     NZ(Y)         IFEQ      '0'
     C*
     C                   MOVE      ' '           NZ(Y)
     C                   ADD       1             Y
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           SNUM
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   MOVEA     NZ            XXFND             3
     C                   MOVEL     FNNAME        XXNAME           40
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23              MOVEL(P)  FNSHNM        XXNAME
     C                   MOVEL(P)  FNSHNM        SCSHNM
     C*** END   ADD *** 12/28/99 **************************************
     C*
     C   21              MOVEL(P)  XXFND         SCDATA
     C   21              MOVE      XXNAME        SCDATA
     C*
     C   22              MOVEL(P)  XXNAME        SCDATA
     C   22              MOVE      XXFND         SCDATA
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23              MOVEL(P)  XXNAME        SCDATA
     C   23              MOVE      XXFND         SCDATA
     C*** END   ADD *** 12/28/99 **************************************
     C*
     C                   MOVE      FNWCF#        SCWCF#
     C*
     C     F1KY          CHAIN     PF101                              99
     C*
     C                   MOVEL     F1OUTA        SCOUTA
     C*
     C                   MOVEL     FNSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - MAIN Information - Option 6
     C*================================================================
     C     OPT6          BEGSR
     C*
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'S'           ##MODE
     C                   ELSE
     C                   MOVE      'T'           ##MODE
     C                   ENDIF
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - A/R rates - Option 7
     C*================================================================
     C     OPT7          BEGSR
     C*
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'S'           ##MODE
     C                   ELSE
     C                   MOVE      'T'           ##MODE
     C                   ENDIF
     C*
     C                   CALL      ACPGM1        ACDPL2
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - Network Information - Option 8
     C*================================================================
     C     OPT8          BEGSR
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVEL'FB'      KKKEY
     C*
     C********** FBKY      CHAINSFB01                99
     C*
     C********** FBFL06    IFEQ 'Y'
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'S'           ##MODE
     C                   ELSE
     C                   MOVE      'T'           ##MODE
     C                   ENDIF
     C*
     C                   CALL      ACPGM2        ACDPL2
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           ELSE
     C*
     C**********           SETON                     71  90
     C*
     C**********           ENDIF
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT9 - State Information - Option 9
     C*================================================================
     C     OPT9          BEGSR
     C*
     C                   MOVEL     SCAST         KKST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     ST1A4         IFEQ      'Y'
     C*
     C     @@CALL        IFEQ      '1'
     C*** BEGIN CHG *** 04/02/99 **************************************
     C**********           MOVE 'D'       ##MODE
     C**********           ELSE
     C**********           MOVE 'C'       ##MODE
     C**********           ENDIF
     C                   MOVE      'D'           ##CALL
     C                   ELSE
     C                   MOVE      'A'           ##CALL
     C                   ENDIF
     C*** END   CHG *** 04/02/99 **************************************
     C*
     C                   CALL      ACPGM3        ACDPL1
     C*
     C                   ELSE
     C*
     C                   SETON                                        72  90
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEG ADD *** 06/11/98 ***************************************
     C*================================================================
     C* OPT10 - EMPLOYERS
     C*================================================================
     C*
     C     OPT10         BEGSR
     C*
     C                   CALL      'F0140R'
     C                   PARM      @@CALL        ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   ENDSR
     C*** END ADD *** 06/11/98 ***************************************
     C*================================================================
     C* OPT11 - Group T/M Rates - Option 11
     C*================================================================
     C     OPT11         BEGSR
     C*
     C                   CALL      'F0047R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT12 - WCMC Control Information - Option 12
     C*================================================================
     C     OPT12         BEGSR
     C*
     C* Inquiry mode
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'D'           ##MODE                         Inquiry
     C*
     C                   ELSE                                                   ELSE INQ
     C*
     C* Determine if need to ADD or CHANGE an existing FMCXP record
     C                   MOVE      SCFND         KKFND
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      'A'           ##MODE                         Add record
     C                   Z-ADD     *ZEROS        XXRRN
     C                   ELSE
     C                   MOVE      'C'           ##MODE                         Chg record
     C*** BEGIN CHG *** 12/28/99 **************************************
     C**********           Z-ADDDBRRN3    XXRRN
     C                   Z-ADD     DBRRN4        XXRRN
     C*** END   CHG *** 12/28/99 **************************************
     C                   ENDIF                                                  END *IN99
     C*
     C                   ENDIF                                                  END INQ
     C*
     C                   CALL      ACPGM4        ACDPL4
     C*
     C                   ENDSR
     C*
     C*** BEG ADD *** 12/01/98 ***************************************
     C*================================================================
     C* OPT14 - Option 14 (DARTS)
     C*================================================================
     C     OPT14         BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C*
     C                   CALL      'FA718R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCIN        #@WCIN            1
     C                   PARM      ##WCDK        #@WDCK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ***************************************
     C*** Begin Add *** 10/02/98 *************************************
     C*================================================================
     C* OPT16 - WORK WITH Group EOMB Comments
     C*================================================================
     C     OPT16         BEGSR
     C*
     C                   CALL      'F0063R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   ENDSR
     C*
     C*** End Add *** 10/02/98 ****************************************
     C*** BEGIN ADD *** 08/13/06 **************************************
     C*================================================================
     C* OPT18 - Work with Group Service Code Billings
     C*================================================================
     C     OPT18         BEGSR
     C*
     C                   CALL      'F0049R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 08/13/06 **************************************
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'I'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 12/31/02 **************************************
     C*================================================================
     C* OPT90 - OUTCOME/SAVINGS CODES
     C*================================================================
     C     OPT90         BEGSR
     C*
     C                   CALL      'F2451R'
     C     *INKC         PARM      *BLANKS       ##KC              1
     C     *INKL         PARM      *BLANKS       ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   ENDSR
     C*** END   ADD *** 12/31/02 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVE      @@CALL        SVCALL
     C                   MOVE      *IN09         SVIN09
      *
     C     SCAUTH        IFEQ      'I'
     C                   MOVE      *ON           *IN09
     C                   MOVE      '1'           @@CALL
     C                   ENDIF                                                  SCAUTH=I
     C                   ENDIF                                                  SCAUTH=I
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '12'          OPT12
     C*** BEGIN ADD *** 08/13/06 **************************************
     C     SCSEL         CASEQ     '18'          OPT18
     C*** END   ADD *** 08/13/06 **************************************
     C     SCSEL         CASEQ     '20'          OPT20
     C*
     C*** BEG ADD *** 10/02/98 ****************************************
     C*
     C     SCSEL         CASEQ     '16'          OPT16
     C*
     C*** BEG ADD *** 10/02/98 ****************************************
     C*** BEG ADD *** 12/01/98 ****************************************
     C     SCSEL         CASEQ     '14'          OPT14
     C*** END ADD *** 12/01/98 ****************************************
     C*** BEGIN ADD *** 12/31/02 **************************************
     C     SCSEL         CASEQ     '90'          OPT90
     C*** END   ADD *** 12/31/02 **************************************
     C*
     C                   ENDCS
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN17         IFEQ      *ON
     C                   MOVE      SVCALL        @@CALL
     C                   MOVE      SVIN09        *IN09
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
      **** Begin Add **** 12/18/24 ******************************************
      * Do not Pass-Through to new screen, if 5th parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 5
     c                   Call      'MTGRPCFG'
     c                   parm                    @@rrn1            9            rrn
     c                   parm                    @@mode1           1            Mode
     c                   parm                    @@next1          10            next pgm
     c                   parm                    @@TabInd1         5            tab index
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     c                   Endif
      **** End Add ****** 12/18/24 ******************************************
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     8             SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
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
     C*** BEGIN ADD *** 12/31/02 **************************************
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C*
     C     ##MED         IFNE      'Y'
     C     ##OS          ORNE      'Y'
     C                   MOVEL     CLRFKY        OFD(14)
     C                   ENDIF
     C*** END   ADD *** 12/31/02 **************************************
     C*
     C     ##NTWK        IFNE      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(6)
     C*
     C                   ENDIF
     C*
     C     @@RRN         IFEQ      *ZEROS
     C     *INKM         OREQ      *ON
     C*
     C                   MOVE      ##SUB#        SCSUB#
     C                   MOVE      ##SUB#        KKSUB#
     C*
     C     KEY1          CHAIN     PSM02                              99
     C*
     C                   MOVE      SMNAME        SCSNAM
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     PSM01                              99
     C*
     C*** BEGIN CHG *** 02/27/98 *********************************
     C**********           MOVE *ZEROS    @@RRN
     C   30              MOVE      *ZEROS        @@RRN
     C*** END   CHG *** 02/27/98 *********************************
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      SMSUB#        KKSUB#
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 10/29/02 **************************************
     C*
     C*  F6=ADD
     C*
     C                   MOVEL(P)  '2'           XXOPFN
     C                   MOVEL(P)  '06'          XXNMBR
     C*
     C     ##MRGR        IFEQ      'Y'
     C                   MOVEL(P)  'NNN'         XXMIS
     C                   ELSE
     C                   MOVEL(P)  'YNN'         XXMIS
     C                   ENDIF                                                  ##MRGR=Y
     C*
     C                   EXSR      UPDOFA
     C*** END   ADD *** 10/29/02 **************************************
     C**** BEGIN ADD *** 02/27/98 *********************************
     C*
     C*** GET SYSTEM INFO
     C*
     C*** BEG DEL *** 08/06/98 *************************************
     C***********          CALL 'SA001R'
     C***********          PARM *BLANK    ##WCIF  1        WC INST
     C***********          PARM *BLANK    ##MCIF  1        MC INST
     C***********          PARM *BLANK    ##WCIN  1        WC INTERFACE
     C***********          PARM *BLANK    ##AR    1        MC AR INST
     C***********          PARM *BLANK    ##NTWK  1        NETWORK INST
     C***********          PARM *BLANK    ##UR    1        PRECERT INST
     C***********          PARM *BLANK    ##MED   1        MEDMGMT INST
     C***********          PARM *BLANK    ##EDI   1        EDI INST
     C***********          PARM *BLANK    ##WCDK  1        WC DARTS KEY
     C*** END DEL *** 08/06/98 *************************************
     C*
     C     ##MED         IFNE      'Y'
     C*
     C*******************  MOVELCLRFKY    OFD,7           DEL 10/09/98
     C*** BEG ADD *** 08/06/98 **************************************
     C                   MOVEL     CLRFKY        OFD(9)
     C*** END ADD *** 08/06/98 **************************************
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 09/25/00 **************************************
     C*
     C*** IS A/R INSTALLED ?
     C*
     C********** ##AR      IFNE 'Y'
     C*
     C**********           MOVELCLRFKY    OFD,5
     C*
     C**********           ENDIF                                       '1'
     C*** END   DEL *** 09/25/00 **************************************
     C*
     C*** IS NETWORK INSTALLED ?
     C*
     C     ##NTWK        IFNE      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(6)
     C*
     C                   ENDIF                                                              '1'
     C**** END   ADD *** 02/27/98 *********************************
     C*** BEGIN ADD *** 06/30/99 **************************************
     C     @@RRN         IFNE      *ZEROS
     C*
     C                   MOVEL     CANFKY        OFD(18)
     C                   MOVEL     CLRFKY        OFD(19)
     C*
     C                   ENDIF
     C*** END   ADD *** 06/30/99 **************************************
     C*** BEGIN ADD *** 08/13/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C     XXCLT#        IFNE      '139'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '18'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END   ADD *** 08/13/06 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*** BEGIN CHG *** 07/20/98 **************************************
     C***21***** KEY1      READEDBRCDF                   95
     C   21
     CANN16KEY1          READE     DBRCDF                                 95
     C   21
     CAN 16KEY2          READE     DBRCDF                                 95
     C*** END   CHG *** 07/20/98 **************************************
     C   22KEY1          READE     DBRCDF2                                95
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23KEY1          READE     DBRCDF3                                95
     C*** END   ADD *** 12/28/99 **************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*** BEGIN CHG *** 07/20/98 **************************************
     C***21***** KEY1      REDPEDBRCDF                   95
     C   21
     CANN16KEY1          READPE    DBRCDF                                 95
     C   21
     CAN 16KEY2          READPE    DBRCDF                                 95
     C*** END   CHG *** 07/20/98 **************************************
     C   22KEY1          READPE    DBRCDF2                                95
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23KEY1          READPE    DBRCDF3                                95
     C*** END   ADD *** 12/28/99 **************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23KEY4          SETGT     DBRCDF3                            96
     C*** END   ADD *** 12/28/99 **************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C*** BEGIN ADD *** 12/28/99 **************************************
     C   23KEY4          SETLL     DBRCDF3                            96
     C*** END   ADD *** 12/28/99 **************************************
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     FNSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     *IN22         IFEQ      *ON
     C     *IN16         ANDEQ     *ON
     C     UPFND         ANDNE     FNFND
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 07/20/98 **************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVE      'A'           SCAUTH
     C*
     C* Check Group Restriction
     C*
     C     ##MRGR        IFEQ      'Y'
     C     ##RCDS        IFEQ      *ON
     C                   CALL      'SA008R'                             99
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM      *BLANK        ##AUTH            1
     C                   PARM      FNFND         #1FND
     C                   PARM      *ZERO         #1EMP#
     C                   PARM      FNSUB#        #1SUB#
     C*
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C                   ENDIF                                                  ##RCDS
     C                   ENDIF                                                  ##MRGR
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   ENDSR
** Column headings
Grp# Group Name                                Standalone Group #    Sts
Group Name                                Grp# Standalone Group #    Sts
Group Short Name                          Grp# Standalone Group #    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
1066=Main information       YYN
1077=A/R info               YYN
1088=Networks               YYN
1099=State info             YYN
11010=Employers             YYN
11111=T/M Rates             YYN
11212=Control info          YNN
11414=DARTS                 YYN
11616=EOMB Comments         YYN
11818=Grp Svc Code Billings YYN
12020=Change Status         YNN
19090=Outcome/Savings Codes YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
213F13=Chg User Parms       YYY
217F17=Subset               YYY
3
