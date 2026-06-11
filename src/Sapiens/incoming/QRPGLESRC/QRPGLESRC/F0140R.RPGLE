     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  F0140R - WORK WITH EMPLOYERS     C:62 02/16/98               *
     H*                                                               *
     H*  CALLED FROM: MANY PLACES                                     *
     H*  CALLS TO   : F0140R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/01/07 C:62  PROJECT: MC099 58565                          *
     H*                 CORRECT MULTI-GROUP FLAG USAGE.               *
     H*
     H*  10/29/02 C:62 PROJECT MC253                                  *
     H*                ADD MULTI GROUP RESTRICTION LOGIC.             *
     H*
     H*  05/16/00 C:33 SEQ#: OP082/21740                              *
     H*                ALLOW FOR DIVISION RESTRICTION.                *
     H*                                                               *
     H*  04/12/00 C:53 PROJECT: MC189                                 *
     H*                ADD OPTION 4 TO PROCESS A DELETE REQUEST       *
     H*                                                               *
     H*  06/30/99 C:62 PROJECT: MC206                                 *
     H*                - MAKE SURE F12 DOES NOT APPEAR IN MENU        *
     H*                - MAKE SURE F13 DOES NOT APPEAR IN GROUP CALL  *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED OPTION 14=DARTS TO PROGRAM               *
     H*                                                               *
     H*  08/20/98 C:62 PROJECT: MC098 13370                           *
     H*                MAKE SURE THAT ONLY ACTIVE EMPLOYERS ARE       *
     H*                DISPLAYED DURING A SEARCH.                     *
     H*                                                               *
     H*  08/06/98 C:94 PROJECT: MCMNT                                 *
     H*                MADE F12=CANCEL AVAILABLE                      *
     H*                                                               *
     H*  07/20/98 C:62 PROJECT: MC177                                 *
     H*                CHECK SMUPL TO DETERMINE IF USER IS RESTRICTED *
     H*                AND IF SO ONLY ALLOW THE USER TO SEE THE SUB-  *
     H*                SCRIBER, GROUP, EMPLOYER# AND/OR DIVISION THE  *
     H*                USER IS RESTRICTED TO.                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*    *IN16 = IS USED TO INDICATE IF EMPLOYER/DIV IS RESTRICTED. *
     H*                                                               *
     H*****************************************************************
     FF0140F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMEML4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEM01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMEML3    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEM01:DBRCDF2)
     F*
     F*  Tertiary logical file definition (for displaying items)
     FFMEML     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PEM01:DBRCDF3)
     FFMSML     IF   E           K DISK
     FFMFNP     IF   E             DISK
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PFN01:PFN02)
     F*** BEGIN ADD *** 07/20/98 **************************************
     FSMUPL     IF   E           K DISK
     F*** END   ADD *** 07/20/98 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(3) CTDATA PERRCD(1)              Column headings
     D*** BEG CHG *** 12/01/98 ***************************************
     D*** BEG CHG *** 04/12/00 ***************************************
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D****************    OF      1  12  1   OFD    35   Options, Function
     D*** END CHG *** 04/12/00 ***************************************
     D**********          OF      1  11  1   OFD    35   Options, Function
     D*** BEG CHG *** 12/01/98 ***************************************
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
     D OPTNM           C                   CONST('Employers')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0140R01')
     D CHGNUM          C                   CONST('SA020R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D STSFKY          C                   CONST('2020=Change status  -
     D                                            YNN')
     D CHGFKY          C                   CONST('022=Change          -
     D                                            YNN')
     D ADDFKY          C                   CONST('06F6=Add            -
     D                                            YNN')
     D*** BEGIN ADD *** 06/30/99 **************************************
     D CANFKY          C                   CONST('12F12=Cancel        -
     D                                            YYN')
     D*** END   ADD *** 06/30/99 **************************************
     D*** BEG ADD *** 04/12/00 **************************************
     D DELETE          C                   CONST('*** DELETED ***')

     I*** END ADD *** 04/12/00 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FMEMP
     C                   PARM                    @@RRN2            9 0          FMFNP
     C                   PARM                    @@VIEW            1 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          FMEMP
     C                   PARM                    ##RRN2            9 0          FMFNP
     C*
     C*  Change numeric field to character field.
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
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
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     RSTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C*
     C     RSTPLE        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'M'           ##APPL
     C                   PARM      *OFF          ##RTCD
     C     SCAUTH        PARM      *BLANK        ##AUTH            1
     C                   PARM      EMFND         #2FND
     C                   PARM      EMEMP#        #2EMP#
     C                   PARM      EMSUB#        #2SUB#
     C*** END   ADD *** 10/29/02 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKNAME
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKDBA
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     KEY4A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
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
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    EMDBA         KKDBA
     C     *LIKE         DEFINE    EMNAME        KKNAME
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C*** END   ADD *** 07/20/98 **************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *LIKE         DEFINE    UPFND         #2FND
     C     *LIKE         DEFINE    UPEMP#        #2EMP#
     C     *LIKE         DEFINE    UPSUB#        #2SUB#
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C     *LIKE         DEFINE    @@CALL        SAVECL
     C     *LIKE         DEFINE    *IN09         SVIN09
     C     *LIKE         DEFINE    UPMRSU        ##MRSU
     C     *LIKE         DEFINE    UPMRGR        ##MRGR
     C     *LIKE         DEFINE    UPMREM        ##MREM
     C*================================================================
     C* INITIALIZE
     C*================================================================
     C                   MOVE      'Y'           XX1ST             1
     C                   MOVEL(P)  Q1USER        SCUSER
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*
     C     @@RRN2        IFEQ      *ZEROS
     C     ##SUB#        ANDEQ     *ZEROS
     C     @@RRN2        OREQ      *ZEROS
     C     ##FND         ANDEQ     *ZEROS
     C*
     C                   CALL      'SRPGMMSG'
     C                   PARM      'MC40049'     ##MSGI
     C                   PARM      'FSMESG'      ##MSGF
     C                   PARM      *BLANKS       ##MSGL
     C*
     C                   ENDIF
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
     C     SCAUTH        WHENEQ    'I'
     C*
     C     SCSEL         IFEQ      '4'
     C     SCSEL         OREQ      '20'
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCSEL=4
     C*
     C                   ENDSL
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C*** BEG ADD *** 04/12/00 **************************************
     C*
     C*** VERIFY THAT OPTION 4 IS ALLOWED IF TAKEN.
     C*
     C     SCSEL         IFEQ      '4'
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN90         ANDEQ     *OFF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C                   MOVE      SCEMP#        #@EMP#            9
     C                   MOVE      SCDIV         #2DIV
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C                   SETON                                        9071
     C                   ENDIF
     C                   ENDIF
     C*
     C*** CHECK FOR ANY VALID CLAIMS FOR THIS EMPLOYER
     C*
     C                   CALL      'FA207R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #@CO#
     C                   PARM                    #@SUB#
     C                   PARM                    #@FND
     C                   PARM                    #@EMP#            9
     C                   PARM                    #2DIV             5
     C*
     C     ##RTCD        IFEQ      *ON
     C                   SETON                                        9072
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 04/12/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN2        IFNE      *ZEROS
     C*
     C     @@RRN2        CHAIN     PFN01                              99
     C*
     C                   MOVE      FNCO#         KKCO#
     C                   MOVE      FNSUB#        KKSUB#
     C                   MOVE      FNFND         KKFND
     C*
     C                   ELSE
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      ##SUB#        KKSUB#
     C                   MOVE      ##FND         KKFND
     C*
     C                   ENDIF
     C*
     C                   MOVE      *LOVAL        KKDBA
     C                   MOVE      *LOVAL        KKNAME
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
     C                   MOVE      UPMRGR        ##MRGR
     C*** BEGIN CHG *** 06/01/07 **************************************
     C**********           MOVE UPMREM    ##MREM
     C                   MOVE      *BLANKS       ##MREM
     C     UPMRSU        IFEQ      'Y'
     C     UPMRGR        OREQ      'Y'
     C     UPMREM        OREQ      'Y'
     C                   MOVEL     'Y'           ##MREM
     C                   ENDIF
     C*** END   CHG *** 06/01/07 **************************************
     C*
     C* Do NOT check active user authorization to Group/Subscriber/Employer
     C* when called as Search from S0009R02
     C*
     C     @@CALL        IFEQ      '*'
     C                   MOVE      '2'           @@CALL
     C                   MOVE      'N'           ##MRSU
     C                   MOVE      'N'           ##MRGR
     C                   MOVE      'N'           ##MREM
     C                   MOVE      *OFF          ##RCDS
     C                   ENDIF                                                  @@CALL
     C*
     C                   ENDIF                                                  XX1ST
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C*** BEGIN CHG *** 10/29/02 **************************************
     C********** UPMREM    IFEQ 'Y'
     C********** UPEMP#    ANDNE*ZEROS
     C********** *IN99     ANDEQ*OFF
     C     ##MREM        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     UPEMP#        ANDNE     *ZEROS
     C     *IN99         ANDEQ     *OFF
     C     *IN17         OREQ      *OFF
     C*** BEGIN CHG *** 06/01/07 **************************************
     C********** ##MREM    ANDEQ'Y'
     C     UPMREM        ANDEQ     'Y'
     C*** END   CHG *** 06/01/07 **************************************
     C     UPEMP#        ANDNE     *ZEROS
     C     *IN99         ANDEQ     *OFF
     C*** END   CHG *** 10/29/02 **************************************
     C*
     C                   Z-ADD     UPEMP#        KKEMP#
     C                   Z-ADD     UPDIV         KKDIV
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     UPFL5         IFNE      'Y'
     C                   MOVE      *LOVAL        KKDIV
     C                   ENDIF                                                  UPFL5<>Y
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C*
     C                   ENDIF
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL     SCPST1        KKNAME
     C   22              MOVEL     SCPST2        KKDBA
     C*** BEGIN CHG *** 07/20/98 **************************************
     C***23*****           MOVE SCPST3    KKEMP#
     C***23*****           MOVE SCPSTD    KKDIV
     C   23
     CANN16              MOVE      SCPST3        KKEMP#
     C   23
     CANN16              MOVE      SCPSTD        KKDIV
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL     SCNAME        KKNAME
     C   22              MOVE      SCDBA         KKDBA
     C   23              MOVEL     SCEMP#        KKEMP#
     C   23              MOVEL     SCDIV         KKDIV
     C*
     C                   ENDSR
     C*================================================================
     C*  KMMO   - Process F13; user parameters
     C*================================================================
     C     KMMO          BEGSR
     C                   CALL      USPAPI                               99
     C                   EXSR      GETCOF
     C                   EXSR      KEYINZ
     C                   EXSR      CLRGET
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EMSTAT        SCSTAT
     C                   MOVE      EMDBA         SCDBA
     C                   MOVEL     EMNAME        SCNAME
     C                   MOVEL     EMEMP#        SCEMP#
     C                   MOVEL     EMDIV         SCDIV
     C*
     C                   MOVEL(P)  EMEMP#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXEMP#            9
     C*
     C     EMDIV         IFNE      *ZEROS
     C*
     C                   MOVEL(P)  EMDIV         ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXDIV             5
     C*
     C                   ELSE
     C*
     C                   MOVE(P)   '0'           XXDIV
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXEMP#        XXEMDV           15
     C                   MOVE      XXDIV         XXEMDV
     C*
     C   21              MOVEL(P)  EMNAME        SCDATA
     C   21              MOVE      XXEMDV        SCDATA
     C*
     C   22              MOVEL(P)  EMDBA         SCDATA
     C   22              MOVE      XXEMDV        SCDATA
     C*
     C   23              MOVEL(P)  XXEMDV        SCDATA
     C   23              MOVE      EMDBA         SCDATA
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/01/98 ***************************************
     C*================================================================
     C* OPT14 - Option 14 (DARTS)
     C*================================================================
     C     OPT14         BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C                   MOVE      SCEMP#        #@EMP#
     C                   MOVE      SCDIV         #@DIV
     C*
     C                   CALL      'FA719R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCIN        #@WCIN            1
     C                   PARM      ##WCDK        #@WDCK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM                    #@EMP#            9
     C                   PARM                    #@DIV             5
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ***************************************
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   Z-ADD     SCDBRR        ##RRN
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
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
     C*
     C     SCAUTH        IFEQ      'I'
     C                   MOVE      *ON           *IN09
     C                   MOVE      '1'           @@CALL
     C                   ENDIF                                                  SCAUTH=I
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C     SCSEL         CASEQ     '20'          OPT20                          CHANGE STATUS
     C*** BEG ADD *** 12/01/98 ****************************************
     C     SCSEL         CASEQ     '14'          OPT14
     C*** END ADD *** 12/01/98 ****************************************
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
     C                   Z-ADD     9             SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C   KM              EXSR      KEDO
     C*
     C* SETS THE INITIAL SCREEN VIEW WHEN CALLED
     C*
     C     Q1PRMS        IFGT      5
     C*
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C*
     C                   ENDIF                                                  Q1PRMS>5
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
     C* FILL SUBSCRIBER AND GROUP HEADER VALUES.
     C*
     C     @@RRN2        IFEQ      *ZEROS
     C*
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCGPNM
     C*
     C                   ELSE
     C*
     C*** BEGIN ADD *** 06/30/99 **************************************
     C  N30              MOVEL     CANFKY        OFD(10)
     C  N30              MOVEL     CLRFKY        OFD(11)
     C*** END   ADD *** 06/30/99 **************************************
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCGPNM
     C*
     C                   ENDIF
     C*
     C                   MOVE      ##SUB#        SCSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSBNM
     C*
     C* GET RRN FOR FMFNP TO PASS TO F0145R01
     C*
     C     @@RRN2        IFEQ      *ZEROS
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*** BEGIN ADD *** 06/30/99 **************************************
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 06/30/99 **************************************
     C*
     C                   Z-ADD     DBRRN4        ##RRN2
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   ELSE
     C*
     C                   SETON                                        99
     C*
     C     *IN99         DOWEQ     *ON
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKM         CASEQ     *ON           KMMO
     C                   ENDCS
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     ##SUB#        KKSUB#
     C                   Z-ADD     ##FND         KKFND
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C  N99              Z-ADD     SMSUB#        SCSUB#
     C  N99              MOVEL     SMNAME        SCSBNM
     C*
     C  N99FNKY          CHAIN     PFN02                              99
     C*
     C  N99              MOVE      FNFND         SCFND
     C  N99              MOVEL     FNNAME        SCGPNM
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*** END   ADD *** 06/30/99 **************************************
     C*
     C                   ELSE
     C*
     C                   Z-ADD     @@RRN2        ##RRN2
     C*
     C                   ENDIF
     C*
     C*** API TO DETERMINE IF GROUP IS STANDALONE.
     C*
     C*
     C     @@RRN2        CHAIN     PFN01                              99
     C*
     C                   MOVE      FNCO#         KKCO#
     C                   MOVE      FNSUB#        KKSUB#
     C                   MOVE      FNFND         KKFND
     C                   MOVE      FNCO#         #1CO#
     C                   MOVE      FNSUB#        #1SUB#
     C                   MOVE      FNFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANK        ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C*
     C                   MOVEL     CLRFKY        OFD(2)
     C*** BEG CHG *** 12/01/98 **************************************
     C*** BEG CHG *** 04/12/00 **************************************
     C                   MOVEL     CLRFKY        OFD(6)
     C                   MOVEL     CLRFKY        OFD(9)
     C**********           MOVELCLRFKY    OFD,5
     C**********           MOVELCLRFKY    OFD,8
     C*** END CHG *** 04/12/00 **************************************
     C**********           MOVELCLRFKY    OFD,4
     C**********           MOVELCLRFKY    OFD,7
     C*** BEG CHG *** 12/01/98 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     CHGFKY        OFD(2)
     C*** BEG CHG *** 12/01/98 **************************************
     C*** BEG CHG *** 04/12/00 **************************************
     C                   MOVEL     STSFKY        OFD(6)
     C                   MOVEL     ADDFKY        OFD(9)
     C**********           MOVELSTSFKY    OFD,5
     C**********           MOVELADDFKY    OFD,8
     C*** END CHG *** 04/12/00 **************************************
     C**********           MOVELSTSFKY    OFD,4
     C**********           MOVELADDFKY    OFD,7
     C*** BEG CHG *** 12/01/98 **************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 10/29/02 **************************************
     C*
     C*  F6=ADD
     C*
     C                   MOVEL(P)  '2'           XXOPFN
     C                   MOVEL(P)  '06'          XXNMBR
     C*
     C     ##MREM        IFEQ      'Y'
     C                   MOVEL(P)  'NNN'         XXMIS
     C                   ELSE
     C                   MOVEL(P)  'YNN'         XXMIS
     C                   ENDIF                                                  ##MREM=Y
     C*
     C                   EXSR      UPDOFA
     C*** END   ADD *** 10/29/02 **************************************
     C   KM              EXSR      BLDLNS
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*** BEGIN CHG *** 07/20/98 **************************************
     C***23***** KEY1      READEDBRCDF3                  95
     C   23
     CANN16KEY1          READE     DBRCDF3                                95
     C*
     C***  BEGIN CHG  ***  05/16/00  ********************************
     C***23*16** KEY4      READEDBRCDF3                  95
     C   23
     CAN 16UPFL5         IFEQ      'Y'
     C     KEY4          READE     DBRCDF3                                95
     C                   ELSE
     C     KEY4A         READE     DBRCDF3                                95
     C                   ENDIF                                                  UPFL5=Y
     C***  END   CHG  ***  05/16/00  ********************************
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*** BEGIN CHG *** 07/20/98 **************************************
     C***23***** KEY1      REDPEDBRCDF3                  95
     C   23
     CANN16KEY1          READPE    DBRCDF3                                95
     C*
     C***  BEGIN CHG  ***  05/16/00  ********************************
     C***23*16** KEY4      REDPEDBRCDF3                  95
     C   23
     CAN 16UPFL5         IFEQ      'Y'
     C     KEY4          READPE    DBRCDF3                                95
     C                   ELSE
     C     KEY4A         READPE    DBRCDF3                                95
     C                   ENDIF                                                  UPFL5=Y
     C***  END   CHG  ***  05/16/00  ********************************
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C   23KEY4          SETGT     DBRCDF3                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C   23KEY4          SETLL     DBRCDF3                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*** BEG ADD *** 04/12/00 **************************************
     C                   MOVEL(P)  DELETE        SCDATA
     C*** END ADD *** 04/12/00 **************************************
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
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     *IN16         IFEQ      *ON
     C*
     C     *IN21         IFEQ      *ON
     C     *IN22         OREQ      *ON
     C*
     C     UPEMP#        IFNE      EMEMP#
     C***  BEGIN DEL  ***  05/16/00  *********************************
     C********** UPDIV     ORNE EMDIV
     C***  END   DEL  ***  05/16/00  *********************************
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C                   ELSE
     C*
     C     UPFL5         IFEQ      'Y'
     C     UPDIV         ANDNE     EMDIV
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  UPFL5=Y
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 07/20/98 **************************************
     C*** BEGIN ADD *** 08/20/98 **************************************
     C     @@CALL        IFEQ      '2'
     C     EMSTAT        ANDEQ     'N'
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 08/20/98 **************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVE      'A'           SCAUTH
     C*
     C*  Check for Employer Restriction
     C*
     C     ##MREM        IFEQ      'Y'
     C     ##RCDS        IFEQ      *ON
     C*
     C                   CALL      'SA008R'      RSTPLE                 99
      *
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C                   ENDIF                                                  ##RCDS
     C                   ENDIF                                                  MREM
     C                   ENDIF                                                  MREM
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   ENDSR
** Column headings
Employer Name                            Employer#   Div  Sts
Employer DBA                             Employer#   Div  Sts
Employer#   Div Employer DBA                              Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
11414=DARTS                 YYN
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
213F13=Chg Usr Parm         YYN
3
