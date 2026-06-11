     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0055R - WORK WITH SUBSCRIBERS   C:62 12/02/97               *
     H*                                                               *
     H*  CALLED FROM: MANY PLACES                                     *
     H*  CALLS TO:                                                    *
     H*               F0055R01                                        *
     H*               F0055R02                                        *
     H*               DARTS                                           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/01/07 C:62  PROJECT: MC099 58565                          *
     H*                 CORRECT MULTI-GROUP FLAG USAGE.               *
     H*                                                               *
     H*  10/28/02 C:62  PROJECT: MC253                                *
     H*                 ADD MULTI GROUP RESTRICTIONS LOGIC.           *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  06/30/99 C:62 PROJECT: MC206                                 *
     H*                ADD PARM FOR CONTINUE WORKFLOW FOR F0055R02.   *
     H*                                                               *
     H*  10/06/98 C:53                                                *
     H*                REMOVED CONDITION ON OPT 10 BEING DISPLAYED    *
     H*                                                               *
     H*  08/06/98 C:94                                                *
     H*                MADE OPTION 11-SUBSCRIBER T/M CONDITIONAL ON   *
     H*                SFBPF/FBAL01, POS. 9 - ##MED                   *
     H*                                                               *
     H*  07/20/98 C:62 PROJECT: MC177                                 *
     H*                CHECK SMUPL TO DETERMINE IF USER IS RESTRICTED *
     H*                AND IF SO ONLY ALLOW THE USER TO SEE THE SUB-  *
     H*                SCRIBER THEY ARE RESTRICTED TO.                *
     H*                                                               *
     H*  06/11/98 C:94 PROJECT: MC145                                 *
     H*                ADDED OPTION 10 TO CALL GROUPS.                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0055F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     FSMUPL     UF   E           K DISK
     F*  Physical file definition (for refreshing subfile record)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMSML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSM01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMSML1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PSM01:DBRCDF2)
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCSUB#                 4      8  0
     D*** BEGIN DEL *** 08/15/00 **************************************
     D**********  DS
     D**********                              1  10 FBAL04
     D**********                              5   5 XXARIN
     D*** END   DEL *** 08/15/00 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Subscribers')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0055R01')
     D ACPGM2          C                   CONST('F0055R02')
     D CHGNUM          C                   CONST('SA020R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** BEGIN ADD *** 06/30/99 **************************************
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      'N'           ##CONT            1
     C*** END   ADD *** 06/30/99 **************************************
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
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD *** 10/28/02 **************************************
     C     RSTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
      *
     C     RSTPL1        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM      *BLANK        ##AUTH            1
     C                   PARM      *ZERO         #@FND
     C                   PARM      *ZERO         ##EMP#
     C                   PARM      SMSUB#        ##SUB#
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNAME
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           KKKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     UPKY          KLIST
     C                   KFLD                    KKUSR
     C*** END   ADD *** 07/20/98 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SMSUB#        KKSUB#
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C     *LIKE         DEFINE    SMNAME        KKNAME
     C*** BEGIN ADD *** 10/28/02 **************************************
     C     *LIKE         DEFINE    UPFND         #@FND
     C     *LIKE         DEFINE    UPEMP#        ##EMP#
     C     *LIKE         DEFINE    UPSUB#        ##SUB#
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C     *LIKE         DEFINE    *IN09         SVIN09
     C     *LIKE         DEFINE    UPMRSU        ##MRSU
     C*================================================================
     C* INITIALIZE
     C*================================================================
     C*
     C                   MOVE      'Y'           XX1ST             1
     C*** END   ADD *** 10/28/02 **************************************
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
     C*** BEGIN ADD *** 10/28/02 **************************************
     C     *IN17         IFEQ      *ON
     C                   SELECT
     C*
     C*  Check for user authorization to OPT 2 = change.
     C*
     C     SCSEL         WHENEQ    '2'
     C     SCAUTH        ANDEQ     'I'
     C     ##RCDS        ANDEQ     *ON
     C                   SETON                                        71  90
     C*
     C     SCSEL         WHENEQ    '20'
     C     SCAUTH        ANDEQ     'I'
     C                   SETON                                        72  90
     C*
     C                   ENDSL
     C                   ENDIF
     C*** END   ADD *** 10/28/02 **************************************
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
     C*** BEGIN ADD *** 10/28/02 **************************************
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
     C*
     C*** END   ADD *** 10/28/02 **************************************
     C*** BEGIN CHG *** 07/20/98 **************************************
     C*
     C* Initialize subscriber restriction indicator to *zero.
     C*
     C                   SETOFF                                       16
     C*
     C                   MOVEL     Q1USER        KKUSR
     C*
     C     UPKY          CHAIN(N)  PUP01                              99
     C*** BEGIN ADD *** 10/28/02 **************************************
     C*
     C* Check for active user authorization
     C*
     C                   CALL      'SA008R'      RSTPL                  99
     C*** BEGIN CHG *** 06/01/07 **************************************
     C**********           MOVE UPMRSU    ##MRSU
     C                   MOVE      *BLANKS       ##MRSU
     C     UPMRSU        IFEQ      'Y'
     C     UPMRGR        OREQ      'Y'
     C     UPMREM        OREQ      'Y'
     C                   MOVEL     'Y'           ##MRSU
     C                   ENDIF
     C*** END   CHG *** 06/01/07 **************************************
     C*
     C* Do NOT check active user authorization to Group/Subscriber when
     C* called as Search from S0009R02
     C*
     C     @@CALL        IFEQ      '*'
     C                   MOVE      '2'           @@CALL
     C                   MOVE      'N'           ##MRSU
     C                   MOVE      *OFF          ##RCDS
     C                   ENDIF                                                  @@CALL
     C*
     C                   ENDIF                                                  XX1ST
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C*** BEGIN CHG *** 10/28/02 **************************************
     C********** UPMRSU    IFEQ 'Y'
     C     ##MRSU        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C*** END   CHG *** 10/28/02 **************************************
     C     UPSUB#        ANDNE     *ZEROS
     C     *IN99         ANDEQ     *OFF
     C*** BEGIN ADD *** 10/28/02 **************************************
     C     *IN17         OREQ      *OFF
     C*** BEGIN CHG *** 06/01/07 **************************************
     C********** ##MRSU    ANDEQ'Y'
     C     UPMRSU        ANDEQ     'Y'
     C*** END   CHG *** 06/01/07 **************************************
     C     UPSUB#        ANDNE     *ZEROS
     C     *IN99         ANDEQ     *OFF
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C                   Z-ADD     UPSUB#        KKSUB#
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   MOVE      *LOVAL        KKSUB#
     C*
     C                   ENDIF
     C*
     C*** END   CHG *** 07/20/98 **************************************
     C                   MOVE      *LOVAL        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*** BEGIN CHG *** 07/20/98 **************************************
     C   21
     CANN16              MOVE      SCPST1        KKSUB#
     C***21*****           MOVE SCPST1    KKSUB#
     C*** END   CHG *** 07/20/98 **************************************
     C   22              MOVEL     SCPST2        KKNAME
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVE      SCSUB#        KKSUB#
     C   22              MOVEL     SCNAME        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     SMSTAT        SCSTAT
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCNAME
     C*
     C                   MOVEL(P)  SMSUB#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXSUB#            5
     C*
     C   21              MOVEL(P)  XXSUB#        SCDATA
     C   21              MOVE      SMNAME        SCDATA
     C*
     C   22              MOVEL(P)  SMNAME        SCDATA
     C   22              MOVE      XXSUB#        SCDATA
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - A/R Information - Option 7
     C*================================================================
     C     OPT7          BEGSR
     C*
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'D'           ##MODE
     C                   ELSE
     C                   MOVE      'C'           ##MODE
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 06/30/99 **************************************
     C**********           CALL ACPGM2    ACDPL
     C                   CALL      ACPGM2        ACDPL2
     C*** END   CHG *** 06/30/99 **************************************
     C*
     C                   ENDSR
     C*** BEG ADD *** 06/11/98 ***************************************
     C*================================================================
     C* OPT10 - GROUPS
     C*================================================================
     C*
     C     OPT10         BEGSR
     C*
     C                   MOVEL     Q1USER        KKUSR
     C*
     C     KKUSR         CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      ##CO#         UPCO#
     C                   MOVE      SCSUB#        UPSUB#
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     DSTIME        UPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C*
     C                   UPDATE    PUP01
     C*
     C                   ENDIF
     C                   CALL      'F0060R'
     C                   PARM      @@CALL        ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR
     C*** END ADD *** 06/11/98 ***************************************
     C*
     C*================================================================
     C* OPT11 - SUBSCRIBER T/M RATE INFORMATION
     C*================================================================
     C*
     C     OPT11         BEGSR
     C*
     C                   CALL      'F0046R'
     C                   PARM      @@CALL        ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OPT14 - Darts - Option 14
     C*================================================================
     C     OPT14         BEGSR
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCSUB#        QCSUB#
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
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
     C*** BEGIN ADD *** 10/28/02 **************************************
     C     *IN17         IFEQ      *ON
     C                   MOVE      @@CALL        SVCALL
     C                   MOVE      *IN09         SVIN09
     C*
     C     SCAUTH        IFEQ      'I'
     C                   MOVE      *ON           *IN09
     C                   MOVE      '1'           @@CALL
     C                   ENDIF                                                  SCAUTH=I
     C                   ENDIF
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C*** BEG ADD *** 06/11/98 ***************************************
     C     SCSEL         CASEQ     '10'          OPT10
     C*** END ADD *** 06/11/98 ***************************************
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*** BEGIN ADD *** 10/28/02 **************************************
     C     *IN17         IFEQ      *ON
     C                   MOVE      SVCALL        @@CALL
     C                   MOVE      SVIN09        *IN09
     C                   ENDIF
     C*** END   ADD *** 10/28/02 **************************************
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
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     11            SF1PAG            5 0
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
     C*
     C     ##MED         IFNE      'Y'
     C*
     C******************   MOVELCLRFKY    OFD,5            DEL 10/06/98
     C*** BEG ADD *** 08/06/98 **************************************
     C                   MOVEL     CLRFKY        OFD(6)
     C*** END ADD *** 08/06/98 **************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVEL'FB'      KKKEY
     C*
     C********** FBKY      CHAINSFB01                99
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXARIN    IFNE 'Y'
     C     ##AR          IFNE      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C                   MOVEL     CLRFKY        OFD(4)                         NO OPT 7
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 10/28/02 **************************************
     C*
     C*  F6=ADD
     C*
     C                   MOVEL(P)  '2'           XXOPFN
     C                   MOVEL(P)  '06'          XXNMBR
     C*
     C     ##MRSU        IFEQ      'Y'
     C                   MOVEL(P)  'NNN'         XXMIS
     C                   ELSE
     C                   MOVEL(P)  'YNN'         XXMIS
     C                   ENDIF                                                  ##MRSU=Y
     C*
     C                   EXSR      UPDOFA
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C*** BEGIN CHG *** 07/20/98 **************************************
     C*
     C***21***** KEY1      READEDBRCDF                   95
     C   21
     CANN16KEY1          READE     DBRCDF                                 95
     C   21
     CAN 16KEY2          READE     DBRCDF                                 95
     C*
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C*** BEGIN CHG *** 07/20/98 **************************************
     C*
     C***21***** KEY1      REDPEDBRCDF                   95
     C   21
     CANN16KEY1          READPE    DBRCDF                                 95
     C   21
     CAN 16KEY2          READPE    DBRCDF                                 95
     C*
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
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
     C     SSSTAT        ANDNE     SMSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     *IN22         IFEQ      *ON
     C     *IN16         ANDEQ     *ON
     C     UPSUB#        ANDNE     SMSUB#
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 07/20/98 **************************************
     C*** BEGIN ADD *** 10/28/02 **************************************
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVE      'A'           SCAUTH
     C*
     C     ##MRSU        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *ON
     C                   CALL      'SA008R'      RSTPL1                 99
     C*
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C*
     C                   ENDIF                                                  ##MRSU=Y
     C                   ENDIF
     C*** END   ADD *** 10/28/02 **************************************
     C*
     C                   ENDSR
** Column headings
 Sub#     Subscriber Name                            Sts
Subscriber Name                               Sub#   Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
1077=A/R info               YYN
11010=Groups                YYN
11111=T/M Rates             YYN
11414=DARTS                 YYN
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
