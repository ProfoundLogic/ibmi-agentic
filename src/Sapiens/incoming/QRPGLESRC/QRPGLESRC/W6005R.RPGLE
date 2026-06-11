     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W6005R - Work With Daily Check Schedules
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W6005C                                             *
     H*                                                               *
     H* CALLS TO:  W6005R02  Daily Check Schedule Maintenance         *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/22/13 C:287 Seq#: SY266_OVR                                *
     H*                   Added Activation group compile options      *
     H*                                                               *
     H* 01/20/03 C:15  Support: WC121/35609                           *
     H*                Added edit to prevent the user from deleting a *
     H*                record if the checks have already printed for  *
     H*                the selected record.                           *
     H*                                                               *
     H* 04/13/01 C:61  PROJECT WC1020                                 *
     H*                Created.                                       *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       CHECK UPDATED AND CAN'T BE DELETED                  *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW6005F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
      *  Override all WACP* to WAC3* if Group Check (@@GRCK = G)
      *
     FWACPP     IF   E             DISK
     F                                     RENAME(PCP01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
      *  Override all WACP* to WAC3* if Group Check (@@GRCK = G)
     FWACPL5    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCP01:DBRCDF)
      *
      *  Override all WACP* to WAC3* if Group Check (@@GRCK = G)
     FWACPL6    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCP01:DBRCDF2)
      *
      *  Override all WACP* to WAC3* if Group Check (@@GRCK = G)
     FWACPL7    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCP01:DBRCDF3)
      *
      *  Override all WACP* to WAC3* if Group Check (@@GRCK = G)
     FWACPL8    IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PCP01:DBRCDF4)
      *
      *
      *
      /COPY SCOPYWW01D
     D HD              S             79    DIM(4) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D OPTN            S             40    DIM(2) CTDATA PERRCD(1)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
      *
     D                 DS
     D  D1                     1     74
     D  D1PRDT                 1     10
     D  D1FND                 13     15
     D  D1ACCT                18     37
     D  D1CKTP                40     49
     D  D1CHK#                52     58
     D  D1STA0                64     64
     D  D1STA1                72     72
      *
     D                 DS
     D  D2                     1     74
     D  D2PRDT                 1     10
     D  D2ACCT                13     32
     D  D2FND                 35     37
     D  D2CKTP                40     49
     D  D2CHK#                52     58
     D  D2STA0                64     64
     D  D2STA1                72     72
      *
     D                 DS
     D  D3                     1     74
     D  D3FND                  1      3
     D  D3ACCT                 6     25
     D  D3PRDT                28     37
     D  D3CKTP                40     49
     D  D3CHK#                52     58
     D  D3STA0                64     64
     D  D3STA1                72     72
      *
     D                 DS
     D  D4                     1     74
     D  D4ACCT                 1     20
     D  D4FND                 23     25
     D  D4PRDT                28     37
     D  D4CKTP                40     49
     D  D4CHK#                52     58
     D  D4STA0                64     64
     D  D4STA1                72     72
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W6005R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(4)
     D SF1PAG          C                   CONST(12)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          WACPP
     C                   PARM                    @@GRCK            1            G=GROUP CHECK
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          WACPP
     C                   PARM      @@GRCK        ##GRCK            1            G=GROUP CHECK
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRDT
     C                   KFLD                    KKFND
     C                   KFLD                    KKACCT
     C                   KFLD                    KKCKTP
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRDT
     C                   KFLD                    KKACCT
     C                   KFLD                    KKFND
     C                   KFLD                    KKCKTP
      *
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKACCT
     C                   KFLD                    KKPRDT
     C                   KFLD                    KKCKTP
      *
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKACCT
     C                   KFLD                    KKFND
     C                   KFLD                    KKPRDT
     C                   KFLD                    KKCKTP
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    CPCO#         KKCO#
     C     *LIKE         DEFINE    CPPRDT        KKPRDT
     C     *LIKE         DEFINE    CPPRDT        S1PRDT
     C     *LIKE         DEFINE    CPFND         KKFND
     C     *LIKE         DEFINE    CPACCT        KKACCT
     C     *LIKE         DEFINE    CPCKTP        KKCKTP
     C     *LIKE         DEFINE    SCPRDS        OPTNM
      *
      *================================================================
     C     @@GRCK        IFEQ      'G'
     C                   MOVEL     OPTN(2)       OPTNM
     C                   ELSE
     C                   MOVEL     OPTN(1)       OPTNM
     C                   ENDIF
      *
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
      *
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
      *
     C     SCSEL         IFEQ      '4'
     C     SCSTA1        ANDEQ     'Y'
     C*** Begin add *** 01/20/03 **************************************
     C     SCSEL         OREQ      '4'
     C     SCSTA0        ANDEQ     'Y'
     C*** End add ***** 01/20/03 **************************************
     C                   SETON                                        71  90
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
      *
      *
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVE      *HIVAL        KKPRDT
     C                   Z-ADD     *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKACCT
     C                   MOVE      *LOVAL        KKCKTP
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
      ******  VIEW 1
      *
     C     *IN21         IFEQ      *ON
     C                   MOVE      ##CO#         KKCO#
      *
     C                   MOVEL     SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPST1
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPRDT
      *
     C                   ELSE
      *
     C                   MOVE      *HIVAL        KKPRDT
     C                   ENDIF
      *
     C                   MOVE      SCPT1A        KKFND
     C                   MOVEL(P)  SCPT1B        KKACCT
     C                   MOVE      *LOVAL        KKCKTP
     C                   ENDIF                                                  *IN21
      *
      ******  VIEW 2
      *
     C     *IN22         IFEQ      *ON
     C                   MOVE      ##CO#         KKCO#
      *
     C                   MOVEL     SCPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPST2
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SCPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPRDT
      *
     C                   ELSE
      *
     C                   MOVE      *HIVAL        KKPRDT
     C                   ENDIF
      *
     C                   MOVEL(P)  SCPT2A        KKACCT
     C                   MOVE      SCPT2B        KKFND
     C                   MOVE      *LOVAL        KKCKTP
     C                   ENDIF                                                  *IN22
      *
      ******  VIEW 3
      *
     C     *IN23         IFEQ      *ON
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      SCPST3        KKFND
     C                   MOVEL(P)  SCPT3A        KKACCT
      *
     C                   MOVEL     SCPT3B        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPT3B
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SCPT3B        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPRDT
      *
     C                   ELSE
      *
     C                   MOVE      *HIVAL        KKPRDT
     C                   ENDIF
      *
     C                   MOVE      *LOVAL        KKCKTP
     C                   ENDIF                                                  *IN23
      *
      ******  VIEW 4
      *
     C     *IN24         IFEQ      *ON
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL(P)  SCPST4        KKACCT
     C                   MOVE      SCPT4A        KKFND
      *
     C                   MOVEL     SCPT4B        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPT4B
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SCPT4B        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPRDT
      *
     C                   ELSE
      *
     C                   MOVE      *HIVAL        KKPRDT
     C                   ENDIF
      *
     C                   MOVE      *LOVAL        KKCKTP
     C                   ENDIF                                                  *IN24
      *
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVE      SCPRDT        KKPRDT
     C                   Z-ADD     SCFND         KKFND
     C                   MOVE      SCACCT        KKACCT
     C                   MOVE      SCCKTP        KKCKTP
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      ##CO#         SCCO#
     C                   Z-ADD     CPPRDT        SCPRDT
     C                   Z-ADD     CPFND         SCFND
     C                   MOVEL     CPACCT        SCACCT
     C                   MOVEL     SCCKTP        SCCKTP
     C                   MOVE      CPSTA0        SCSTA0
     C                   MOVE      CPSTA1        SCSTA1
      *
      *
     C                   Z-ADD     CPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         D1PRDT
     C                   MOVEL(P)  XXNUM         D2PRDT
     C                   MOVEL(P)  XXNUM         D3PRDT
     C                   MOVEL(P)  XXNUM         D4PRDT
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       D1PRDT
     C                   MOVEL(P)  *BLANKS       D2PRDT
     C                   MOVEL(P)  *BLANKS       D3PRDT
     C                   MOVEL(P)  *BLANKS       D4PRDT
     C                   ENDIF                                                  SRERR
      *
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL(P)  CPFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         D1FND
     C                   MOVEL     XXNUM         D2FND
     C                   MOVEL     XXNUM         D3FND
     C                   MOVEL     XXNUM         D4FND
      *
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL(P)  CPCHK#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         D1CHK#
     C                   MOVEL     XXNUM         D2CHK#
     C                   MOVEL     XXNUM         D3CHK#
     C                   MOVEL     XXNUM         D4CHK#
      *
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEL     CPACCT        D1ACCT
     C                   MOVEL     CPCKTP        D1CKTP
     C                   MOVEL     CPSTA0        D1STA0
     C                   MOVEL     CPSTA1        D1STA1
     C                   MOVEL(P)  D1            SCDATA
      *
     C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     1
     C                   MOVEL     CPACCT        D2ACCT
     C                   MOVEL     CPCKTP        D2CKTP
     C                   MOVEL     CPSTA0        D2STA0
     C                   MOVEL     CPSTA1        D2STA1
     C                   MOVEL(P)  D2            SCDATA
      *
     C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     1
     C                   MOVEL     CPACCT        D3ACCT
     C                   MOVEL     CPCKTP        D3CKTP
     C                   MOVEL     CPSTA0        D3STA0
     C                   MOVEL     CPSTA1        D3STA1
     C                   MOVEL(P)  D3            SCDATA
      *
     C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     1
     C                   MOVEL     CPACCT        D4ACCT
     C                   MOVEL     CPCKTP        D4CKTP
     C                   MOVEL     CPSTA0        D4STA0
     C                   MOVEL     CPSTA1        D4STA1
     C                   MOVEL(P)  D4            SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
      *  Set on LR so over-rides for group checks will work
      *
     C                   SETON                                        LR
      *
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C     @@CALL        COMP      '1'                                    09     INQUIRY MODE
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
     C   24              READ      DBRCDF4                                95
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C   23              READP     DBRCDF3                                95
     C   24              READP     DBRCDF4                                95
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C   24KEY4          SETGT     DBRCDF4                            96
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C   24KEY4          SETLL     DBRCDF4                            96
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C     Q1PRMS        IFGT      3
     C                   MOVE      SCDBRR        @@RRN
     C                   ENDIF
      *
     C                   ENDSR                                                  SLCTRD
      *
      *
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
      *
      *  Process date
      *
B01  C     SSPRDT        IFNE      *ZEROS
      *
     C                   MOVEL     SSPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSPRDT
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SSPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1PRDT
      *
     C     S1PRDT        IFNE      CPPRDT
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  S1PRDT          EROS
      *
E01  C                   ENDIF                                                  SRERR           EROS
      *
E01  C                   ENDIF                                                  SSFND           EROS
      *
      *  Group
      *
B01  C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     CPFND
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SSFND           EROS
      *
      *  Bank Account
      *
B01  C     SSACCT        IFNE      *BLANKS
     C     SSACCT        ANDNE     CPACCT
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SSFND           EROS
      *
      *  Bank code (Check type)
      *
B01  C     SSCKTP        IFNE      *BLANKS
     C     SSCKTP        ANDNE     CPCKTP
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SSFND           EROS
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
Opt Process Dt  Grp  Bank Account#         Bank Code   Check#   Printed Updated
Opt Process Dt  Bank Account#         Grp  Bank Code   Check#   Printed Updated
Opt Grp  Bank Account#         Process Dt  Bank Code   Check#   Printed Updated
Opt Bank Account#         Grp  Process Dt  Bank Code   Check#   Printed Updated
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
** Pgm screen names
Daily Check Schedules
Group Check Schedules
