     H*  (C) Copyright Progressive Data Solutions, Inc., 2003
     H*****************************************************************
     H/TITLE W0151R - WORK WITH INTERNAL ACCT COMMISSION RATES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY INTERNAL ACCT        *
     H*                COMMISION RATES.                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*    W0151R02 - INTERNAL ACCT REP COMM INFORMATION (OPT 2/5, F6)*
     H*                                                               *
     H*  CREATED: 01/15/03  PROGRAMMER: C:136 PROJECT:114WEY          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0151F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMICP     IF   E             DISK
     F                                     RENAME(PIC01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMICL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PIC01:DBRCDF)
     FWMFNP     IF   E             DISK
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Internal Acct Rep Co-
     D                                     mm Rates')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0151R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)
     C/COPY SCOPYWW01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##GRRN            9 0
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKGRP
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKGRP
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKNWRW
     C                   KFLD                    KKPLVL
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    ICCO#         KKCO#
     C     *LIKE         DEFINE    ICGRP         KKGRP
     C     *LIKE         DEFINE    ICEFDT        KKEFDT
     C     *LIKE         DEFINE    ICNWRW        KKNWRW
     C     *LIKE         DEFINE    ICPLVL        KKPLVL
     C*
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
      *
     C     SSSTAT        IFNE      *BLANK
     C                   SETON                                        11
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
      *
     C                   MOVE      'A'           SSSTAT
      *
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
      *
     C                   Z-ADD     ##CO#         KKCO#
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         KKGRP
     C*
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKNWRW
     C                   MOVE      *LOVAL        KKPLVL
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C                   MOVE      *LOVAL        KKNWRW
     C                   MOVE      *LOVAL        KKPLVL
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCGRP         KKGRP
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C                   MOVEL     SCNWRW        KKNWRW
     C                   Z-ADD     SCPLVL        KKPLVL
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     ICEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   MOVE      ICNWRW        SCNWRW
     C                   Z-ADD     ICPLVL        SCPLVL
     C                   Z-ADD     ICRATE        SCRATE
     C                   MOVE      ICSTAT        SCSTAT
      *
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20  (CHANGE STATUS)
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     FNFND         SCGRP
     C                   MOVEL     FNNAME        SCGRNM
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     ICSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
** Column headings
Opt    Eff Date  Renew    Premium Level     Rate   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
