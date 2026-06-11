     H* (C) Copyright Progressive Data Solutions, Inc., 2002
     H*
     H* W1062R   - Work with Group Rate Set Codes
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0010R - Work with Groups                          *
     H*                                                               *
     H* CALLS TO: W0010R92 - Group Rate Set Code Information.         *
     H*                                                               *
     H* Created  03/05/02  C:61  Project: 114WCY                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW1062F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWMGRP     IF   E             DISK
     F                                     RENAME(PGR01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWMGRL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PGR01:DBRCDF)
      *
      * Rate Set Code Table
     FWMRSL     IF   E           K DISK
      *
      * Group Master
     FWMFNP     IF   E             DISK
      *
      *
      *
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
      *
      *
      *
      *
     D*+*+*+*/COPY SCOPYWW01I
      *
      *
      *
     D                 DS
     D  VIEW1                  1     74
     D  V1RTCD                 1      3
     D  V1DESC                 6     36
     D  V1JRST                46     47
     D  V1STAT                60     60
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Group Rate Set Codes')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1062R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
      *
      *
      *
     C/COPY SCOPYWW01C
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          WMGRP
     C                   PARM                    @@RRN1            9 0          WMFNP
      *
      *  Program must be called with Group WMFNP relative record number
      *
      *=================================================================
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          WMGRP
     C                   PARM      @@RRN1        ##RRN1            9 0          WMFNP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
     C     KEY1          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
      *
     C     KEY2          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    KKRTCD
      *
      *  SYSTEM-WIDE STATE CODE TABLE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    KKKEY            10
      *
      *  Rate set code file
      *
     C     RSKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    GRRTCD
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    GRRTCD        KKRTCD
      *
      *
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
      *
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
     C                   ENDSR                                                  CHKSSM
      *
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
      *
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
      *
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
      *
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKRTCD
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
     C                   MOVEL(P)  SCPST1        KKRTCD
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL(P)  SCRTCD        KKRTCD
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
     C                   CLEAR                   SCDATA
     C                   CLEAR                   VIEW1
      *
     C                   MOVEL     GRRTCD        SCRTCD
     C                   MOVEL     GRSTAT        SCSTAT
      *
      * ZZZ
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  GRRTCD        V1RTCD
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSDESC        V1DESC
     C  N60              MOVE      RSJRST        V1JRST
     C                   MOVEL(P)  GRSTAT        V1STAT
     C                   MOVEL(P)  VIEW1         SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         BEGSR
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR                                                  OPT20
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
     C                   ENDSR                                                  PGMEND
      *
      *
      *  ZZZ
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      * Get Group Info
      *
     C     @@RRN1        CHAIN     PFN01                              60
     C   60              SETON                                        LR
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCNAME
      *
      *
     C                   MOVEL     HD(2)         SCHDL2
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVE      SCDBRR        @@RRN
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
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     GRSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
Rate                                       Rating
Code  Description                          State         Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
213F13=User parms           NNN
217F17=Subset               YYY
3
