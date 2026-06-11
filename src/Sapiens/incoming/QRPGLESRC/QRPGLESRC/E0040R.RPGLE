     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0040R - BATCH NUMBER MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR BATCH NUMBER MASTER MAINTENANCE          *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: E0040R02                                            *
     H*                                                               *
     H* CREATED: 08/13/01                                             *
     H*                                                               *
     H* 08/13/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       CANNOT DELETE IF PAYROLL RECORD EXISTS              *
     H*  72       CANNOT DELETE IF EXCESS CLAIMS RECORD EXISTS        *
     H*                                                               *
     H*****************************************************************
     FE0040F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FEMEBP     IF   E             DISK
     F                                     RENAME(PEB01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FEMEBL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEB01:DBRCDF)
      *
      *  Employer master file
     FEMEMP     IF   E             DISK
      *
      *  Fund master file
     FEMFNL     IF   E           K DISK
      *
      *  Payroll claims file
     FEMEPL     IF   E           K DISK
      *
      *  Excess claims file
     FEMEEL     IF   E           K DISK
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Batch Numbe-
     D                                     r')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('E0040R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D DELETD          C                   CONST('*Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D*================================================================
     D*  Data Structures
     D*================================================================
      *
     D                 DS
     D  VIEW1                  1     70
     D  V1YEAR                 3      5
     D  V1FRDT                 8     17
     D  V1TODT                20     29
     D  V1STAT                35     35
      *

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
      *
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
      * Employers batch key used for READE and REDPE
      *
     C     EBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *
      * Employers batch key used for SETLL and SETGT
      *
     C     EBKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKYEAR
      *
      * Fund master key used to obtain name of group
      *
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      * Key list for the employer payroll master file
      *
     C     EPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKYEAR
      *
      * Key list for the excess/non-excess clains file
      *
     C     EEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKYEAR
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
      *
     C     *LIKE         DEFINE    EBCO#         KKCO#
     C     *LIKE         DEFINE    EBFND         KKFND
     C     *LIKE         DEFINE    EBEMP#        KKEMP#
     C     *LIKE         DEFINE    EBYEAR        KKYEAR
      *
     C/COPY SCOPYWW01C
      *
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      *
     C     SCSEL         IFEQ      '4'
     C                   CLEAR                   PFRCDF
     C     SCDBRR        CHAIN     PFRCDF                             60
     C                   Z-ADD     EBCO#         KKCO#
     C                   Z-ADD     EBFND         KKFND
     C                   Z-ADD     EBEMP#        KKEMP#
     C                   Z-ADD     EBYEAR        KKYEAR
      *
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        71  90
     C                   ENDIF                                                  EPKY
      *
     C     EEKY          CHAIN     PEE01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        72  90
     C                   ENDIF                                                  EEKY
     C                   ENDIF                                                  SCSEL = 4
      *
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
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
      *
     C                   CLEAR                   PEM01
     C     @@RRN         CHAIN     PEM01                              60
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     EMFND         KKFND
     C                   Z-ADD     EMEMP#        KKEMP#
     C                   Z-ADD     0             KKYEAR
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        KKYEAR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCYEAR        KKYEAR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   Z-ADD     EBYEAR        SCYEAR
     C                   MOVE      EBSTAT        SCSTAT
      *
     C                   Z-ADD     EBFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
      *
     C                   Z-ADD     EBTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
      *
     C                   MOVEL     SCYEAR        V1YEAR
     C                   MOVEL     SCSTAT        V1STAT
      *
     C                   MOVEL(P)  SCFRDT        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C                   MOVEL     ##NUM         V1FRDT
      *
     C                   MOVEL(P)  SCTODT        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM      'DATE'        ##MASK
     C                   MOVEL     ##NUM         V1TODT
      *
     C                   MOVEL     VIEW1         SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* OPT8 - SUBROUTINE FOR OPTION 09 WORK WITH PAYROLLS
     C*================================================================
     C     OPT8          BEGSR
      *
     C                   CALL      'E0045R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C                   ENDSR
     C*================================================================
     C* OPT9 - SUBROUTINE FOR OPTION 09 WORK WITH CLAIMS
     C*================================================================
     C     OPT9          BEGSR
      *
     C                   CALL      'E0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 CHANGE STATUS
     C*================================================================
     C     OPT20         BEGSR
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
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
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
      * Fill fields at top of screen
      *
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   MOVEL(P)  EMNAME        SCEMNM
      *
     C                   CLEAR                   PFN01
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNNM
      *
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21EBKY          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21EBKY          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21EBKY1         SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21EBKY1         SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
      *
     C     Q1PRMS        IFGE      1
     C                   MOVE      *INKC         @@KC
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      2
     C                   MOVE      *INKL         @@KL
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      5
     C                   MOVEL     SCFND         @@FND
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      6
     C                   MOVEL     SCEMP#        @@EMP#
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt  Batch   From date     To date   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1088=Payroll                YYN
1099=Excess/Nonexcess ClaimsYYN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
3
