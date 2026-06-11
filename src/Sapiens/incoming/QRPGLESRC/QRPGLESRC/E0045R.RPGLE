     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0045R - EMPLOYER PAYROLL
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR EMPLOYER PAYROLL                        *
     H*                                                               *
     H* CALLED BY: E0040R                                             *
     H*                                                               *
     H* CALLS TO: E0045R02                                            *
     H*                                                               *
     H* CREATED: 08/17/01                                             *
     H*                                                               *
     H* 08/17/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H*****************************************************************
     FE0045F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FEMEPP     IF   E             DISK
     F                                     RENAME(PEP01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FEMEPL3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEP01:DBRCDF)
      *
      *  Employer master file
     FEMEML     IF   E           K DISK
      *
      *  Fund master file
     FEMFNL     IF   E           K DISK
      *
      *  Employer batch file
     FEMEBP     IF   E             DISK
      *
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Payroll')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('E0045R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D DELETD          C                   CONST('*Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D*================================================================
     D*  Data Structures
     D*================================================================
     D                 DS
     D  VIEW1                  1     70
     D  V1DRSC                 5      7
     D  V1CLAS                12     16
     D  V1RDAT                19     28
     D  V1AMT                 32     40
     D  V1STAT                49     49

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
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
      * Used for READE and REDPE
      *
     C     EPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKYEAR
      *
      * Used for SETLL and SETGT
      *
     C     EPKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKYEAR
     C                   KFLD                    KKDRSC
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKRDAT
      *
      * Fund master key used to obtain name of group
      *
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      * Employer master key used to obtain name of employer
      *
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
      *
     C     *LIKE         DEFINE    EPCO#         KKCO#
     C     *LIKE         DEFINE    EPFND         KKFND
     C     *LIKE         DEFINE    EPEMP#        KKEMP#
     C     *LIKE         DEFINE    EPYEAR        KKYEAR
     C     *LIKE         DEFINE    EPDRSC        KKDRSC
     C     *LIKE         DEFINE    EPCLAS        KKCLAS
     C     *LIKE         DEFINE    EPRDAT        KKRDAT
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
     C                   CLEAR                   PEB01
     C     @@RRN         CHAIN     PEB01                              60
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     EBFND         KKFND
     C                   Z-ADD     EBEMP#        KKEMP#
     C                   Z-ADD     EBYEAR        KKYEAR
     C                   MOVE      *LOVAL        KKDRSC
     C                   Z-ADD     0             KKCLAS
     C                   Z-ADD     0             KKRDAT
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL(P)  SCPST1        KKDRSC
     C                   Z-ADD     SCPSTA        KKCLAS
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL(P)  SCDRSC        KKDRSC
     C                   Z-ADD     SCCLAS        KKCLAS
     C                   Z-ADD     SCRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKRDAT
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     EPDRSC        SCDRSC
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   Z-ADD     EPAMT         SCAMT
     C                   MOVE      EPSTAT        SCSTAT
      *
     C                   Z-ADD     EPRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRDAT
      *
     C                   MOVE      SCDRSC        V1DRSC
      *
     C                   MOVEL(P)  SCCLAS        ##NUM
     C                   MOVEL     '2'           ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   MOVEL(P)  ##NUM         V1CLAS
      *
     C                   Z-ADD     EPRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM      'DATE'        ##MASK
     C                   MOVEL(P)  ##NUM         V1RDAT
      *
     C                   MOVEL(P)  SCAMT         ##NUM
     C                   MOVE      2             ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   MOVEL(P)  ##NUM         V1AMT
      *
     C                   MOVEL     SCSTAT        V1STAT
     C                   MOVEL(P)  VIEW1         SCDATA
      *
     C                   ENDSL
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
     C                   Z-ADD     EBFND         SCFND
     C                   Z-ADD     EBEMP#        SCEMP#
     C                   Z-ADD     EBYEAR        SCYEAR
      *
     C                   CLEAR                   PEM01
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCEMNM
      *
     C                   CLEAR                   PFN01
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNNM
      *
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21EPKY          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21EPKY          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21EPKY1         SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21EPKY1         SETLL     DBRCDF                             96
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt  Rate Code  Class   Rate Date  Payroll Amount  Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
3
