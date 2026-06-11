     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0050R - EMPLOYER EXCESS/NON-EXCESS CLAIMS
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR EMPLOYER EXCESS/NON-EXCESS CLAIMS       *
     H*                                                               *
     H* CALLED BY: E0040R                                             *
     H*                                                               *
     H* CALLS TO: E0050R02                                            *
     H*                                                               *
     H* CREATED: 08/27/01                                             *
     H*                                                               *
     H* 08/27/01 C:136 PROJECT WC577                                  *
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
     FE0050F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FEMEEP     IF   E             DISK
     F                                     RENAME(PEE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FEMEEL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEE01:DBRCDF)
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
     D OPTNM           C                   CONST('Excess/Non-Excess Cl-
     D                                     aims')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('E0050R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D DELETD          C                   CONST('*Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)

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
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
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
     C     EEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKYEAR
      *
      * Used for SETLL and SETGT
      *
     C     EEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKYEAR
     C                   KFLD                    KKACDT
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMULT
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
     C     *LIKE         DEFINE    EECO#         KKCO#
     C     *LIKE         DEFINE    EEFND         KKFND
     C     *LIKE         DEFINE    EEEMP#        KKEMP#
     C     *LIKE         DEFINE    EEYEAR        KKYEAR
     C     *LIKE         DEFINE    EEACDT        KKACDT
     C     *LIKE         DEFINE    EECASE        KKCASE
     C     *LIKE         DEFINE    EEMULT        KKMULT
      *
     C/COPY SCOPYWW01C
      *
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
      *
     C     SSACDT        IFNE      *ZEROS
     C     SSCASE        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
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
      *
     C     *IN90         IFEQ      *OFF
     C     SSACDT        ANDNE     *ZEROS
      *
     C                   Z-ADD     SSACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SSACDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71  90
     C                   ENDIF                                                  IF SRERR = 1
     C                   ENDIF                                                  IF 90 OFF
      *
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
     C                   Z-ADD     0             KKACDT
     C                   MOVE      *BLANKS       KKCASE
     C                   MOVE      *BLANKS       KKMULT
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
     C                   MOVE(P)   SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKACDT
      *
     C                   MOVE(P)   SCPSTA        KKCASE
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCACDT        KKACDT
     C                   MOVE(P)   SCCASE        KKCASE
     C                   MOVE(P)   SCMULT        KKMULT
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
      *
     C                   Z-ADD     EEACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   MOVE(P)   EECASE        SCCASE
     C                   MOVE(P)   EESTAT        SCSTAT
     C                   MOVE(P)   EEMULT        SCMULT
     C                   Z-ADD     EEAMT         SCAMT
     C                   Z-ADD     EETLCA        SCTLCA
      *
     C                   ENDSL
      *
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
     C   21EEKY          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21EEKY          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21EEKY1         SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21EEKY1         SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCCASE
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
     C                   Z-ADD     SSACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         S1ACDT            7 0
      *
     C     SSACDT        IFNE      *ZEROS
     C     S1ACDT        ANDNE     EEACDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSCASE        IFNE      *BLANKS
     C     SSCASE        ANDNE     EECASE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSR
** Column headings
Opt      Acc. Dt   Case             Stat   Mult          Amount   #Clms
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
