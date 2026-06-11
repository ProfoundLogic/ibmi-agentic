     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7419R - WORK WITH EXCLUDED STATES
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0051R - Work with Employer Policy Years           *
     H*                                                               *
     H* CALLS TO:  W7419R02 Excluded States Info Maint                *
     H*            SY601R - Search for valid state code               *
     H*                                                               *
     H* CREATED:                                                      *
     H*                                                               *
     H* 11/03/00 C:61  PROJECT 95WFF                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/10/04 C:65  PROJECT 114WAK                                 *
     H*                ALLOW PROGRAM TO BE CALLED FROM WW/GROUPS TO   *
     H*                PROVIDE FOR DEFAULT STATES ENTRY.              *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
      *
     FW7419F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWDXSP     IF   E             DISK
     F                                     RENAME(PXS01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWDXSL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PXS01:DBRCDF)
      *
      *  Employer Policy Year
     FWDELP     IF   E             DISK
      *
      *  State
     FSTSTL1    IF   E           K DISK
      *
      *  Employer Master
     FWMEML     IF   E           K DISK
      *
      *  Group Master
     FWMFNL     IF   E           K DISK
      *
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
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Excluded States')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W7419R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          WDXSP (Return)
     C                   PARM                    @@RRN1            9 0          WDELP
     C*** BEGIN ADD ***  02/10/04  ************************************
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C*** END ADD *****  02/10/04  ************************************
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN1            9 0          WDELP
     C                   PARM                    @2CO#             3
     C                   PARM                    @2FND             3
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
      *
      *  Excluded States
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKST
      *
      *  State
      *
     C     STKY          KLIST
     C                   KFLD                    KKKEY
      *
      *  EMPLOYER MASTER
      *
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *  GROUP MASTER
      *
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    XSCO#         KKCO#
     C     *LIKE         DEFINE    XSFND         KKFND
     C     *LIKE         DEFINE    XSEMP#        KKEMP#
     C     *LIKE         DEFINE    ELDIV         KKDIV
     C     *LIKE         DEFINE    XSFYR         KKFYR
     C     *LIKE         DEFINE    XSST          KKST
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    ELEMP#        ##EMP#
     C     *LIKE         DEFINE    ELDIV         ##DIV
     C     *LIKE         DEFINE    EMNAME        ##NAME
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CKSSM
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
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVE      *LOVAL        KKST
      *
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C   21              MOVE      SCPST1        KKST
      *
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVE      SCST          KKST
      *
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      XSST          SCST
     C                   MOVEL(P)  XSST          SCDATA
     C                   MOVEL     XSST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              CAT(P)    STNAME:2      SCDATA
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
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      * Get Employer Policy Year record
      *
     C*** BEGIN ADD ***  02/10/04  ************************************
     C                   MOVE      *OFF          *IN41
     C*
     C     Q1PRMS        IFGT      5
     C                   MOVE      *ON           *IN41
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@CO#         ELCO#
     C                   MOVE      @@FND         ELFND
     C                   Z-ADD     0             ELEMP#
     C                   Z-ADD     0             ELDIV
     C                   Z-ADD     0             ELFYR
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      @@CO#         @2CO#
     C                   MOVE      @@FND         @2FND
     C                   ELSE
     C                   MOVE      '000'         @2CO#
     C                   MOVE      '000'         @2FND
     C*** END ADD *****  02/10/04  ************************************
     C     @@RRN1        CHAIN     PEL01                              60
     C*** BEGIN ADD ***  02/10/04  ************************************
     C                   ENDIF                                                  Q1PRMS IFGT
     C*** END ADD *****  02/10/04  ************************************
     C   60              SETON                                        LR
     C   60              RETURN
      *
     C                   Z-ADD     ELCO#         KKCO#
     C                   Z-ADD     ELFND         KKFND
     C                   Z-ADD     ELEMP#        KKEMP#
     C                   Z-ADD     ELDIV         KKDIV
     C                   Z-ADD     ELFYR         KKFYR
      *
      *  Get Information for Header
      *
     C                   Z-ADD     ELFYR         SCFYR
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
      *
     C     EMKY          CHAIN     PEM01                              60
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      ELEMP#        ##EMP#
     C                   PARM      ELDIV         ##DIV
     C                   PARM      EMNAME        ##NAME
     C                   PARM                    ##EDN            80
      *
     C                   MOVEL(P)  ##EDN         SCEDN
      *
      * Get Group name
      *
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
      *
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
      *
     C   21KEY1          READE     DBRCDF                                 95
      *
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
      *
     C   21KEY1          READPE    DBRCDF                                 95
      *
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
      *
     C   21KEY2          SETGT     DBRCDF                             96
      *
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
      *
     C   21KEY2          SETLL     DBRCDF                             96
      *
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETD        SCDATA
      *
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
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
Opt  State
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YNY
217F17=Subset               NYY
3
