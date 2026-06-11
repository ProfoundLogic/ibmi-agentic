     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2268R - WORK WITH CAMPUS LOCATION DETAIL
     H*****************************************************************
     H*  CALLED FROM: W2267R - W W CAMPUS CODES                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 07/16/03  PROGRAMMER: C:136 PROJECT: 25WNL          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/04/10 C:190 PROJECT: 25WRE                                *
     H*                 Added pgm  to current release.                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW2268F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDCYP     IF   E             DISK
     F                                     RENAME(PCY01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDCYL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCY01:DBRCDF)
     F*
     F*  Additional Entity/Location file
     FWMCLP     IF   E             DISK
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     F*  Admin/darts control definition file
     FWMEXL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Campus Location Deta-
     D                                     il')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2268R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D DELETD          C                   CONST('*Deleted')
     D*
     D*  Data structure for screen
     D                 DS
     D  ZZDATA                 1     70
     D  ZZFYR                  3      5
     D  ZZEMPY                15     21
     D*  Data structure for DARTS
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*  Note : @@RRN is passed in as the RRN of WMCLP
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
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKCMPS
     C                   KFLD                    KKFYR
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKCMPS
     C*
     C*  Fund master key list
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Employer master key list
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Darts/admin control defn key
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CYCO#         KKCO#
     C     *LIKE         DEFINE    CYFND         KKFND
     C     *LIKE         DEFINE    CYEMP#        KKEMP#
     C     *LIKE         DEFINE    CYDIV         KKDIV
     C     *LIKE         DEFINE    CYCMPS        KKCMPS
     C     *LIKE         DEFINE    CYFYR         KKFYR
     C     *LIKE         DEFINE    EMEMP#        QQEMP#
     C     *LIKE         DEFINE    EMDIV         QQDIV
     C     *LIKE         DEFINE    EMNAME        QQNAME
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C*
     C/COPY SCOPYWW01C
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '4'
     C* CHAIN TO WMEXP ( ADMIN DARTS FILE ) TO CHK SECURITY LEVEL
     C     EXKY          CHAIN     PEX01                              60
     C*
     C     ##LVL         IFLT      EXDLTL                                       NOT AUTHORIZED
     C     *IN60         OREQ      *ON                                          NOT FOUND
     C                   SETON                                        77  90
     C                   ENDIF                                                  ##LVL LT EXDLTL
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKFYR
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVE      SCPST1        KKFYR
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCFYR         KKFYR
     C                   ENDSR                                                  KEYSFL
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14=DARTS
     C*===============================================================
     C     KNDO          BEGSR
     C*
     C*  Establish key
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C*
     C*  Call DARTS entry program
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   ZZDATA
     C                   Z-ADD     CYFYR         SCFYR
     C                   Z-ADD     CYEMPY        SCEMPY
     C*
     C*  Edit stuff for output
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  CYFYR         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         ZZFYR
     C*
     C                   MOVEL(P)  CYEMPY        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         ZZEMPY
     C*
     C                   MOVEL(P)  ZZDATA        SCDATA
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@RRN         CHAIN     PCL01                              60
     C                   Z-ADD     CLCO#         SCCO#
     C                   Z-ADD     CLFND         SCFND
     C                   Z-ADD     CLEMP#        SCEMP#
     C                   Z-ADD     CLDIV         SCDIV
     C                   MOVE(P)   CLCMPS        SCCMPS
     C                   MOVEL     CLDESC        SCDESC
     C*
     C                   Z-ADD     SCCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C                   MOVE(P)   SCCMPS        KKCMPS
     C*
     C*  Get fund name
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Get employer name
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*    Compress emp#/div/name
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        QQEMP#
     C                   PARM      EMDIV         QQDIV
     C                   PARM      EMNAME        QQNAME
     C                   PARM                    QQEDN            80
     C*
     C                   MOVEL     QQEDN         SCEDN
     C*
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY2          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY2          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
** Column headings
Opt    Year    Number of Employees
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=DARTS                YYN
217F17=Subset               NNN
3
