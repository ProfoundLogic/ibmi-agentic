     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0075R - WORK WITH ENTITY/LOCATION DETAIL
     H*****************************************************************
     H*  CALLED FROM: W0076R - W W ENTITY/LOCATIONS                   *
     H*                                                               *
     H*  CALLS TO: WOO79R02                                           *
     H*                                                               *
     H*  CREATED: 02/05/99  PROGRAMMER: C:87  PROJECT: WC878          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/17/07 C:136 SUPPORT: WA099 59135                           *
     H*                PULL OPTION 8 PER CG                           *
     H*                                                               *
     H* 06/01/06 C:168 PROJECT 129WNX                                 *
     H*                Added handle for field Total Payroll           *
     H*                                                               *
     H* 03/21/03 C:61  PROJECT 129WAJ                                 *
     H*                Call W0665R5 directly if policy year passed in.*
     H*                                                               *
     H*  02/03/03 C:136 PROJECT: 129WAI                               *
     H*                 Add option to call W/W Location Payroll Info  *
     H*                                                               *
     H*****************************************************************
     H*
     FW0075F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDLDP     IF   E             DISK
     F                                     RENAME(PLD01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDLDL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PLD01:DBRCDF)
     F*
     F*  Additional Entity/Location file
     FWMALP     IF   E             DISK
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     F*  Admin/darts control definition file
     FWMEXL     IF   E           K DISK
      ***** BEG ADD ******* 03/21/03 **********************************
      *
      *  Employer Policy Year
     FWDELP     IF   E             DISK
      ***** END ADD ******* 03/21/03 **********************************
      *
      *
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
      *** BEG CHG **** 02/03/03 ******************************************
     D******************  OF      1  11  1   OFD    35   Options, Function
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
      *** END CHG **** 02/03/03 ******************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Entity/Location Deta-
     D                                     il')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0075R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D DELETD          C                   CONST('*Deleted')
     D*
     D*  Data structure for screen
     D                 DS
     D  ZZDATA                 1     70
     D  ZZFYR                  3      5
     D  ZZEMPY                15     21
      *****  BEG  ADD  06/01/06 ***************************************
     D  ZZTPYR                35     55
      *****  END  ADD  06/01/06 ***************************************
     D*  Data structure for DARTS
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          wmalp
      ***** BEG ADD ******* 03/21/03 **********************************
     C                   PARM                    @@RRN3            9 0          wdelp
      ***** END ADD ******* 03/21/03 **********************************
     C*  Note : @@RRN is passed in as the RRN of WMALP
     C*
     C*         if @@RRN3 is passed in, WW Location Payroll (W0660R5) is
     C*         called directly.
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
      ***** BEG ADD ******* 03/21/03 **********************************
     C                   PARM                    ##RRN3            9 0          wdelp
      ***** END ADD ******* 03/21/03 **********************************
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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKELID
     C                   KFLD                    KKFYR
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKELID
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
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    LDCO#         KKCO#
     C     *LIKE         DEFINE    LDFND         KKFND
     C     *LIKE         DEFINE    LDEMP#        KKEMP#
     C     *LIKE         DEFINE    LDDIV         KKDIV
     C     *LIKE         DEFINE    LDELID        KKELID
     C     *LIKE         DEFINE    LDFYR         KKFYR
     C     *LIKE         DEFINE    EMEMP#        QQEMP#
     C     *LIKE         DEFINE    EMDIV         QQDIV
     C     *LIKE         DEFINE    EMNAME        QQNAME
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C*
     C/COPY SCOPYWW01C
     C*
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
      *
      *
      *
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
      ***** BEG DEL ******* 03/21/03 **********************************
     C*********  @@RRN     CHAINPAL01                60
     C*********            Z-ADDALCO#     KKCO#
     C*********            Z-ADDALFND     KKFND
     C*********            Z-ADDALEMP#    KKEMP#
     C*********            Z-ADDALDIV     KKDIV
     C*********            MOVE ALELID    KKELID
      ***** END DEL ******* 03/21/03 **********************************
     C                   MOVE      *LOVAL        KKFYR
     C*
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKFYR
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCFYR         KKFYR
     C*
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
     C*
     C*  Move record values into hidden subfile screen fields
     C                   MOVE      LDFYR         SCFYR
     C                   MOVE      LDEMPY        SCEMPY
     C*
     C*  Edit stuff for output
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  LDFYR         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         ZZFYR
     C*
     C                   MOVEL(P)  LDEMPY        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         ZZEMPY
     C*
     C**** BEG  ADD  06/01/06 *****************************************
     C                   MOVEL     'X,XXX.XX'    EDTMSK
     C                   MOVEL(P)  LDTPYR        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         ZZTPYR
     C**** END  ADD  06/01/06 *****************************************
     C*
     C                   MOVEL(P)  ZZDATA        SCDATA
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OPT8   - Location Payroll Information
     C*================================================================
     C     OPT8          BEGSR
      *
     C                   CALL      'W0660R5'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C                   ENDSR                                                  OPT8
      *
      *
      *
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
      *** BEG ADD **** 02/03/03 ******************************************
     C     SCSEL         CASEQ     '8'           OPT8
     C                   ENDCS
      *** END ADD **** 02/03/03 ******************************************
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
      ***** BEG ADD ******* 03/21/03 **********************************
      *
     C     @@RRN         CHAIN     PAL01                              60
     C                   Z-ADD     ALCO#         KKCO#
     C                   Z-ADD     ALFND         KKFND
     C                   Z-ADD     ALEMP#        KKEMP#
     C                   Z-ADD     ALDIV         KKDIV
     C                   MOVE      ALELID        KKELID
      *
      ***** END ADD ******* 03/21/03 **********************************
     C*
     C*  Get fund name
     C*
     C                   Z-ADD     ALFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Get employer name
     C*
     C                   Z-ADD     ALEMP#        SCEMP#
     C                   Z-ADD     ALDIV         SCDIV
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*    Compress emp#/div/name
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        QQEMP#
     C                   PARM      EMDIV         QQDIV
     C                   PARM      EMNAME        QQNAME
     C                   PARM                    QQEDN            80
     C*
     C                   MOVEL     QQEDN         SCEDN
     C*
     C                   MOVE      ALELTP        SCELTP
     C                   MOVE      ALELID        SCELID
     C                   Z-ADD     ALEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
      ***** BEG ADD ******* 03/21/03 **********************************
      *
      *  Call WW Location Payroll directly if this pgm called from
      *  Mass Audit.
      *
     C                   Z-ADD     *ZERO         ##RRN3
      *
     C     Q1PRMS        IFGE      5
     C     @@RRN3        ANDNE     *ZERO
     C                   Z-ADD     @@RRN3        ##RRN3
     C     @@RRN3        CHAIN     WDELP                              60
     C                   Z-ADD     ELFYR         KKFYR
     C     KEY1          CHAIN     DBRCDF                             60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DBRRN         SCDBRR
     C                   EXSR      OPT8
     C                   SETON                                        LR
     C                   RETURN
      *
     C                   ELSE
      *
     C                   EXFMT     WDW01
      *
     C     *INKL         IFEQ      *ON
     C                   SETON                                        LR
     C                   RETURN
      *
     C                   ELSE
      *
     C                   EXSR      KEYINZ
     C                   ENDIF                                                  INKL
      *
     C                   ENDIF                                                  KEY1
      *
     C                   ENDIF                                                  Q1PRMS GE 5
      *
      ***** END ADD ******* 03/21/03 **********************************
      *
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
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
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
Opt    Year    Number of Employees         Total Payroll
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
