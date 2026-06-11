     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE N0420R - A/R INQUIRY
     H*
     H*****************************************************************
     H* Description: This program will allow the user to display      *
     H*              Invoices, across all cases.                      *
     H*                                                               *
     H* Called by: MENU                                               *
     H* Calls    :                                                    *
     H*                                                               *
     H* Created: 04/07/00  C:62  Project: AR001                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/23/01 C:62 PROJ:MCMNT                                     *
     H*                MAKE OPT 7 AND 8 UNAVAILABLE IN INQUIRY MODE.  *
     H*                                                               *
     H*  11/03/00 C:62 PROJ:AR001                                     *
     H*                CHANGE THE SF1PAG VALUE FROM 12 TO 8.          *
     H*                                                               *
     H*  08/24/00 C:62 PROJ:MCMNT                                     *
     H*                REMOVE HEADER AMT PARMS.                       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FN0420F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FNMIMP     IF   E             DISK
     F                                     RENAME(PIM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FNMIML2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PIM01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FNMIML3    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PIM01:DBRCDF2)
     FNMARL6    IF   E           K DISK
     FFMCMP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('A/R Inquiry')
     D OPTNM2          C                   CONST('Re-Invoice')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('N0440R')
     D COMAPI          C                   CONST('SA005R')
     D CHGNUM          C                   CONST('SA020R')
     D USPAPI          C                   CONST('S0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(8)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          NMIMP
     C                   PARM                    @@RRN2            9 0          FMCMP
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      *BLANKS       ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@RRN2        ##RRN             9 0          FMCMP
     C*
     C*  Change number fields to alpha fields parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C     CNMPL2        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKINV#
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTRDT
     C*
     C     ARKY          KLIST
     C                   KFLD                    IMCO#
     C                   KFLD                    IMSUB#
     C                   KFLD                    IMFND
     C                   KFLD                    IMFYR
     C                   KFLD                    IMCASE
     C                   KFLD                    IMINV#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    IMCO#         KKCO#
     C     *LIKE         DEFINE    ARSUB#        KKSUB#
     C     *LIKE         DEFINE    ARFND         KKFND
     C     *LIKE         DEFINE    ARFYR         KKFYR
     C     *LIKE         DEFINE    ARCASE        KKCASE
     C     *LIKE         DEFINE    ARINV#        KKINV#
     C     *LIKE         DEFINE    ARTRDT        KKTRDT
     C     *LIKE         DEFINE    ARTRTP        ##MODE
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*
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
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     *ZEROS        KKINV#
     C                   Z-ADD     *HIVAL        KKTRDT
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKINV#
     C   22              MOVE      SCPST2        SRDTI
     C   22              CALL      'SRCVTDAT'    DTCYMD
     C   22              MOVEL     SRDTO         KKTRDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              Z-ADD     SCINV#        KKINV#
     C   22              Z-ADD     SCTRDT        KKTRDT
     C*
     C                   ENDSR
     C*================================================================
     C* KGDO   - F7=A/R SUMMARY
     C*================================================================
     C     KGDO          BEGSR
     C*
     C                   CALL      'N0421R'
     C                   PARM      *BLANKS       ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@RRN2        ##RRN             9 0          FMCMP
     C                   PARM      *ZEROS        ##SUB#            5 0
     C                   PARM      *ZEROS        ##FND             3 0
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     IMTRDT        SCTRDT
     C                   Z-ADD     IMINV#        SCINV#
     C*
     C                   MOVEL(P)  IMINV#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXINV#            9
     C*
     C                   Z-ADD     IMTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   MOVEL(P)  XXTRDT        ##NUM
     C                   CALL      CHGNUM        CNMPL2
     C                   MOVEL     ##NUM         X1TRDT           10
     C*
     C   21              MOVEL(P)  XXINV#        SCDATA
     C   21              MOVE      X1TRDT        SCDATA
     C   22              MOVEL(P)  X1TRDT        SCDATA
     C   22              MOVE      XXINV#        SCDATA
     C*
     C                   Z-ADD     *ZEROS        SCINAM
     C                   Z-ADD     *ZEROS        SCPAAM
     C                   Z-ADD     *ZEROS        SCAJAM
     C*
     C     ARKY          CHAIN     PAR01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     ARTRTP        WHENEQ    'P'
     C                   ADD       ARAMT         SCPAAM
     C*
     C     ARTRTP        WHENEQ    'A'
     C                   ADD       ARAMT         SCAJAM
     C*
     C     ARTRTP        WHENEQ    'I'
     C                   ADD       ARAMT         SCINAM
     C*
     C                   ENDSL
     C*
     C     ARKY          READE     PAR01                                  99
     C*
     C                   ENDDO
     C*
     C     SCINAM        SUB       SCPAAM        SCTDUE
     C                   ADD       SCAJAM        SCTDUE
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - Invoice detail
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   CALL      'N0425R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*** BEGIN DEL *** 08/24/00 **************************************
     C**********           PARM SCINAM    ##IAMT  92
     C**********           PARM SCPAAM    ##PAMT  92
     C**********           PARM SCAJAM    ##AAMT  92
     C**********           PARM SCTDUE    ##TDUE  92
     C*** END   DEL *** 08/24/00 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C     *INKC         OREQ      *ON
     C*
     C                   SETOFF                                       KLKC
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - Re-invoice
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   CALL      'N7146C1'
     C                   PARM      OPTNM2        ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      KKCO#         ##CO              3 0
     C                   PARM      KKSUB#        ##SUB             5 0
     C                   PARM      KKFND         ##FND             3 0
     C                   PARM      KKFYR         ##FYR             3 0
     C                   PARM      KKCASE        ##CASE            7 0
     C                   PARM      SCINV#        ##INV#            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - Cash posting
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'N0430R'
     C                   PARM      *BLANKS       ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0          NMIMP
     C                   PARM      SCINAM        ##IAMT            9 2
     C                   PARM      SCPAAM        ##PAMT            9 2
     C                   PARM      SCAJAM        ##AAMT            9 2
     C                   PARM      SCTDUE        ##TDUE            9 2
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C                   SETOFF                                       KCKL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKG         CASEQ     *ON           KGDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1            WC INST
     C                   PARM      *BLANKS       ##MCIF            1            MC INST
     C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANKS       ##AR              1            MC AR INST
     C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
     C                   PARM      *BLANKS       ##UR              1            PRECERT INST
     C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
     C                   PARM      *BLANKS       ##EDI             1            EDI INST
     C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      KKCO#         #1CO#             3
     C                   MOVE      KKSUB#        #1SUB#            5
     C                   MOVE      KKFND         #1FND             3
     C                   MOVE      KKFYR         #1FYR             3
     C                   MOVE      KKCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCHD1         PARM      *BLANK        ##L1             78
     C     SCHD2         PARM      *BLANK        ##L2             78
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR
** Column headings
Opt   Invoice#   Date         Inv amt    Payments  Adjustments  Total Due
Opt    Date      Invoice#     Inv amt    Payments  Adjustments  Total Due
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
1066=Invoice detail         YYN
1077=Re-invoice             YNN
1088=Cash posting           YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=A/R summary           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
215F15=Fold/Truncate        YYY
217F17=Subset               NNN
3
