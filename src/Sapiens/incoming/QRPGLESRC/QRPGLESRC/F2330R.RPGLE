     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*******************************************************************
     H/TITLE F2330R - WORK WITH EOMB CODES
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*  07/18/23 C:342 Project: NY060                                  *
     H*                 NY Compliance: CARC/RARC Update                 *
     H*                                                                 *
     H*  04/17/14 C:079 Redmine: #4127                                  *
     H*                 Make activation group = *caller                 *
     H*                                                                 *
     H*  02/26/14 C:296 PROJECT: NC034                                  *
     H*                 FOR NC CALL F2330RNC                            *
     H*                                                                 *
     H*  07/05/12 C:280 PROJECT: 114mer                                 *
     H*                 FOR CK CALL F2330RCK                            *
     H*                                                                 *
     H*  04/06/07 C:118 PROJECT: CA002                                  *
     H*                 FOR CA CALL F2330RCA                            *
     H*                                                                 *
     H*  02/18/05 C:79 PROJECT:TX029                                    *
     H*                FOR TX CALL F2330RTX                             *
     H*                                                                 *
     H*  09/26/00 C:62 PROJECT:MCMNT                                    *
     H*                ADD TWO ADDITIONAL VIEWS.                        *
     H*                                                                 *
     H*  09/11/00 C:62 PROJECT:MC205                                    *
     H*                ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.   *
     H*                                                                 *
     H*  05/25/00 C:62 PROJECT:MC205                                    *
     H*                ADD F21=PRINT LIST                               *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*******************************************************************
     FF2330F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTEBP     IF   E             DISK
     F                                     RENAME(PEB01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFTABL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEB01:DBRCDF)
     F*** BEGIN ADD *** 09/26/00 **************************************
     F*
     F*  Secondary logical file definition (for displaying items)
     FFTEBL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEB01:DBRCDF2)
     F*
     F*  Tertiary logical file definition (for displaying items)
     FFTEBL2    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PEB01:DBRCDF3)
     F*** END   ADD *** 09/26/00 **************************************
     FSTABL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMB Codes')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D*** BEG DEL *** 02/18/05 ****************************************
     D************* 'F2330R01'            C         ACDPGM
     D*** END DEL *** 02/18/05 ****************************************
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D*** BEGIN ADD *** 05/25/00 **************************************
     D FLAG            C                   CONST('1              ')

     I*** END   ADD *** 05/25/00 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@ST              2
     C                   PARM                    @@KEY            15
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*** BEGIN ADD *** 09/26/00 **************************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDESC
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKXREF
     C                   KFLD                    KKKEY
     C*** END   ADD *** 09/26/00 **************************************
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    EBDESC        U1PDSC
     C     *LIKE         DEFINE    EBDESC        U2PDSC
     C*** BEGIN ADD *** 09/26/00 **************************************
     C     *LIKE         DEFINE    EBDESC        KKDESC
     C     *LIKE         DEFINE    EBXREF        KKXREF
     C*** END   ADD *** 09/26/00 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
B01  C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
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
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKKEY
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVE      *LOVAL        KKDESC
     C                   MOVE      *LOVAL        KKXREF
     C*** END   ADD *** 09/26/00 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*** BEGIN CHG *** 09/26/00 **************************************
     C**********           MOVELSCPST1    KKKEY
     C   21              MOVEL(P)  SCPST1        KKKEY
     C*** END   CHG *** 09/26/00 **************************************
     C*** BEGIN ADD *** 09/26/00 **************************************
     C   22              MOVEL(P)  SCPST2        KKDESC
     C   23              MOVEL(P)  SCPST3        KKXREF
     C   23              MOVEL(P)  *BLANKS       KKKEY
     C*** END   ADD *** 09/26/00 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C*** BEGIN CHG *** 09/26/00 **************************************
     C**********           MOVELSCKEY     KKKEY
     C   21              MOVEL(P)  SCKEY         KKKEY
     C*** END   CHG *** 09/26/00 **************************************
     C*** BEGIN ADD *** 09/26/00 **************************************
     C   22              MOVEL(P)  SCDESC        KKDESC
     C   23              MOVEL(P)  SCXREF        KKXREF
     C   23              MOVEL(P)  SCKEY         KKKEY
     C*** END   ADD *** 09/26/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/25/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   MOVE      ##CO#         ##CO
     C                   MOVEL(P)  FLAG          ##FLAG
     C*
     C                   CALL      'F7010C'
     C                   PARM                    ##CO              3
     C                   PARM                    ##FLAG           15
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C                   PARM      SCST          ##ST              2
     C                   PARM      SCSTNM        ##STNM           40
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 05/25/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EBSTAT        SCSTAT
     C                   MOVEL     EBKEY         SCKEY
     C*** BEGIN CHG *** 09/26/00 **************************************
      *** BEG CHG *** 01/06/99 ****************************************
     C**********           MOVELEBDESC    XXDESC 60
      *** Beg Chg *** 07/18/23 ****************************************
     C********           MOVEL     EBDESC        XXDESC           55
     C                   MOVEL     EBDESC        XXDESC           52
      *** End Chg *** 07/18/23 ****************************************
      *** END CHG *** 01/06/00 ****************************************
     C*** END   CHG *** 09/26/00 **************************************
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  EBDESC        SCDESC
     C                   MOVEL(P)  EBXREF        SCXREF
     C*** END   ADD *** 09/26/00 **************************************
     C*
     C*** BEGIN ADD *** 09/26/00 **************************************
      *** Beg Chg *** 07/18/23 ****************************************
     C*******            MOVEL     EBKEY         XXKEY             3
     C                   MOVEL     EBKEY         XXKEY             6
      *** End Chg *** 07/18/23 ****************************************
     C*** END   ADD *** 09/26/00 **************************************
     C*** BEGIN CHG *** 09/26/00 **************************************
     C**********           MOVELEBKEY     SCDATA    P
     C**********           MOVE XXDESC    SCDATA
      *** BEG ADD *** 01/06/99 ****************************************
     C**********           MOVE '     '   SCDATA
     C**********           MOVE EBXREF    SCDATA
      *** END ADD *** 01/06/99 ****************************************
     C   21              MOVEL(P)  XXKEY         XXDATA           60
     C   21              MOVE      XXDESC        XXDATA           60
     C   21              MOVEL(P)  XXDATA        SCDATA
     C   21              MOVE      EBXREF        SCDATA
     C*** END   CHG *** 09/26/00 **************************************
     C*** BEGIN ADD *** 09/26/00 **************************************
     C   22              MOVEL(P)  XXDESC        XXDATA           60
     C   22              MOVE      XXKEY         XXDATA           60
     C   22              MOVEL(P)  XXDATA        SCDATA
     C   22              MOVE      EBXREF        SCDATA
     C*
     C   23              MOVEL(P)  XXKEY         XXDATA           60
     C   23              MOVE      XXDESC        XXDATA           60
     C   23              MOVEL(P)  EBXREF        SCDATA
     C   23              MOVE      XXDATA        SCDATA
     C*** END   ADD *** 09/26/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 05/25/00 **************************************
     C*
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
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C*** BEGIN CHG *** 09/11/00 **************************************
     C**********           Z-ADD11        SF1PAG  50
     C                   Z-ADD     10            SF1PAG            5 0
     C*** END   CHG *** 09/11/00 **************************************
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*** BEG ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 04/23/07 ****************************************
     C     @@ST          IFEQ      'FL'
     C                   MOVEL(P)  'F2330RFL'    ACDPGM            8
     C                   ELSE
     C*** END ADD *** 04/23/07 ****************************************
     C*** BEG ADD *** 04/06/07 ****************************************
     C     @@ST          IFEQ      'CA'
     C                   MOVEL(P)  'F2330RCA'    ACDPGM            8
     C                   ELSE
     C*** END ADD *** 04/06/07 ****************************************
     C     @@ST          IFEQ      'TX'
     C                   MOVEL(P)  'F2330RTX'    ACDPGM            8
     C*** BEG ADD *** 07/05/12 ****************************************
     C                   ELSE
     C     @@ST          IFEQ      'CK'
     C                   MOVEL(P)  'F2330RCK'    ACDPGM
     C                   ELSE
     C*** Beg Add *** 02/26/14 ****************************************
     C     @@ST          IFEQ      'NC'
     C                   MOVEL(P)  'F2330RNC'    ACDPGM
     C                   ELSE
     C*** End Add *** 02/26/14 ****************************************
     C                   MOVEL(P)  'F2330R01'    ACDPGM
     C*** Beg Add *** 02/26/14 ****************************************
     C                   ENDIF
     C*** End Add *** 02/26/14 ****************************************
     C                   ENDIF
     C*** END ADD *** 07/05/12 ****************************************
     C                   ENDIF
     C*** BEG ADD *** 04/06/07 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/06/07 ****************************************
     C*** BEG ADD *** 04/23/07 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/23/07 ****************************************
     C*** END ADD *** 02/18/05 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C*** BEGIN ADD *** 09/26/00 **************************************
     C   22KEY1          READE     DBRCDF2                                95
     C   23KEY1          READE     DBRCDF3                                95
     C*** END   ADD *** 09/26/00 **************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C*** BEGIN ADD *** 09/26/00 **************************************
     C   22KEY1          READPE    DBRCDF2                                95
     C   23KEY1          READPE    DBRCDF3                                95
     C*** END   ADD *** 09/26/00 **************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C*** BEGIN ADD *** 09/26/00 **************************************
     C   22KEY3          SETGT     DBRCDF2                            96
     C   23KEY4          SETGT     DBRCDF3                            96
     C*** END   ADD *** 09/26/00 **************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C*** BEGIN ADD *** 09/26/00 **************************************
     C   22KEY3          SETLL     DBRCDF2                            96
     C   23KEY4          SETLL     DBRCDF3                            96
     C*** END   ADD *** 09/26/00 **************************************
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
     C*
     C                   MOVEL     SCKEY         @@KEY
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      70            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      70            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  EBDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     EBSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Code     Description                                           Xref   Sts
 Description                                           Code    Xref   Sts
Xref     Code    Description                                          Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           YYN
3
