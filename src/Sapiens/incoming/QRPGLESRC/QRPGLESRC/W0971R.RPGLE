     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  Auto Claim Letters                                             WCMC
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: W0971R                                            *
     H*    CREATED: 04/22/03  C:53 PROJ: WC1121                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 03/01/16 C:062 Project: SY251                                 *
     H*                Only allow edit/add auto letters if no         *
     H*                Scriptura.                                     *
     H*                                                               *
     H*  07/04/14  C:282 SUPPORT: 86093                               *
     H*                  Add More Keys for Prpper Rolling In pgm      *
     H*                                                               *
     H*  11/25/13 C:282 Project: 129WSC Priss# 18                     *
     H*                 Removed the "LT" column from the Grid.        *
     H*                                                               *
     H*****************************************************************
     FW0971F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWACLP     IF   E             DISK
     F                                     RENAME(PCL01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWACLL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCL01:DBRCDF)
     FWMUPL     IF   E           K DISK
     F*
     FWMDOL     IF   E           K DISK
     FWMFNP     IF   E             DISK
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Auto Claim Letters')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0971R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D DELETD          C                   CONST('*Deleted')
     D*
     D                 DS
     D  SCDATA                 1     70
     D*
     D  XSFND                  1      3
     D  XSOFC#                 5      9
     D  XSST                  12     13
     D  XSLTFL                15     15
     D  XSDPCD                18     22
     D  XSDCID                25     36
     D  XSDESC                39     53
     D  XSOUTQ                56     65
     D  XS#CPY                68     70

     D*** Begin Add *** 03/01/16 **************************************
     D  isScriptura    s              1
     D*** End   Add *** 03/01/16 **************************************

     I*** V2

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C     *INKC         PARM      *INKC         @@KC              1
     C     *INKL         PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
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
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      *ZEROS        ##DEC             3 0
     C                   PARM      *BLANK        ##THOU            1
     C*
     C     FNDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKOFC#
     C                   KFLD                    KKST
     C                   KFLD                    KKLTFL
      *** BEG ADD *** 07/04/14 *********************************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITM
      *** END ADD *** 07/04/14 *********************************************
     C*
     C     KEYR          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     DOKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    CLDPCD
     C                   KFLD                    CLFLDR
     C                   KFLD                    CLDCID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CLCO#         KKCO#
     C     *LIKE         DEFINE    CLFND         KKFND
     C     *LIKE         DEFINE    CLOFC#        KKOFC#
     C     *LIKE         DEFINE    CLST          KKST
     C     *LIKE         DEFINE    CLLTFL        KKLTFL
     C     *LIKE         DEFINE    CLLTFL        @@CALL
     C     *LIKE         DEFINE    DODEPT        KKDEPT
      *** BEG ADD *** 07/04/14 *********************************************
     C     *LIKE         DEFINE    CLIDT         KKIDT
     C     *LIKE         DEFINE    CLITM         KKITM
      *** END ADD *** 07/04/14 *********************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKPGM -
     C*================================================================
     C     CHKPGM        BEGSR
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0010R'      ##PGM            10
     C                   PARM      *BLANKS       ##ERR             1
     C                   MOVE      '2'           ##MODE
     C*
     C     ##ERR         IFEQ      *ON
     C*
     C                   CALL      'SA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'W0010R'      ##FNM            10
     C                   PARM      'W0010Q'      ##TNM            10
     C*
     C     ##RTN         IFEQ      'A'
     C*
     C                   SETON                                        90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        ##RRN
     C*
     C                   CALL      'W0010Q'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#
     C                   PARM      *ZEROS        ##FND
     C                   PARM      *BLANKS       ##FYR             3
     C                   PARM      1             ##VIEW            1 0
     C*
     C                   MOVE      ##FND         SSFND
     C                   MOVE      ##FND         SCFND
     C                   MOVE      ##FND         KKFND
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSFND         IFNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDBF - Convert database fields to screen format
     C*================================================================
     C*
     C     CVTDBF        BEGSR
     C*
     C                   MOVEL     CLFND         ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XTFND             3
     C*
     C*                    MOVELCLOFC#    ##NUM
     C*                    MOVEL*BLANKS   ##MASK
     C*                    CALL CHGNUM    CHGPL
     C*                    MOVEL##NUM     XTOFC#  5 P
     C*
     C                   MOVEL     CL#CPY        ##NUM
     C                   MOVEL     *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XT#CPY            3
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
     C* PROCESS SEARCHES
     C*
     C     *INKJ         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSFND'
     C*
     C                   EXSR      CHKPGM
     C     ##RRN         CHAIN     WMFNP                              60
     C                   MOVE      FNFND         SSFND
     C*
     C**********           MOVE '2'       ##MODE
     C**********           CALL 'W0010R'  ACDPL        99
     C*
     C                   OTHER
     C                   ENDSL
     C*
     C                   END                                                    *INKJ
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     UPXFND        IFEQ      'Y'
     C                   MOVE      *ON           *IN31
     C                   ENDIF
     C*
     C                   MOVEL     FNFND         SSFND
     C                   ENDSR
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C********             MOVEL*LOVAL    KKFND
     C                   MOVEL     *LOVAL        KKST
     C                   MOVEL     *LOVAL        KKLTFL
     C                   MOVEL     *LOVAL        KKOFC#
      *** BEG ADD *** 07/04/14 *********************************************
     C                   MOVEL     *LOVAL        KKIDT
     C                   MOVEL     *LOVAL        KKITM
      *** END ADD *** 07/04/14 *********************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCCO#         KKCO#
     C                   MOVEL     SCFND         KKFND
     C                   MOVEL     *LOVAL        KKOFC#
     C                   MOVEL     *LOVAL        KKST
      *** BEG ADD *** 07/04/14 *********************************************
     C                   MOVEL     *LOVAL        KKIDT
     C                   MOVEL     *LOVAL        KKITM
      *** END ADD *** 07/04/14 *********************************************
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     SCPST1        IFNE      *BLANKS
     C                   MOVEL     SCPST1        KKOFC#
     C                   ENDIF
     C*
     C     SCPS1A        IFNE      *BLANKS
     C                   MOVE      SCPS1A        KKST
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCCO#         KKCO#
     C                   MOVEL     SCFND         KKFND
     C                   MOVEL     SCOFC#        KKOFC#
     C                   MOVEL     SCST          KKST
     C                   MOVEL     SCLTFL        KKLTFL
      *** BEG ADD *** 07/04/14 *********************************************
     C                   MOVEL     SCIDT         KKIDT
     C                   MOVEL     SCITM         KKITM
      *** END ADD *** 07/04/14 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      CLCO#         SCCO#
     C                   MOVE      CLFND         SCFND
     C                   MOVE      CLST          SCST
     C                   MOVE      CLLTFL        SCLTFL
     C                   MOVE      CLOFC#        SCOFC#
      *** BEG ADD *** 07/04/14 *********************************************
     C                   MOVEL     CLIDT         SCIDT
     C                   MOVEL     CLITM         SCITM
      *** END ADD *** 07/04/14 *********************************************
     C*
     C*** CONVERT FIELDS
     C                   EXSR      CVTDBF
     C*
     C                   MOVE      *BLANKS       SCDATA
     C*
     C                   MOVE      XTFND         XSFND
     C                   MOVE      CLST          XSST
      *** Beg Add 11/25/13 *************************************************
     C*****              MOVE      CLLTFL        XSLTFL
     C                   MOVE      *BLANK        XSLTFL
      *** End Add 11/25/13 *************************************************
     C                   MOVE      CLDPCD        XSDPCD
     C                   MOVEL     CLDCID        XSDCID
     C                   MOVEL     DODESC        XSDESC
     C                   MOVE      XT#CPY        XS#CPY
     C                   MOVE      CLOUTQ        XSOUTQ
     C                   MOVE      CLOFC#        XSOFC#
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
     C     Q1USER        CHAIN     PUP01                              60
     C     @@RRN         CHAIN     PFN01                              60
     C                   MOVE      FNCO#         KKCO#
     C                   MOVE      FNFND         KKFND
     C                   MOVE      'CS'          KKDEPT
     C*** Begin Add *** 03/01/16 **************************************
     C*
     C* Is Scriptura Enabled?
     C*
     C                   call      'SA047R'
     C                   parm      ' '           isScriptura
     C*
     C                   Eval      scriptura = *off
     C                   If        isScriptura = 'Y'
     C*** No Change
     C                   Move      '1'           XXOPFN
     C                   Move      '02'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*** No Delete
     C                   Move      '1'           XXOPFN
     C                   Move      '04'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*** No Add
     C                   Move      '2'           XXOPFN
     C                   Move      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   Eval      scriptura = *on
     C                   EndIf
     C*** End   Add *** 03/01/16 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEYR          READE     DBRCDF                                 95
     C     DOKY          CHAIN     PDO01                              99
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEYR          READPE    DBRCDF                                 95
     C     DOKY          CHAIN     PDO01                              99
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
     C                   MOVEL     SCDBRR        ##RRN
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   SELECT
     C*
     C     SSFND         WHENNE    0
     C     CLFND         ANDNE     SSFND
     C                   MOVE      *OFF          MEETSS
     C*
E01  C                   ENDSL
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C******
     C******     STRING    BEGSR
     C******
     C******               CALL 'QCLSCAN'
     C******               PARM           U1PDSC
     C******               PARM 30        STRLEN  30
     C******               PARM 1         START   30
     C******               PARM           U2PDSC
     C******               PARM 30        PATLEN  30
     C******               PARM '0'       TRANS   1
     C******               PARM '1'       TRIM    1
     C******               PARM ' '       WILD    1
     C******               PARM           SCNRES  30
     C******
     C******     SCNRES    IFEQ 0
     C******               MOVE *OFF      MEETSS
     C******               ENDIF                           SCNRES=0
     C******
     C******               ENDSR                           STRING
     C******
** Column headings
Opt  Grp Office St    Dept   Doc ID        Description      Outq        Copies      // 11/25/13
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
