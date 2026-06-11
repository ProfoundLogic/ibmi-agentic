     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*==================================================================*
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*==================================================================*
     H/TITLE ISII23R - MC CLAIMS INDEX PROGRAM
     H*==================================================================*
     H* Description: This program indexes incoming images by MC          *
     H*              Case key.                                           *
     H*                                                                  *
     H* Called from: IS215R - Work With Queued Entries                   *
     H* Calls to   :                                                     *
     H*                                                                  *
     H* Created    : 12/30/1999                                          *
     H* Programmer : C:113                                               *
     H* Project    : 81WQW                                               *
     H*==================================================================*
     H* REVSIONS:                                                        *
      *                                                               *
      * 02/28/19 C:300 Support: 77447 (CoveRisk)                      *
      *                  - Indexing Options Not Displaying Properly   *
      *                                                               *
      * 10/17/11 C:279 Project: SY195                                 *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
      * 01/06/09 C:062 PROJECT: 114IRZ                                *
      *                ADD BARCODE INDEXING                           *
      *                                                               *
      * 08/25/06 C:062 PROJECT: VI114 - 54184                         *
      *                Default the diary user.                        *
      *                                                               *
      * 03/15/06 C:132 PROJECT: IM120 - 51809                         *
      *                Fix Diary Flag Problems                        *
      *                                                               *
      * 06/14/04 C:062 PROJECT: 129WCS                                *
      *                Add process to allow both a data entry pgm     *
      *                and reference an image                         *
      *                                                               *
      * 02/19/04 C:136 SUPPORT: DR129 40553                           *
      *                Ensure that the summarcy code (CMSMCD) gets    *
      *                a value from the Remark Table (STCTP) so       *
      *                that the summary code will display all images. *
      *                                                               *
      * 01/30/02 C:128 PROJECT: DR169                                 *
      *                If remark type changed,change description.     *
     H*                                                                  *
     H*                                                                  *
     H* ---------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.              *
     H* ---------------------------------------------------------------- *
     H*==================================================================*
     FISII23F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F** Image Batch Administration Master File
     FIMBML     IF   E           K DISK
     F*
     F** Image Document Type Master File
     FIMDTL     IF   E           K DISK
     F*
     F** Image Document Notes
     FIMDNL2    IF   E           K DISK
     F*
     F** DARTS Remark Detail File
     FSDCML     O    E             DISK
     F*
     FSDCMLI    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0I)
     F*
     F** DARTS Security Master File
     FSMSLL     IF   E           K DISK
     F*
     F** DARTS Remark Type Table File
     FSTCTL1    IF   E           K DISK
     F*
     F** System & Application Control File
     FSSYLF     IF   E           K DISK
     F*
     F** User Paramters File
     FSMUPL     IF   E           K DISK
     F*
     F** Work File
     FWXERP1    IF   E             DISK    USROPN
     F*
     F** Managed Care Subscriber Master File
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM0P)
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0F)
     F*** BEGIN ADD *** 06/14/04 **************************************
     FSDAIP     O    E             DISK
     F*** END   ADD *** 06/14/04 **************************************
     F*** BEGIN ADD *** 01/06/09 **************************************
     FIMRAL     IF   E           K DISK
     F*** END   ADD *** 01/06/09 **************************************

     D*==================================================================*
     D* Array & Table Definitions
     D*==================================================================*
     D CK1             S             80    DIM(5) CTDATA PERRCD(1)              F-KEYS
     D DN              S             75    DIM(4)                               DOCN's
     D QCA             S             55    DIM(1) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D PGMNAM          C                   CONST('MC Claim Index')
     D COMAPI          C                   CONST('WA001R')
     D CVTAPI          C                   CONST('FA703R')
     D*
     D MDOC            C                   CONST('To Darts:')
     D MSEN            C                   CONST('User :')
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('** INDEXED **')
     D*
     D** DARTS PNAM
     D SCPNAM          C                   CONST('F0269R')
     D*
     D** Date & Time structure
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** BEGIN ADD *** 01/06/09 **************************************
     D                 DS
     D  YYDKEY                 1     40
     D  YYCO#                  1      3
     D  YYSUB#                 4      8
     D  YYFND                  9     11
     D  YYFYR                 12     14
     D  YYCASE                15     21
     D*** END   ADD *** 01/06/09 **************************************
     D*
     D** File Information data structure
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D** Program status data structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*==================================================================*
     I* Data Structure & Constant Definitions
     I*==================================================================*
     IPCM0I
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#

     C*==================================================================*
     C* *LIKE DEFN Definitions
     C*==================================================================*
     C     *LIKE         DEFINE    CMKEY         DEKEY
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    DRAL02        OFCPGM
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CMCDAT        XXCDAT
     C     *LIKE         DEFINE    CMCDAT        XXDATE
     C     *LIKE         DEFINE    CMCDAT        XXDSDT
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMTRTM        XXTIME
     C*==================================================================*
     C* KLIST Definitions
     C*==================================================================*
     C*
     C** DARTS Additional Remarks file
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C** Image Batch Master file
     C     BMKY1         KLIST
     C                   KFLD                    QQBCHI
     C*
     C** DARTS Remarks Detail file
     C     CMKYA         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    ##SEQ#            5 0
     C*
     C** DART Remark Type file
     C     CTKY          KLIST
     C                   KFLD                    CMRMCD
     C*
     C     CTKY1         KLIST
     C                   KFLD                    SCCODE            5
     C*
     C** Image Document Notes file
     C     DNKY2         KLIST
     C                   KFLD                    QQDCID
     C*** BEGIN ADD *** 01/06/09 **************************************
     C     RAKY          KLIST
     C                   KFLD                    QQDCID
     C*** END   ADD *** 01/06/09 **************************************
     C*
     C** Image Document Type Table file
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
     C*
     C** DARTS Security file
     C     SLKY          KLIST
     C                   KFLD                    $$USER
     C*
     C** System Control file
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C** User Parameter file
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C***************
     C*  MC files
     C***************
     C** MC Master Subscriber file
     C     SMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C*
     C     FNKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCGRP
     C*
     C     FMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCGRP
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*==================================================================*
     C* PLIST Definitions
     C*==================================================================*
     C*
     C** Entry parameters
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQPTH            40
     C                   PARM                    QQDOCT            8
     C                   PARM                    QQDDSC           40
     C                   PARM                    QQQID            10
     C                   PARM                    QQQDSC           40
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQ#PGS            6 0
     C                   PARM                    QQSDAT            8 0
     C                   PARM                    QQSTIM            6 0
     C                   PARM                    QQSREP            8
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5 0
     C                   PARM                    QQRTCD            1
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQIFSP            1
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C** Date check/conversion parameters
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C** MC Claim Search
     C     ACDPL1        PLIST
     C                   PARM                    ##MODE            1
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQGRP             3 0
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8 0
     C                   PARM                    QQRTCD            1
     C** MC Subscriber Search
     C     ACDPL2        PLIST
     C                   PARM                    ##MODE            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C** MC/WC convert API
     C     CVTPL         PLIST
     C                   PARM                    #RTN              1
     C                   PARM                    #WCIN             1
     C                   PARM                    #WDCK             1
     C                   PARM                    #CO#              3
     C                   PARM                    #SUB              5
     C                   PARM                    #GRP              3
     C                   PARM                    #FYR              3
     C                   PARM                    #CASE             7
     C                   PARM                    #TMPKY           40
     C*==================================================================*
     C* Main
     C*==================================================================*
     C                   EXSR      PGMSTR
     C*
     C     *INKC         DOUEQ     *ON
     C     *INKL         OREQ      *ON
     C     *IN90         OREQ      *OFF
     C*
     C                   EXSR      PREDSP
     C                   EXFMT     SCR01
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C                   MOVE      *OFF          *IN97
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   EXSR      OTHFKS
     C*
     C     *IN97         IFEQ      *OFF
     C                   EXSR      ERRCK1
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C                   EXSR      FINISH
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C                   ENDIF                                                  *INKC = *OFF etc
     C*
     C                   ENDDO
     C*
     C                   EXSR      PGMEND
     C*
     C                   MOVE      *ON           *INLR
     C*====================================================================*
     C*  CHKKEY  SUBROUTINE - EDIT PMSC KEY FIELDS
     C*====================================================================*
     C     CHKKEY        BEGSR
     C*
     C** Subscriber
     C     *IN90         IFEQ      *OFF
     C     SMKEY         CHAIN     PSM01                              62
     C     *IN62         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF                                                  *IN62 = *ON
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C** Group
     C     *IN90         IFEQ      *OFF
     C     FNKEY         CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF                                                  *IN63 = *ON
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C** Case
     C     *IN90         IFEQ      *OFF
     C     FMKEY         CHAIN     PCM0F                              64
     C     *IN64         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF                                                  *IN64 = *ON
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C                   ENDSR
     C*====================================================================*
     C*  DNOT DOC NOTES  - CREATE DOC NOTES AND CPY NOTES TO SMAR
     C*====================================================================*
     C     DNOT          BEGSR
     C*
     C*  Fill all doc note fields.
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        #$NOWD            8
     C                   MOVE      DSTIME        #$NOWT            6
     C*
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     #$NOWD        XXM               2
     C                   MOVE      #$NOWD        XXY               4
     C     2             SUBST     #$NOWD:3      XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     #$NOWT        XXH               2
     C                   MOVE      #$NOWT        XXS               2
     C     2             SUBST     #$NOWT:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
     C*
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75
     C*
     C                   MOVEL     XX75          DN(1)
     C*
     C                   MOVE      QQSEQ#        $XSEQ             5
     C     MDOC          CAT(P)    CMKEY:1       XX75
     C                   CAT(P)    $XSEQ:5       XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C     MSEN          CAT(P)    Q1USER:1      XX75
     C                   CAT(P)    '.'           XX75
     C                   MOVEL     XX75          DN(3)
     C                   MOVEL     *BLANKS       DN(4)
     C*
     C*  Call the program to write doc notes.
     C*
     C                   DO        4             G                 3 0
     C*
     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(G)         QQCM75
     C*
     C     G             IFGE      4
     C                   MOVE      '1'           Q$RTC             1
     C                   ELSE
     C                   MOVE      ' '           Q$RTC
     C                   ENDIF
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    #$NOWD                           NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   ENDDO
     C*
     C*  Now we need to create the Additional Remarks for this
     C*   DART, by read/writing all Notes to the SMARP file.
     C*   We will also BACK DATE the Remarks, so they are NOT modifiable.
     C*
     C                   CALL      'ISNOTAR'
     C                   PARM                    QQDCID           12
     C                   PARM      CMKEY         Q@KEY            40
     C                   PARM      CMSEQ#        Q@SEQ#            5 0
     C                   PARM      *BLANKS       Q@RCD             1
     C*
     C                   ENDSR
     C*====================================================================*
     C*  ERRCK1  SUBROUTINE - EDIT SCREEN 1 FIELDS
     C*====================================================================*
     C     ERRCK1        BEGSR
     C*
     C*  Validate key fields.
     C*
     C                   EXSR      CHKKEY
     C*
     C*  Date entered must be valid.
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C  N90SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN52
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   MOVE      ' '           #DIARY            1             DIARY TYPE FLAG
     C*
     C*  Remark type must be entered and valid.
     C*
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C*
     C*  If remark is secured, the secuity level displayed on screen
     C*   is moved into the field to be stored when record is made
     C*   or updated.
     C*
     C     *IN56         IFEQ      *ON
     C     SCSECL        ANDNE     ##SECL
     C                   MOVEL     SCSECL        ##SECL
     C                   ELSE
     C     *IN56         IFEQ      *OFF
     C                   MOVEL     *ZEROS        SCSECL
     C                   MOVEL     SCSECL        ##SECL
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCCODE        IFNE      *BLANKS
     C     SCCODE        CHAIN     STCTL1                             60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN51
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  Determine if remark type is diary or not.
     C*
     C  N90SCDITP        IFEQ      'Y'
     C*
     C*  User name, if diary type, must be valid.
     C*
     C                   MOVEL     SCDUSR        $$USER
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN58
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                   *IN60 = *ON
     C                   ENDIF                                                   SCDITP = 'Y'
     C*
     C  N90SCDITP        IFNE      ' '
      ***** BEG CHG ***** 03/15/06 ***************************************
     C*********  SCDITP    ANDNE'Y'
     C     SCDITP        IFNE      'Y'
      ***** BEG CHG ***** 03/15/06 ***************************************
     C     SCDITP        ANDNE     'N'
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN90
      ***** BEG ADD ***** 03/15/06 ***************************************
     C                   ENDIF                                                  SCDITP NE 'Y'
      ***** BEG ADD ***** 03/15/06 ***************************************
     C                   ELSE
     C                   MOVE      CTFL01        SCDITP
     C                   ENDIF                                                  SCDITP NE ' '
     C*
     C*  Check Data Entry Flag to see if Data Entry Program has been
     C*     called once already.
     C*
     C     XXDEFL        IFEQ      'N'
     C*
     C*  If DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
     C*     if it has an associated Data Entry Program ID.
     C*
     C                   MOVE      'N'           XXDEPG            1
     C     SYKY          CHAIN     SDR01                              40
     C     *IN40         IFEQ      *OFF
     C     DRFL03        IFEQ      'Y'
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE      'Y'           XXDEPG
     C*
     C*  Doc ID not allowed if the remark type has an associated data
     C*   entry program.
     C*
     C  N90SCDCID        IFNE      *BLANKS
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C                   MOVE      *BLANKS       XXPGM            10
     C                   MOVEL     CTDEPG        XXPGM
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C  N90XXCDAT        IFLE      XXDSDT
     C*
     C     ERRFLE        IFEQ      ' '
     C                   CALL      'ERROR1C'
     C*
     C                   Z-ADD     55            LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              37
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH
     C*
     C                   OPEN      WXERP1
     C                   MOVE      'Y'           ERRFLE            1
     C                   ENDIF
     C*
     C     7             CHAIN     PER01                              37
     C     *IN37         DOWEQ     *OFF
     C*
     C*  Check for Recursive call to program
     C*
     C     ERPGM         IFEQ      XXPGM
     C                   MOVE      *ON           *IN47
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   READ      PER01                                  37
     C                   ENDDO
     C*
     C*  ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
     C*  (NOTE: It was necessary to break up the comment field of size
     C*   275 into DECOM1 and DECOM2 because the largest alpha-numeric
     C*   value allowed for a parameter is 256.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVE      *BLANKS       DEKEY
     C                   MOVE      *BLANKS       DECOM1
     C                   MOVE      *BLANKS       DECOM2
     C                   MOVE      *BLANKS       DERTCD
     C                   MOVEL     QQKEY         DEKEY
     C*
     C                   CALL      CTDEPG                               37
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
     C*
     C*
     C     *IN37         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      'Y'           XXDEFL            1
     C*
     C     DERTCD        IFEQ      'A'
     C     DERTCD        OREQ      'B'
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C     DERTCD        IFEQ      *BLANKS
     C                   MOVE      'Y'           XXDEWR
     C                   MOVE      *BLANKS       SCCOMM
     C     DECOM1        IFEQ      *BLANK
     C     DECOM2        ANDEQ     *BLANK
     C                   MOVEL     CTDESC        SCCOMM
     C                   MOVEL     CTDESC        DECOM1
     C                   ELSE
     C                   MOVEL     DECOM1        SCCOMM
     C                   MOVE      DECOM2        SCCOMM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     CTDEPG        SCDCID
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     XXCDAT        IFLE      XXDSDT
     C*
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C                   ENDIF                                                  CTDEPG NE 0
     C                   ENDIF                                                  DRFL03 = 'Y'
     C                   ENDIF                                                  *IN40 = 0
     C                   ENDIF
     C*
     C     *IN99         IFEQ      '0'                                           NO EDIT IF IMAGE
     C     XXDEFL        ANDEQ     'N'
     C     *IN90         ANDEQ     *OFF
     C*
     C*  Doc ID must be valid.
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCDFLR        XXFOLD           80
     C                   MOVEL     *BLANK        ZZRTCD            1
     C*
     C     SCDCID        IFNE      *BLANK
     C*
     C*  If AS/400 Office NOT Installed - Don't allow this function
     C*
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  Must have a Default Folder entered for this function
     C*
     C  N90XXFOLD        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  XXFOLD = *BLANKS
     C*
     C  N90              CALL      'SVALDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    ZZRTCD
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN54
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  QQRTCD = '1'
     C*
     C                   ENDIF                                                  DCID <> *BLANK
     C*
     C                   ENDIF                                                  99 = 0
     C*
     C*  Comments cannot be blank.
     C*
     C  N90SCCOMM        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN53
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCCOMM = *BLANKS
     C*
     C*  Additional Remarks flag is required.
     C*
     C     SCARMK        IFNE      'Y'
     C     SCARMK        ANDNE     'N'
     C                   MOVE      *ON           *IN46
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  Purgeable item flag must be 'Y', 'N' or blank.
     C*
     C  N90SCPGFL        IFNE      'Y'
     C     SCPGFL        ANDNE     'N'
     C     SCPGFL        ANDNE     *BLANK
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C*  If purgeable item flag is = 'Y'
     C*
     C     SCPGFL        WHENEQ    'Y'
     C*
     C  N90CTDPRG        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN43
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C  N90SCDITP        IFNE      'Y'
     C                   MOVE      *ON           *IN44
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C*  If purgeable item flag is = *BLANKS
     C*
     C     SCPGFL        WHENEQ    *BLANKS
     C*
     C     CTDPRG        IFNE      *ZEROS
     C     SCDITP        ANDEQ     'Y'
     C                   MOVE      'Y'           SCPGFL
     C                   ELSE
     C                   MOVE      'N'           SCPGFL
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*===============================================================*
     C*  FINISH
     C*===============================================================*
     C     FINISH        BEGSR
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '3'           QQRTCD
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      'C'           QQRTCD
     C                   ENDIF
     C*
     C     QQRTCD        IFNE      '3'
     C     QQRTCD        ANDNE     '8'
     C     QQRTCD        ANDNE     '9'
     C     QQRTCD        ANDNE     'A'
     C     QQRTCD        ANDNE     'C'
     C     QQRTCD        ANDNE     'K'
     C     QQRTCD        ANDNE     'P'
     C                   EXSR      PRSCTL
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KEDO - F5=Employer name search
     C*===============================================================*
     C     KEDO          BEGSR
     C*
     C                   MOVE      '2'           ##MODE            1
     C                   CALL      SCPNAM        ACDPL1                 99
     C     QQCASE        IFNE      *ZEROS
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQGRP         SCGRP
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   ENDIF
     C*
     C     SCCASE        IFEQ      *ZERO
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KFDO - F6=Bypass (ONLY FOR USER QUEUE)
     C*===============================================================*
     C     KFDO          BEGSR
     C     *IN73         IFEQ      *ON
     C*
     C*  Bypass by putting a "P" in the return code and ending.
     C*
     C                   MOVE      'P'           QQRTCD
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KGDO - F7=Document Notes
     C*===============================================================*
     C     KGDO          BEGSR
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   CALL      'IS016R'
     C                   PARM                    QQDCID
     C                   PARM      Q1PGM         XXPNAM           10
     C*
     C     DNKY2         CHAIN     PDN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KHDO - F8=Reject Document
     C*===============================================================*
     C     KHDO          BEGSR
     C*
     C     *IN73         IFEQ      *OFF
     C*
     C* Reject the Document being Indexed.  This will
     C*  call the Reject Document program  to allow
     C*  the entry of reason for the reject, Then update
     C*  the IE record, and the DO record with RJ   status.
     C*  Code will be RJ. (later a program will be available
     C*  to see the "rejects" and perform some processing on them.
     C*  The user will be required to fill in a reason for the
     C*  Reject in the program, which will be written to the
     C*  Doc Notes.
     C*
     C                   CALL      'IS157R'
     C                   PARM                    QQDCID
     C                   PARM      'RJ'          QQDSTS            2
     C                   PARM      'ISII01R '    XXPNAM           10
     C                   PARM      ' '           QQRTC             1
     C*
     C                   SELECT
     C     QQRTC         WHENEQ    'A'
     C                   MOVE      '3'           QQRTCD
     C                   MOVEL     *ON           *INLR
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C*
     C     QQRTC         WHENEQ    'B'
     C                   MOVE      *ON           *IN97
     C*
     C                   OTHER
     C*
     C*  If valid return from reject, bypass the current record.
     C                   MOVEL     '8'           QQRTCD
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN73 = *ON
     C*
     C                   ENDSR                                                  *INKH = *ON
     C*===============================================================*
     C*  KIDO - F9=Reassign document
     C*===============================================================*
     C     KIDO          BEGSR
     C*
     C                   MOVE      QQSDAT        #$SDT             8
     C                   MOVE      QQSTIM        #$STM             6
     C*
     C                   CALL      'IS115R'
     C                   PARM                    #$SDT
     C                   PARM                    #$STM
     C                   PARM                    QQDOCT            8
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDCT
     C                   PARM      *BLANKS       @#FLR             8
     C                   PARM                    @#SFLR           12
     C                   PARM      QQPTH         @#PTH            63
     C                   PARM                    QQQID
     C                   PARM      QQDDSC        @#DDSC           30
     C                   PARM      *BLANKS       @#RTC1            1
     C*
     C                   SELECT
     C*
     C     @#RTC1        WHENEQ    'A'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C*
     C     @#RTC1        WHENEQ    'B'
     C                   MOVE      *ON           *IN97
     C*
     C                   ENDSL
     C*
     C     QQRTCD        IFNE      'A'
     C                   MOVEL     *BLANKS       QQKEY
     C                   MOVEL     '9'           QQRTCD                         REASS. RTN
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KJDO - F10=Search
     C*===============================================================*
     C     KJDO          BEGSR
     C     SLF10         IFEQ      'Y'
     C*
     C                   SELECT
     C*
     C     FLD           WHENEQ    'SCDUSR  '
     C*
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    QQRTCD            1
     C*
     C     FLD           WHENEQ    'SCDEPT  '
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVE      *BLANK        ZZRTCD
     C*
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    ZZRTCD
     C*
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTFL01        SCDITP
      *** Begin Chg *** 01/30/02 ***********************************************
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVEL     CTDESC        SCCOMM
      *** End   Chg *** 01/30/02 ***********************************************
     C                   ENDIF
     C*
     C     FLD           WHENEQ    'SCRMTY '
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      *BLANK        ZZRTCD
     C*
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    ZZRTCD
     C*
     C                   MOVEL     SCCODE        SCDEPT
     C                   MOVE      SCCODE        SCRMTY
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CTFL01        SCDITP
      *** Begin Chg *** 01/30/02 ***********************************************
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVEL     CTDESC        SCCOMM
      *** End   Chg *** 01/30/02 ***********************************************
     C                   ENDIF
     C*
     C     FLD           WHENEQ    'SCSUB#'
     C                   MOVE      '2'           ##MODE            1
     C                   CALL      'F0055R'      ACDPL2                 99
     C     ##RRN         CHAIN     PSM0P                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCSUB#        OREQ      *ZERO
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDIF                                                  SLF10 = Y
     C*
     C                   ENDSR
     C*===============================================================*
     C* KMDO - F13=Change User Parms
     C*===============================================================*
     C     KMDO          BEGSR
     C*
     C                   CALL      'F0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C                   ENDSR
     C*===============================================================*
     C* KNDO - F14=Split
     C*===============================================================*
     C     KNDO          BEGSR
     C*
     C                   MOVE      QQSDAT        #$SDT             8
     C                   MOVE      QQSTIM        #$STM             6
     C*
     C     QQDCT         IFEQ      'F'
     C                   MOVEL     'F'           Q@RTC             1
     C                   ELSE
     C                   MOVEL     'I'           Q@RTC
     C                   ENDIF
     C*
     C                   MOVEL     'I'           Q@RTX
     C*
     C                   CALL      'IS215R3'
     C                   PARM      QQQID         $$QID            10
     C                   PARM      QQDCID        $$DCID           12
     C                   PARM                    #$SDT             8
     C                   PARM                    #$STM             6
     C                   PARM      QQDOCT        $$DOCT            8
     C                   PARM                    Q@RTX             1             FRM INDX PG
     C*
     C     Q@RTX         IFEQ      '3'
     C                   MOVEL     Q@RTX         QQRTCD
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KQDO - F16=New Index
     C*===============================================================*
     C     KQDO          BEGSR
     C*
     C                   MOVEL     'K'           QQRTCD
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *ON           *IN97
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KRDO - F17=User profile search
     C*===============================================================*
     C     KRDO          BEGSR
     C*
     C     SLF09         IFEQ      'Y'
     C*
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR           10
     C                   PARM                    ZZRTCD            1
     C*
     C                   ENDIF                                                  SLF09 = Y
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KXDO - F23=Secure/Unsecure remarks
     C*===============================================================*
     C     KXDO          BEGSR
     C*
     C     SLF23         IFEQ      'Y'
     C*
     C*  Remark UNSECURED to SECURED
     C*
     C     *IN56         IFEQ      *OFF
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              59
     C*
     C     *IN59         IFEQ      *ON
     C                   MOVE      SLSECL        SCSECL
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN56
     C                   MOVE      SCSECL        ##SECL            3 0
     C                   MOVE      'Y'           ##SBYL            1
     C*
     C*  Display "UNSECURE REMARKS" if "SECURE REMARKS" was displayed.
     C*
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  Remark SECURED to UNSECURED
     C*
     C                   MOVE      *OFF          *IN56
     C                   MOVE      *ZEROS        SCSECL
     C                   MOVE      SCSECL        ##SECL            3 0
     C                   MOVE      'N'           ##SBYL            1
     C*
     C*  Display "SECURE REMARKS" if "UNSECURE REMARKS" was displayed.
     C*
     C     C1            IFEQ      4
     C                   Z-ADD     3             C1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C                   ENDIF                                                  END SLF23=Y
     C*
     C                   ENDSR
     C*===============================================================*
     C*  KYDO - F24=More Keys
     C*===============================================================*
     C     KYDO          BEGSR
     C*
     C     SLF24         IFEQ      'Y'
     C*
     C                   SELECT
     C*
     C     C1            WHENEQ    1
     C     C1            OREQ      5
     C                   Z-ADD     2             C1
     C*
     C*  Remark Secured
     C*
     C     *IN56         WHENEQ    *ON
     C     C1            ANDEQ     2
     C                   Z-ADD     4             C1
     C*
     C*  Remark NOT Secured
     C*
     C     *IN56         WHENEQ    *OFF
     C     C1            ANDEQ     2
     C                   Z-ADD     3             C1
     C*
     C     C1            WHENEQ    3
     C     C1            OREQ      4
     C*
     C     *IN73         IFEQ      *ON
     C                   Z-ADD     5             C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C                   ENDIF                                                  END S24
     C*
     C                   ENDSR
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKE         CASEQ     *ON           KEDO                     90
     C     *INKF         CASEQ     *ON           KFDO                     90
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKH         CASEQ     *ON           KHDO                     90
     C     *INKI         CASEQ     *ON           KIDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKM         CASEQ     *ON           KMDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C     *INKQ         CASEQ     *ON           KQDO                     90
     C     *INKR         CASEQ     *ON           KRDO                     90
     C     *INKX         CASEQ     *ON           KXDO                     90
     C     *INKY         CASEQ     *ON           KYDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C     PGMEND        BEGSR
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '3'           QQRTCD
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      'C'           QQRTCD
     C                   ENDIF
     C*
     C                   ENDSR
     C*====================================================================*
     C* PGMSTR
     C*====================================================================*
     C     PGMSTR        BEGSR
     C*** BEGIN ADD *** 06/14/04 **************************************
     C*
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*** END   ADD *** 06/14/04 **************************************
     C*
     C                   MOVEL     PGMNAM        SCTITL
     C                   MOVEL     'SC'          XXSYK
     C                   Z-ADD     0             C                 5 0
     C                   Z-ADD     0             C1                5 0
     C                   MOVE      'N'           XXDEWR            1
     C                   MOVE      *BLANKS       DERTCD            1
     C                   MOVE      *BLANKS       DECOM1          256
     C                   MOVE      *BLANKS       DECOM2           19
     C*
     C                   MOVE      *BLANKS       DEKEY
     C*
     C*  Save the RTCD (F=fax, I=image, " "= scanned image)
     C*
     C                   MOVEL     QQRTCD        QQDCT             1
     C*
     C     QQDCT         IFNE      'F'
     C                   MOVE      *OFF          *IN73
     C                   ELSE
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             XXTIME
     C                   Z-ADD     0             XXDATE
     C                   Z-ADD     0             XXSEQ#
     C                   Z-ADD     *HIVAL        ##SEQ#
     C                   Z-ADD     0             ##SECL
     C                   MOVE      'N'           ##SBYL
     C*** GET USER PARM
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      UPCO#         XXCO#             3 0
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZEROS        XXCO#
     C                   ENDIF
     C*** BEGIN ADD *** 01/06/09 **************************************
     C*
     C* TRY TO RETRIEVE DATA FROM IMRAP
     C*
     C     RAKY          CHAIN     PRA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RADKEY        YYDKEY
     C                   MOVE      YYCO#         XXCO#
     C                   MOVE      YYCO#         SCCO#
     C                   MOVE      YYSUB#        SCSUB#
     C                   MOVE      YYFND         SCGRP
     C                   MOVE      YYFYR         SCFYR
     C                   MOVE      YYCASE        SCCASE
     C                   ENDIF
     C*** END   ADD *** 01/06/09 **************************************
     C*
     C                   MOVEL     Q1USER        $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       $$USER           10
     C     SLKY          CHAIN     PSL01                              60
     C                   ENDIF
     C*
     C                   MOVEL     SLFOLD        SCDFLR           80
     C*
     C     SLCCIF        IFNE      'Y'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF
     C*
     C*  Check System Control File Install Flags for this Client:
     C*
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  Check for AS/400 Office Installed
     C*
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1
     C                   MOVE      *OFF          *IN03
     C                   ELSE
     C                   MOVE      'N'           XXOFIF
     C                   MOVE      *ON           *IN03
     C                   ENDIF                                                  SYOFIF = 'Y'
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C*  Look at DARTS System Control File to determine what's installed.
     C*
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  If Image is installed, set on *IN99.
     C*
     C     DRFL01        IFEQ      'Y'
     C                   MOVE      *ON           *IN99
     C                   ENDIF
     C*
     C     DRFL03        IFEQ      'Y'
     C                   MOVE      *ON           *IN34
     C                   ENDIF
     C*
     C*  If Summary Code Option is installed, set on *IN36.
     C*
     C     DRFL02        IFEQ      'Y'
     C                   MOVE      *ON           *IN36
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C     *IN73         IFEQ      *ON
     C                   Z-ADD     5             C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF                                                  END DCT = F
     C*
      ***** BEG ADD ***** 02/28/19 ***********************************
      /Free
       av_keys = %subst(ck1(c1):1:55)  + '   ' + 'F10=Table File Search' +
        '   ' + %subst(ck1(2):20:35) + '   ' + %subst(ck1(3):1:35)   +
        '   ' + 'F23=Secure/Unsecure';
      /End-free
      ***** END ADD ***** 02/28/19 ***********************************
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C*  Load the record format info for the first time.
     C*
     C                   MOVE      XXCO#         SCCO#
     C                   MOVEL     QQDCID        SCDOCI
     C                   MOVEL     QQPTH         SCPTH
     C                   MOVEL     QQDOCT        SCDOCT
     C                   MOVEL     QQDDSC        SCDDSC
     C                   MOVEL     QQQID         SCQID            10
     C                   MOVEL     QQQDSC        SCQDSC           40
     C                   MOVEL     QQBCHI        SCBCHI
     C                   MOVE      QQ#PGS        SC#PGS
     C                   MOVE      QQSDAT        SCSDAT
     C                   MOVE      QQSTIM        SCSTIM
     C                   MOVEL     QQSREP        SCSREP
     C*
     C                   MOVEL     SCDDSC        SCCOMM
     C                   MOVEL     'N'           SCARMK
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SCCDAT
     C*
     C     DTKY          CHAIN     PDT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DTRMCD        SCDEPT
     C                   MOVE      DTRMCD        SCRMTY
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDEPT
     C                   MOVE      *BLANKS       SCRMTY
     C                   ENDIF
     C*
      ***** BEG ADD ***** 03/15/06 ***************************************
      * FILL IN THE DEFAULT DIARY TYPE HERE
     C                   MOVEL     SCDEPT        SCCODE
     C                   MOVE      SCRMTY        SCCODE
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CTFL01        SCDITP
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDITP
     C                   ENDIF                                                  *IN60  = *OFF
      ***** END ADD ***** 03/15/06 ***************************************
      ***** BEG ADD ***** 08/25/06 ***************************************
     C*
     C* DEFAULT THE DIARY USER TO CURRENT USER IF THE DIARY FLAG IS Y
     C*
     C     SCDITP        IFEQ      'Y'
     C                   MOVEL     Q1USER        SCDUSR
     C                   ENDIF
      ***** end ADD ***** 08/25/06 ***************************************
     C*
     C     BMKY1         CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BMBDSC        SCBDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
     C*
     C                   MOVEL     SCBDSC        SCCOMM
     C*
     C*  Any Non-System Doc notes exist?
     C*
     C     DNKY2         CHAIN     PDN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF
     C*
     C                   ENDSR
     C*====================================================================*
     C* PREDSP
     C*====================================================================*
     C     PREDSP        BEGSR
     C                   ENDSR
     C*====================================================================*
     C* PRSCTL
     C*====================================================================*
     C     PRSCTL        BEGSR
     C*
     C*  We have passed the edits, move screen key into CM record.
     C*
     C                   MOVE      SCCO#         #CO#
     C                   MOVE      SCSUB#        #SUB
     C                   MOVE      SCGRP         #GRP
     C                   MOVE      SCFYR         #FYR
     C                   MOVE      SCCASE        #CASE
     C                   MOVE      'Y'           #WCIN
     C                   MOVE      'Y'           #WDCK
     C                   MOVE      'K'           #RTN
     C*
     C                   CALL      CVTAPI        CVTPL
     C*
     C                   MOVEL     #TMPKY        ##KEY            40
     C*
     C                   MOVEL     SCPNAM        ##PNAM           10
     C*
     C                   EXSR      WCOM
     C*
     C*  EXSR here to write doc notes, and also to copy all doc notes
     C*  to the SMARP file for this new DART remark.
     C*
     C                   EXSR      DNOT
     C*
     C*  Retain the item on the users queue?
     C*
     C     SCRETN        IFEQ      'Y'
     C                   MOVEL     'R'           QQRTCD
     C                   ENDIF
     C                   ENDSR
     C*====================================================================*
     C*  WCOM  SUBROUTINE - WRITE REMARK RECORD
     C*====================================================================*
     C     WCOM          BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT              7 0
     C                   Z-ADD     DSTIME        HHTIME            6 0
     C*
     C                   MOVE      ##SECL        CMSECL
     C                   MOVE      ##SBYL        CMSBYL
     C*
     C                   MOVEL     ##KEY         CMKEY
     C                   MOVEL     *BLANKS       CMCSID
     C                   MOVEL     ##PNAM        CMPNAM
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVEL     SCCODE        CMRMCD
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      ' '           CMFL01
     C*** END   ADD *** 06/14/04 **************************************
      *** BEG ADD ***** 02/19/04 **********************************
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CTSMCD        CMSMCD
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     CTDEPG        ANDNE     *BLANKS
     C                   MOVE      'Y'           CMFL01
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       CMSMCD
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      *BLANKS       CTDEPG
     C*** END   ADD *** 06/14/04 **************************************
     C                   ENDIF
      *** END ADD ***** 02/19/04 **********************************
     C                   MOVEL     '***IMAGE'    CMDCID
     C                   MOVE      '*** '        CMDCID
     C                   MOVEL     SCCOMM        CMCOMM
     C*** BEGIN DEL *** 06/14/04 **************************************
     C**********           MOVE ' '       CMFL01
     C*** END   DEL *** 06/14/04 **************************************
     C*
     C*  Load fax folder.
     C*
     C                   MOVEL     QQPTH         CMFOLD
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C                   Z-ADD     HHDT          CMTRDT
     C                   Z-ADD     HHTIME        CMTRTM
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     QQUNCN        CMUNCN
     C                   MOVEL     QQIFSP        CMIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C*  Fill hidden fields.
     C*
     C                   MOVEL     SCIQUE        CMIQUE                             HIDDEN
     C                   MOVEL     SCIPTH        CMIPTH                             HIDDEN
     C                   MOVE      SCPIDX        CMPIDX                             HIDDEN
     C                   MOVEL     SCAPID        CMAPID                             HIDDEN
     C                   MOVEL     QQDCID        CMIDOC                             HIDDEN
     C                   MOVEL     SCQPTY        CMQPTY                             HIDDEN
     C                   MOVEL     SCDCTP        CMDCTP                             HIDDEN
     C*
     C*  Check fax flag.
     C*
     C     QQDCT         IFNE      'F'
     C                   MOVEL     'Y'           CMIMGF                             HIDDEN
     C                   ELSE
     C                   MOVEL     'F'           CMIMGF
     C                   MOVEL     QQDCID        CMDCID
     C                   MOVE      *BLANKS       CMIDOC
     C                   ENDIF
     C*
     C                   MOVEL     *BLANK        CMSTAT
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C*  If Diary user entered, write to CMUSER, else default to current user.
     C*
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVEL     SCDUSR        CMUSER
     C                   ELSE
     C                   MOVEL     Q1USER        CMUSER
     C                   ENDIF                                                  SCDUSR <> *BLANKS
     C*
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C     SCDITP        IFEQ      'Y'
     C     SCDITP        OREQ      'N'
     C                   MOVE      SCDITP        CMDIFL
     C                   ELSE
     C                   MOVE      ' '           CMDIFL
     C                   ENDIF
     C*
     C                   MOVEL     'D'           CMFL05
     C*
     C                   MOVE      SCPGFL        CMFL02
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
     C*
     C                   Z-ADD     *ZERO         CMDCDT
     C                   MOVEL     Q1PGM         CMIPGM
     C*
      *** BEG DEL ***** 02/19/04 *****************************************
     C******************** MOVE *BLANKS   CMSMCD
      *** END DEL ***** 02/19/04 *****************************************
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C*
     C*  Get the last SEQ# used.
     C*
     C                   MOVEL     ##KEY         QQKEY
     C*
     C     CMKYA         SETGT     PCM0I
     C                   READP     PCM0I                                  60
     C     *IN60         IFEQ      *OFF
     C     QQKEY         ANDEQ     PIKEY
     C     PISEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     0             CMSEQ#
     C                   END
     C*
     C                   WRITE     PCM01
     C                   FEOD      SDCML
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C                   EXSR      WRTAIP
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C*  If additional remarks are requestd, call program to add the remarks.
     C*
     C                   MOVEL     CMKEY         QQKEY
     C                   MOVE      CMSEQ#        QQSEQ#
     C                   MOVEL     Q1PGM         QQPNAM
     C*
     C     SCARMK        IFEQ      'Y'
     C                   CALL      'SC014R'
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ#
     C                   PARM                    QQPNAM           10
     C                   ENDIF
     C*
     C* Execute the user index exit here only for diary types.
     C*
     C     SCDITP        IFEQ      'Y'
     C*
     C                   MOVE      CMSEQ#        V@SEQ#
     C                   CALL      'ISE002C'                            99
     C                   PARM      QQQID         FQ               10
     C                   PARM      SCDUSR        TU               10
     C                   PARM      Q1USER        UID              10
     C                   PARM      CMDCID        DCID             12
     C                   PARM      CMKEY         V@KEY            40
     C                   PARM                    V@SEQ#            5
     C                   PARM      *BLANKS       RTRN              6
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 06/14/04 **************************************
     C*====================================================================*
     C* WRTAIP - WRITE SDAIP                                            * **
     C*====================================================================*
     C     WRTAIP        BEGSR
     C*
     C                   CLEAR                   PAI01
     C*
     C                   MOVEL(P)  CMKEY         AIKEY
     C                   Z-ADD     CMSEQ#        AISEQ#
     C*
     C                   MOVEL(P)  CTDEPG        AIDPGM
     C*
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL(P)  CMKEY         AIDKEY
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  Q1USER        AIUSER
     C                   MOVEL(P)  Q1PGM         AIPGM
     C                   Z-ADD     DSTIME        AITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C*
     C                   MOVEL(P)  Q1USER        AIIUSR
     C                   MOVEL(P)  Q1PGM         AIIPGM
     C                   Z-ADD     DSTIME        AIITIM
     C                   Z-ADD     AIDT          AIIDT
     C*
     C                   WRITE     PAI01
     C*
     C                   ENDSR
     C*** END   ADD *** 06/14/04 **************************************
     C*====================================================================*
      ***** BEG CHG ***** 02/28/19 ***********************************
      * Changed following CK1 Command Keys - SCR01 values
      *   F3=Exit   F5=Claim Search   F7=Notes   F8=Reject   F9=Reassign   F24=More keys
      *   F10=Search   F12=Previous   F13=Chg User Parms   F24=More keys
      *   F14=Split   F16=New Index Type   F23=Secure Remarks   F24=More keys
      *   F14=Split   F16=New Index Type   F23=Unsecure Remarks   F24=More keys
      *   F3=Exit   F5=Claim Search   F6=Bypass   F7=Notes   F24=More keys
      ***** END CHG ***** 02/28/19 ***********************************
**  CK1 - Command Keys - SCR01
F3=Exit  F5=Search  F7=Notes   F8=Reject   F9=Reassign   F24=More keys
F10=Search         F12=Previous   F13=Chg User Parms     F24=More keys
F14=Split    F16=New Index Type    F23=Secure Remarks    F24=More keys
F14=Split    F16=New Index Type    F23=Unsecure Remarks  F24=More keys
F3=Exit  F5=Search  F6=Bypass   F7=Notes   F9=Reassign   F24=More keys
**  QCMDEXC TABLE
OVRDBF FILE(WXERP1) TOFILE(QTEMP/WXERP1 )
