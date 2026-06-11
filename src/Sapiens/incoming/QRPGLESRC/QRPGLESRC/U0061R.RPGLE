     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0061R - WORK WITH PRE/RE-CERT PROCEDURE CODES
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
      *                PROCEDURE CODES FOR A PRE/RE-CERT.             *
      *                THE PROCEDURE CODE MAY BE CHOSEN               *
      *                FOR UPDATE, DISPLAY OR SELECTION DEPENDING     *
      *                UPON THE CALLING PROGRAM.                      *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    U0060R   - WORK WITH PRE-CERTS, OPTION 9                   *
      *    U0060R01 - PRE-CERT MAINT                                  *
      *  CALLS TO:                                                    *
      *    U0061R01 - PRE/RE-CERT PROCEDURE CODE MAINTENANCE          *
      *    U0060R02 - PRE/RE-CERT ADDITIONAL INFO MAINTENANCE         *
      *                                                               *
      *  CREATED:  2/20/98    C:47      PROJECT: 87MCB                *
      *****************************************************************
      * REVISIONS:
      *
      *  06/07/00 C:062 PROJECT: MCMNT
      *                 SHORTEN THE TITLE SO THAT IT DOESN'T TRUNCATE.
      *
      *  03/06/00 C:062 PROJECT: URMNT **BASE ONLY**
      *                 ADD SUBSET OF EXTENSION AND REMOVE EXT FROM KEY
      *
      *  05/26/99 C:062 PROJECT: MC145
      *                 ADD SEQUENCE # TO KEY FOR PAGING
      *
      *****************************************************************
      *
     FU0061F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FUDPOP     IF A E             DISK
     F                                     RENAME(PPO01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEGIN CHG *** 03/06/00 **************************************
     F**********UDPOL   IF  E           K        DISK
     FUDPOL5    IF   E           K DISK
     F*** END   CHG *** 03/06/00 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPO01:DBRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEGIN CHG *** 03/06/00 **************************************
     F**********UDPOL1  IF  E           K        DISK
     FUDPOL6    IF   E           K DISK
     F*** END   CHG *** 03/06/00 **************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPO01:DBRCDF2)
     F*
     F*  Pre-Cert Master
     FUMPCP     IF   E             DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPC01:PPC0P)
     FUMPCL     IF   E           K DISK
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT  Descend
     FUMPCL9    IF   E           K DISK
     F                                     RENAME(PPC01:PPC09)
      *
      /COPY SCOPYWW01D
     D*
     D HDA             S             76    DIM(2) CTDATA PERRCD(1)              Column Headings L1
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D DATA            S             74    DIM(3)
     D DTA1            S              1    DIM(74)
     D DTA2            S              1    DIM(74)
     D*
     D***PY SCOPYWW01E
     D*
      *
      ***PY SCOPYWW01I
      *
     D                 DS                  INZ
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pre-Cert -
     D*** BEGIN CHG *** 06/07/00 **************************************
     D**********    'Procedure Codes'
     D                                     Proc Codes')
     D*** END   CHG *** 06/07/00 **************************************
     D OPTNMR          C                   CONST('Re-Cert -
     D                                     Procedure Codes')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('U0061R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(9)
     D DELETD          C                   CONST('*Deleted')
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM      SCDBRR        @@RRN             9 0
     C                   PARM                    QQCONT            1
     C                   PARM                    @@MODE            1
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQCERT
     C                   PARM                    QQEXT
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM                    QQCONT            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C*** BEGIN DEL *** 03/06/00 **************************************
     C**********           KFLD           KKEXT
     C*** END   DEL *** 03/06/00 **************************************
      *
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C*** BEGIN DEL *** 03/06/00 **************************************
     C**********           KFLD           KKEXT
     C*** END   DEL *** 03/06/00 **************************************
     C                   KFLD                    KKPROC
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   KFLD                    KKEXT
     C*** END   ADD *** 03/06/00 **************************************
     C*** BEGIN ADD *** 05/26/99 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 05/26/99 **************************************
      *
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C*** BEGIN DEL *** 03/06/00 **************************************
     C**********           KFLD           KKEXT
     C*** END   DEL *** 03/06/00 **************************************
     C                   KFLD                    KKDESC
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   KFLD                    KKEXT
     C*** END   ADD *** 03/06/00 **************************************
     C*** BEGIN ADD *** 05/26/99 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 05/26/99 **************************************
     C*
     C     KEY9          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    PCCERT
     C*
     C*   KEY LIST FOR THE PRE-CERTIFICATION MASTER FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCERT
     C*
     C     PCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C*   KEY LIST FOR THE PROCEDURE CODE FILE
     C*
     C     POKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C*** BEGIN DEL *** 03/06/00 **************************************
     C**********           KFLD           XXEXT
     C*** END   DEL *** 03/06/00 **************************************
     C*
     C     POKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C*** BEGIN DEL *** 03/06/00 **************************************
     C**********           KFLD           X1EXT
     C*** END   DEL *** 03/06/00 **************************************
     C                   KFLD                    POPROC
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   KFLD                    X1EXT
     C*** END   ADD *** 03/06/00 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    POSUB#        KKSUB#
     C     *LIKE         DEFINE    POFND         KKFND
     C     *LIKE         DEFINE    POFYR         KKFYR
     C     *LIKE         DEFINE    POCASE        KKCASE
     C     *LIKE         DEFINE    POCERT        KKCERT
     C     *LIKE         DEFINE    POEXT         KKEXT
     C     *LIKE         DEFINE    POEXT         XXEXT
     C     *LIKE         DEFINE    POEXT         X1EXT
     C     *LIKE         DEFINE    POPROC        KKPROC
     C     *LIKE         DEFINE    POSEQ#        KKSEQ#
     C     *LIKE         DEFINE    PODESC        KKDESC
     C     *LIKE         DEFINE    PODT          XXDT
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     *LIKE         DEFINE    POEXT         HDEXT
     C*** END   ADD *** 03/06/00 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     SSEXT         IFNE      *BLANKS
     C*
     C                   SETON                                        11
     C*
     C                   ENDIF
     C*** END   ADD *** 03/06/00 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  CAN NOT CHANGE INFO IF PRE-CERT IS CLOSED.
     C*
     C     SCSEL         IFEQ      '2 '
     C*
     C     SCSTAT        IFEQ      'C'
     C                   SETON                                        9074
     C                   ENDIF
     C*
     C                   ENDIF
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
     C                   MOVEL     'A'           SSSTAT
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL(P)  PCCERT        KKCERT
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           Z-ADDSCEXT     KKEXT
     C                   Z-ADD     *ZEROS        KKEXT
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVE(P)   *LOVAL        KKPROC
     C                   MOVE(P)   *LOVAL        KKSEQ#
     C                   MOVE(P)   *LOVAL        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL(P)  SCPST1        KKPROC
     C   21              MOVE(P)   *LOVAL        KKSEQ#
     C   22              MOVEL     SCPST2        KKDESC
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*** BEGIN DEL *** 05/26/99 **************************************
     C**********           Z-ADDPCSUB#    KKSUB#
     C**********           Z-ADDPCFND     KKFND
     C**********           Z-ADDPCFYR     KKFYR
     C**********           Z-ADDPCCASE    KKCASE
     C**********           MOVELPCCERT    KKCERT    P
     C**********           Z-ADDSCEXT     KKEXT
     C*** END   DEL *** 05/26/99 **************************************
     C                   MOVEL(P)  SCPROC        KKPROC
     C*** BEGIN CHG *** 05/26/99 **************************************
     C**********           MOVE *LOVAL    KKSEQ#    P
     C                   MOVE(P)   SCSEQ#        KKSEQ#
     C*** END   CHG *** 05/26/99 **************************************
     C                   MOVEL(P)  SCDESC        KKDESC
      *
     C     *INKF         IFEQ      *ON
     C                   MOVE(P)   *LOVAL        KKPROC
     C                   MOVE(P)   *LOVAL        KKSEQ#
     C                   MOVE(P)   *LOVAL        KKDESC
     C                   END
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C     SCEXT         IFGT      *ZERO
     C     WWCONS        CAT(P)    OPTNMR:1      ##DATA                         RE-CERT
     C                   ELSE
     C     WWCONS        CAT(P)    OPTNM:1       ##DATA                         PRE-CERT
     C                   END
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   MOVEL(P)  PCCERT        SCCERT
     C                   MOVEL(P)  PCSTAT        SCSTAT
     C                   MOVEL(P)  POPROC        SCPROC
     C                   Z-ADD     POSEQ#        SCSEQ#
     C                   MOVEL(P)  PODESC        SCDESC
     C                   MOVEL(P)  PODESC        XXDESC           33
      *
     C                   Z-ADD     POFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCFRDT            8
      *
      * EDIT FROM DATE
      *
     C                   MOVEL(P)  SCFRDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALFRDT           10
      *
     C                   Z-ADD     POTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCTODT            8
      *
      * EDIT TO DATE
      *
     C                   MOVEL(P)  SCTODT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALTODT           10
      *
      * ZERO SUPPRESS AUTHORIZED VISITS NUMBER
      *
     C                   MOVEL(P)  POVST         XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         XXVST             3
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   MOVEL(P)  POEXT         XXNUM
     C*
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM      *BLANKS       EDTMSK           10
     C                   MOVEL     XXNUM         JJEXT             3
     C*
     C     JJEXT         IFEQ      *BLANKS
     C*
     C                   MOVEL     '  0'         JJEXT
     C*
     C                   ENDIF
     C*** END   ADD *** 03/06/00 **************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LOAD SUBFILE - VIEW 1
      *
     C                   SELECT
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     POPROC        DTA1(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  7         Y
     C                   ADD       6             Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     XXDESC        DTA1(Y)
     C                   ADD       35            Y
     C                   MOVEA     POTOS         DTA1(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  5         Y
     C                   ADD       4             Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     XXVST         DTA1(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  5         Y
     C                   ADD       4             Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     ALFRDT        DTA1(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  12        Y
     C                   ADD       11            Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     ALTODT        DTA1(Y)
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   ADD       11            Y
     C                   MOVEA     JJEXT         DTA1(Y)
     C*** END   ADD *** 03/06/00 **************************************
     C                   MOVEA(P)  DTA1          SCDATA
     C                   MOVEA(P)  HDA(1)        SCHDLA
     C* VIEW 2
     C     HD#           WHENEQ    2
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     XXDESC        DTA2(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  35        Y
     C                   ADD       34            Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     POPROC        DTA2(Y)
     C                   ADD       7             Y
     C                   MOVEA     POTOS         DTA2(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  5         Y
     C                   ADD       4             Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     XXVST         DTA2(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  5         Y
     C                   ADD       4             Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     ALFRDT        DTA2(Y)
     C*** BEGIN CHG *** 03/06/00 **************************************
     C**********           ADD  12        Y
     C                   ADD       11            Y
     C*** END   CHG *** 03/06/00 **************************************
     C                   MOVEA     ALTODT        DTA2(Y)
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   ADD       11            Y
     C                   MOVEA     JJEXT         DTA2(Y)
     C*** END   ADD *** 03/06/00 **************************************
     C                   MOVEA(P)  DTA2          SCDATA
     C                   MOVEA(P)  HDA(2)        SCHDLA
      *
     C                   OTHER
     C                   MOVEA(P)  HDA(1)        SCHDLA
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* OPT20 - OPTION 20 DELETE
     C*================================================================
     C**         OPT20     BEGSR
     C**
     C**                   MOVEL'4 '      SCSEL
     C**                   EXSR OPT4
     C**                   MOVEL'20'      SCSEL
     C**
     C**                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '20'          OPT4
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     QQCONT        ANDEQ     'Y'
     C                   CALL      'U0060R02'    ACDPL1
      *
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           RSTPGM
     C                   END
      *
     C                   END                                                    *INKC = OFF
      *
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   MOVE      *OFF          *IN12
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           @@CALL
     C                   END
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET INFO FOR HEADING, LOAD KEY FIELDS FOR UMPCP
      *   SUB,GROUP,YEAR,CASE
      *
     C     @@RRN         CHAIN     PPC0P                              99
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL     PCCERT        KKCERT
     C                   Z-ADD     PCEXT         KKEXT
     C                   MOVEL(P)  PCCERT        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C                   MOVEL(P)  PCCERT        QQCERT           15
     C                   MOVE      PCEXT         QQEXT             3
      *
      * CALL API TO CREATE A TWO LINE HEADING
      *
     C                   MOVE      PCCO#         QQCO#             3
     C                   MOVE      PCSUB#        QQSUB#            5
     C                   MOVE      PCFND         QQFND             3
     C                   MOVE      PCFYR         QQFYR             3
     C                   MOVE      PCCASE        QQCASE            7
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C     SCL1HD        PARM      *BLANK        QQL1             78
     C     SCL2HD        PARM      *BLANK        QQL2             78
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DEFAULT PROCEDURE CODES FOR NEW RE-CERT
      *
     C     @@MODE        IFEQ      'A'
     C     PCEXT         ANDGT     *ZERO
     C                   Z-ADD     *ZERO         XXEXT
     C     POKY          SETLL     DBRCDF
     C     *IN60         DOUEQ     *ON
     C     POKY          READE     DBRCDF                                 60
     C     *IN60         IFEQ      *OFF
     C     POPROC        ANDNE     *BLANK
     C                   Z-ADD     PCEXT         X1EXT
     C     POKY1         CHAIN     DBRCDF                             61
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     PCEXT         POEXT
     C                   MOVEL(P)  'A'           POSTAT
     C                   MOVEL(P)  Q1USER        POIUSR
     C                   MOVEL(P)  Q1PGM         POIPGM
     C                   Z-ADD     XXDT          POIDT
     C                   Z-ADD     DSTIME        POITIM
     C                   MOVE      *BLANK        POUSER
     C                   MOVE      *BLANK        POPGM
     C                   Z-ADD     *ZERO         PODT
     C                   Z-ADD     *ZERO         POTIME
     C                   WRITE     PFRCDF
     C                   END
     C                   END
     C                   ENDDO
     C                   END
      *
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ACCUMULATE THE TOTAL NUMBER OF DAYS THRU THE LAST PRE/RE-CERT
      *
     C                   Z-ADD     *ZERO         SCTREC
     C     PCKY1         SETGT     PPC01
     C     *IN60         DOUEQ     *ON
     C     PCKEY         READPE    PPC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   ADD       PCRECD        SCTREC
     C                   END
     C                   ENDDO
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      * IF THIS PRE-CERT HAS BEEN CLOSED, DO NOT ALLOW RE-CERT.
      *
     C                   MOVE      *OFF          *IN41
     C     KEY9          SETLL     PPC09
     C     *IN60         DOUEQ     *ON
     C     KEY9          READE     PPC09                                  60
     C     *IN60         IFEQ      *OFF
     C     PCSTAT        IFEQ      'C'
     C                   MOVE      *ON           *IN41
     C                   MOVEL     CLRFKY        OFD(8)
     C                   LEAVE
     C                   END
     C                   END
     C                   ENDDO
      *
      *
     C                   MOVEA(P)  HDA(1)        SCHDLA
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  POCERT        SCCERT
     C                   MOVEL(P)  POEXT         SCEXT
     C*
     C     ##KL          IFEQ      *OFF
     C     ##KC          ANDEQ     *OFF
     C                   MOVEL(P)  DELETD        SCDATA
     C                   END
     C*
     C     ##KL          IFEQ      *ON
     C     ##KC          OREQ      *ON
     C     ##RRN         CHAIN     PFRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVDBSC
     C                   Z-ADD     ##RRN         SCDBRR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   MOVE      SCDBRR        @@RRN
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
      *
      * ONLY LOAD ACTIVE PROCEDURE CODES
      *
     C     POSTAT        IFEQ      'I'
     C                   MOVE      *OFF          MEETSS
     C                   END
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     SSEXT         IFNE      *BLANKS
     C*
     C                   MOVE      SSEXT         HDEXT
     C*
     C     HDEXT         IFNE      POEXT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 03/06/00 **************************************
      *
     C                   ENDSR
     C*================================================================
    1 *       2         3         4         5        6        7         8
678901*345679801234567890123465798012345678901234567901234678901234567890
** Column headings
                                                   Dates of Services
                                                   Dates of Services
** Column headings
Opt Code  Description                        TOS Vst    from       to      Ext
Opt Description                       Code   TOS Vst    from       to      Ext
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Delete                YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
