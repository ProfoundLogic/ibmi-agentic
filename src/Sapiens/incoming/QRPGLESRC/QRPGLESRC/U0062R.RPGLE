     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0062R - WORK WITH RE-CERTIFICATIONS
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
      *                RE-CERTIFICATIONS FOR A PRE-CERT.              *
      *                THE RE-CERT MAY BE CHOSEN                      *
      *                FOR UPDATE, DISPLAY OR SELECTION DEPENDING     *
      *                UPON THE CALLING PROGRAM.                      *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    U0060R   - WORK WITH PRE-CERTS                             *
      *  CALLS TO:                                                    *
      *    U0060R01 - PRE/RE-CERT MAINTENANCE                         *
      *    U0063R01 - RE-CERT EXPLANATION                             *
      *                                                               *
      *  PROGRAM: U0062R                                              *
      *  CREATED:  2/26/98    C:47      PROJECT: 87MCB                *
      * ***************************************************************
      *  FILE USAGE:                                                  *
      *
      * VIEW 1 - UMPCL10
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT
      *   - Select PCEXT NE 000
      *
      * UMPCL9
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT Descend
      *   - Used to get the Re-Cert# and Status
      * ***************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *  05/23/13 C:277 PROJECT: MC329                                *
      *                 Change the arreay length for ICD10 Diag       *
      *                                                               *
      *  01/26/01  C:62  PROJECT: MCMNT  BASE ONLY                    *
      *                  ENABLE F5=REFRESH                            *
      *                                                               *
      *  05/19/00  C:62  PROJECT: MC172                               *
      *                  RECON IS NOW IMPORTANT SO ADD TO PARM LIST.  *
      *                                                               *
      *  03/07/00 C:62 PROJECT: UR025                                 *
      *                NO PRE-CERT LETTERS IN INQUIRY MODE            *
      *                                                               *
      *  06/25/99 C:62 PROJECT: MC145                                 *
      *                SWAP OUT U0040R WITH F4110C(LETTERS).          *
      *                                                               *
      *  08/19/98 C:62 PROJECT: 98MAG                                 *
      *                ADDED OPT17=T/M AND SET EDI AND T/M OPTIONS    *
      *                TO BE CONTROLLED BY SYSTEM FLAGS.              *
      *                                                               *
      * ***************************************************************
      * INDICATORS:
      *                                                               *
      *  01 - Used on the PAGEDOWN keyword                            *
      *  02 - Used on the PAGEUP keyword                              *
      *  03 - Used to condition SFLCLR                                *
      * N03 - Used to condition SFLDSPCTL                             *
      *  04 - Used to condition SFLDSP                                *
      *  05 - Used to condition DSPATR(RI) on the selection field     *
      * N06 - Used to condition DSPATR(PC) on the position to field   *
      *  07 - Used to condition DSPATR(PC) on the selection field     *
      *  08 - Used to condition SFLNXTCHG                             *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      * N10 - Used to condition SFLEND (on message subfile)           *
      *  11 - Used to indicate the list is subsetted                  *
      *  12 - Used to condition SFLDROP/SFLFOLD                       *
      *  71 - These indicators are to used as error indicators for    *
      *       thru the various screens. They will set off after       *
      *       displaying each input capible screen.                   *
      *  90 - Used to general purpose error indicator                 *
      *  91 - Used to display CPF5203; roll up/down past last record  *
      *  95 - Used to condition SFLEND; also used to indicate EOF and *
      *       BOF on the file being read.                             *
      * ***************************************************************
     FU0062F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      * PRE-CERT FILE
     FUMPCP     IF   E             DISK
     F                                     RENAME(PPC01:PFRCDF)
      * VIEW 1
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT S ext<>0
     FUMPCL10   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPC01:DBRCDF)
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT  Descend
     FUMPCL9    IF   E           K DISK
     F                                     RENAME(PPC01:PPC09)
      *
      * PROVIDER file
     FFMPDL     IF   E           K DISK
      * USER PARM file
     FSMUPL1    IF   E           K DISK
      * DIAGNOSIS CODE FILE
     FFMDIL     IF   E           K DISK
     F*** BEGIN ADD *** 08/19/98 **************************************
     FFAEOLL    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F*** END   ADD *** 08/19/98 **************************************
      *
      /COPY SCOPYWW01D
     D HDA             S             76    DIM(1) CTDATA PERRCD(1)              Column Headings L1
     D HD              S             76    DIM(1) CTDATA PERRCD(1)              Column Headings L2
     D OF              S              1    DIM(28) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(28) ALT(OF)
     D DATA            S             74    DIM(1)
     D*** Beg Chg *** 05/23/13 **********************************
     D*** DTA1            S              1    DIM(74)
     D*** DTA2            S              1    DIM(74)
     D*** DTA3            S              1    DIM(74)
     D DTA1            S              1    DIM(76)
     D DTA2            S              1    DIM(76)
     D DTA3            S              1    DIM(76)
     D*** Beg Chg *** 05/23/13 **********************************
      *
     D***PY SCOPYWW01E
      *
     D***PY SCOPYWW01I
      *
      * /COPY required constants
      *
     D OPTNM           C                   CONST('Re-Certifications')
     D ACDPGM          C                   CONST('U0060R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA013R')
     D USPAPI          C                   CONST('F0006R')
      *
      * Program specific constants
      *
     D SF1PAG          C                   CONST(8)
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
      *
     D                 DS                  INZ
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *

     C     *LIKE         DEFINE    PCSUB#        KKSUB#
     C     *LIKE         DEFINE    PCFND         KKFND
     C     *LIKE         DEFINE    PCFYR         KKFYR
     C     *LIKE         DEFINE    PCCASE        KKCASE
     C     *LIKE         DEFINE    PCCERT        KKCERT
     C     *LIKE         DEFINE    PCEXT         KKEXT
     C     *LIKE         DEFINE    PCCO#         ##CO#
     C     *LIKE         DEFINE    PCSUB#        ##SUB#
     C     *LIKE         DEFINE    PCFND         ##FND
     C     *LIKE         DEFINE    PCFYR         ##FYR
     C     *LIKE         DEFINE    PCSUB#        SCSUB#
     C     *LIKE         DEFINE    PCFND         SCFND
     C     *LIKE         DEFINE    PCFYR         SCFYR
     C     *LIKE         DEFINE    PCCASE        SCCASE
     C     *LIKE         DEFINE    PCCO#         XXCO#
     C     *LIKE         DEFINE    PCSUB#        XXSUB#
     C     *LIKE         DEFINE    PCFND         XXFND
     C     *LIKE         DEFINE    PCFYR         XXFYR
     C     *LIKE         DEFINE    PCCASE        XXCASE
     C     *LIKE         DEFINE    PCCERT        XXCERT
     C     *LIKE         DEFINE    PCADDT        XXDT
     C     *LIKE         DEFINE    PCFEIN        XXFEIN
     C     *LIKE         DEFINE    PCMOD         XXMOD
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    DILOST        XXLOS
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
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
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM      'R'           QQOCST            1
     C                   PARM      'Y'           QQCONT            1
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    QQCONT            1
     C                   PARM                    @@MODE            1
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXICD9
     C                   KFLD                    XXLOS
     C*
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C     KEY9          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    PCCERT
     C*** BEGIN ADD *** 08/17/98 **************************************
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*** END   ADD *** 08/17/98 **************************************
     C*
     C**  KEY LIST FOR PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
      *
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  CAN NOT CHANGE INFO IF PRE-CERT IS CLOSED.
     C*
     C     SCSEL         IFEQ      '2 '
     C     SCSTAT        ANDEQ     'C'
     C                   SETON                                        9074
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
     C*
     C                   ENDSR
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14 = DARTS
     C*===============================================================
     C*
     C     KNDO          BEGSR
      *
      * CALL API TO ACCESS DARTS
      *
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      PCSUB#        XASUB#
     C                   MOVE      PCFND         XAFND
     C                   MOVE      PCFYR         XAFYR
     C                   MOVE      PCCASE        XACASE
      *
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@WCIN
     C                   PARM                    @@WCDK
     C                   PARM                    XACO#             3
     C                   PARM                    XASUB#            5
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C                   PARM                    XACASE            7
     C                   PARM      *BLANK        ##KEY            40
      *
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL     PCCERT        KKCERT
     C                   Z-ADD     *LOVAL        KKEXT
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVE      SCPST1        KKEXT
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL(P)  SCCERT        KKCERT
     C                   MOVE(P)   SCEXT         KKEXT
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
      * GET INFO FOR SUBFILE
     C                   MOVEL(P)  PCCERT        SCCERT
     C                   MOVE(P)   PCEXT         SCEXT
     C                   MOVEL(P)  PCSTAT        SCSTAT
      *
      * ZERO SUPPRESS RE-CERT EXTENSION
      *
     C                   MOVEL(P)  PCEXT         XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALEXT             3
      *
      * GET DIAGNOSIS DESC FROM FMDIP
      *
     C                   MOVEL(P)  PCICD1        XXICD9
     C                   MOVEL(P)  PCLOS1        XXLOS
     C     DIKEY         SETGT     PDI01
      *
     C     *IN60         DOUEQ     *ON
     C     DIKEY         READPE    PDI01                                  60
     C     *IN60         IFEQ      *OFF
     C     DISTAT        ANDNE     'I'
     C                   MOVEL(P)  DIDSC1        XXDIAD           29
     C                   LEAVE
     C                   END
     C                   ENDDO
      *
      * SMUPP
     C     PCINTR        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  UPUSNM        XXINTD           10
     C                   END
      *
      * ZERO SUPPRESS RE-CERT DATE
      *
     C                   Z-ADD     PCIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         XXNUM                          LOAD DS
      *
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALRCDT           10
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LOAD SUBFILE - VIEW 1
      *
     C                   SELECT
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     ALEXT         DTA1(Y)
     C                   ADD       5             Y
     C                   MOVEA     PCLIAB        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     PCICD1        DTA1(Y)
     C                   ADD       7             Y
     C                   MOVEA     XXDIAD        DTA1(Y)
     C                   ADD       31            Y
     C                   MOVEA     PCINTR        DTA1(Y)
     C                   ADD       5             Y
     C                   MOVEA     XXINTD        DTA1(Y)
     C                   ADD       12            Y
     C                   MOVEA     ALRCDT        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C                   MOVEA(P)  HDA(1)        SCHDLA
     C                   ENDSL
     C*
     C                   ENDSR
     C*** BEGIN DEL *** 06/25/99 **************************************
     C*================================================================
     C* OPT8 - OPTION 8  LETTERS
     C*================================================================
     C********** OPT8      BEGSR
     C*
     C**********           MOVE PCCO#     QLCO#   3
     C**********           MOVE PCSUB#    QLSUB#  5
     C**********           MOVE PCFND     QLFND   3
     C**********           MOVE SCCERT    QLCERT 15
     C**********           MOVE SCEXT     QLEXT   3
     C**********           CALL 'U0040R'
     C**********           PARM           QQPRDS 40
     C**********           PARM           QLCO#
     C**********           PARM           QLSUB#
     C**********           PARM           QLFND
     C**********           PARM           QLCERT
     C**********           PARM           QLEXT
     C**********           PARM           QQRTCD
     C*
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           END
     C*
     C**********           ENDSR
     C*** END   DEL *** 06/25/99 **************************************
     C*** BEGIN ADD *** 06/25/99 **************************************
     C*================================================================
     C* OPT8 - OPTION 8  LETTERS
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'F4110C'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      SCDBRR        ##RRN             9 0          UMPCP
     C                   PARM      'P'           ##TYPE            1
     C*
     C                   ENDSR
     C*** END   ADD *** 06/25/99 **************************************
     C*================================================================
     C* OPT9  - Option  9  PROCEDURE CODES
     C*================================================================
     C     OPT9          BEGSR
     C*
     C                   MOVE      *BLANK        QQCONT
     C                   MOVEL     'C'           @@MODE
     C     @@CALL        IFEQ      '1'
     C                   MOVEL     'D'           @@MODE
     C                   END
     C                   CALL      'U0061R'      ACDPL1
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 08/19/98 **************************************
     C*================================================================
     C* OPT17 - OPTION 17 Time and Materials
     C*================================================================
     C     OPT17         BEGSR
     C*
     C                   MOVEL     SCCERT        KKCERT
     C                   Z-ADD     SCEXT         KKEXT
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   CALL      'F3040R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN2        ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      SCCERT        ##CERT           15
     C                   PARM      SCEXT         ##EXT             3 0
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *BLANKS       ##EOB             1
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 08/19/98 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKN         CASEQ     *ON           KNDO                     90     DARTS
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C*** BEGIN ADD *** 08/19/98 **************************************
     C     SCSEL         CASEQ     '17'          OPT17
     C*** END   ADD *** 08/19/98 **************************************
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
     C                   MOVE      *ON           *INLR
     C*
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C*** BEGIN ADD *** 08/19/98 **************************************
     C*
     C     @@MED         IFNE      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(15)
     C*
     C                   ENDIF
     C*
     C     @@EDI         IFNE      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(16)
     C                   MOVEL     CLRFKY        OFD(17)
     C*
     C                   ENDIF
     C*** END   ADD *** 08/19/98 **************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET INFO FOR HEADING, LOAD KEY FIELDS FOR UMPCP
      *   SUB,GROUP,YEAR,CASE
      *
     C     @@RRN         CHAIN     PFRCDF                             99
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL     PCCERT        KKCERT
     C                   MOVEL     PCCERT        SCCERT
     C                   MOVEL(P)  PCCERT        QQCERT
     C                   MOVE(P)   PCEXT         QQEXT
      *  ADMIT DATE
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADDT
      *
      *  PROVIDER
     C                   Z-ADD     PCFEIN        SCPPHY
     C                   Z-ADD     PCFEIN        XXFEIN
     C                   Z-ADD     PCMOD         SCPPHM
     C                   Z-ADD     PCMOD         XXMOD
      *
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDNE     'I'
      *
     C                   MOVEL(P)  PDNAME        XXLAST           56
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL(P)  XXLAST        XAPRNM           32
     C                   ELSE
     C                   MOVEL     PDNAME        XAPRNM
     C                   END                                                    END PDFSN
      *
     C                   MOVE      PDAREA        XPAREA            3
     C                   MOVEL     PDPHN#        XPPHN1            3
     C                   MOVE      PDPHN#        XPPHN2            4
      *
      * BUILD STRING FOR PHONE#
      *
     C     XPPHN1        IFNE      *BLANK
     C     '('           CAT(P)    XPAREA:0      XAPPHN           14            AREA CODE
     C                   CAT       ')':0         XAPPHN
     C                   CAT       XPPHN1:1      XAPPHN                         PHONE #
     C                   CAT       '-':0         XAPPHN
     C                   CAT       XPPHN2:0      XAPPHN
     C                   END
      *
     C     XAPRNM        CAT(P)    XAPPHN:2      SCPROV
     C                   END                                                    END *IN60
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
     C                   MOVEA(P)  HDA(1)        SCHDLA
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
     C                   MOVEL     CLRFKY        OFD(21)
     C                   LEAVE
     C                   END
     C                   END
     C                   ENDDO
      *
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY           READE     DBRCDF                                 95
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
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
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVE      SCDBRR        @@RRN
      *
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
    1 *       2         3         4         5        6        7         8
678901*345679801234567890123465798012345678901234567901234678901234567890
** Column headings
         Prim                                                     Re-Cert
** Column headings
Ext Liab Diag   Primary Diag desc              Interviewer         date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYN
1077=Re-Certs               NNN
1088=Letters                YNN
1099=Proc Codes             YYN
11010=Pre-Cert History      NNN
11111=Close Pre-Cert        NNN
11212=                      NNN
11313=                      NNN
11414=DARTS                 NNN
11515=Pre-Cert              NNN
11616=EOMB(s)               NNN
11717=Time and Materials    YYN
11818=EDI                   NNN
11919=EDI Log Hist          NNN
12020=Change Status         NNN
18282=Send Subsq 148        NNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
214F14=DARTS                YYY
215F15=Fold/Truncate        NNN
217F17=Subset               NNN
3
