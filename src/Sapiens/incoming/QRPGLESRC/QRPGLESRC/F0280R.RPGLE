     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0280R - WW Outcome/Savings
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY:                                                   *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*     05/04/98 C:61 PROJECT 98MAC                               *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     F*                                                               *
     F*  05/29/02 C:62  Project: MC099 32722                          *
     F*                 -CHANGED SF1PAG TO 6                          *
     H*                                                               *
     H*  01/26/01 C:62  PROJECT: MCMNT  BASE ONLY                     *
     H*                 COMPILE ONLY                                  *
     H*                                                               *
     H*  09/28/99 C:31  Support: 19799                                *
     H*                 No data displayed in F0280R01                 *
     H*                 1. Change VLDOPT process                      *
     H*                                                               *
     H*  04/21/99 C:31  Project: 98MBR                                *
     H*                 Outcome/Savings: Opt. 20                      *
     H*                 1. Add option 17=Subset                       *
     H*                 2. Add option 20=Change Status                *
     H*                 3. Add column for status                      *
     H*                                                               *
     H*  03/30/99 C:33 PROJECT: WC577                                 *
     H*          C:118 RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*                                                               *
     H*  02/23/99 C:62 PROJECT MC098 16780                            *
     H*                GET THE CORRECT SEQ#                           *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  16/N16   FAMSP RRN/UMPCP RRN                                 *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
      *
     FF0280F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FFDOSP     IF   E             DISK
     F                                     RENAME(POS01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FFDOSL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(POS01:DBRCDF)
      *
      *  Secondary logical file definition (for displaying items)
     FFDOSL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(POS01:DBRCDF2)
      *
      *  Outcome/Savings File
     FFDOSL2    IF   E           K DISK
     F                                     RENAME(POS01:POS02)
     F*** BEGIN ADD *** 02/23/99 **************************************
      *
      *  Primary logical file definition (for displaying items)
     FFDOSL3    IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F                                     RENAME(POS01:POS03)
      *
      *  Secondary logical file definition (for displaying items)
     FFDOSL4    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     F                                     RENAME(POS01:POS04)
      *
      *  Outcome/Savings File
     FFDOSL5    IF   E           K DISK
     F                                     RENAME(POS01:POS05)
     F*** END   ADD *** 02/23/99 **************************************
      *
      *  Medical Services
     FFAMSP     IF   E             DISK
      *
      *  Pre Cert Master
     FUMPCP     IF   E             DISK
      *
      *  Medical Service Code Table File
     FFTMCL     IF   E           K DISK
      *
      *  Outcome/Savings Table File
     FFTOTL     IF   E           K DISK
      *
      *
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D*** BEGIN CHG *** 04/21/99 **************************************
     D********            OF      1  12  1   OFD    35   Options, Function
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*** END CHG *** 04/21/99 ****************************************
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
     D*** BEGIN ADD *** 02/23/99 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D*** END   ADD *** 02/23/99 **************************************
      *
      *
     D                 DS
     D  DRKY                   1     40
     D  DRCO#                  1      3  0
     D  DRSUB#                 4      8  0
     D  DRFND                  9     11  0
     D  DRFYR                 12     14  0
     D  DRCASE                15     21  0
     D  DRMSCD                22     25
     D  DRSEQ#                26     30  0
      *
      *
     D                 DS
     D  DPKY                   1     40
     D  DPCO#                  1      3  0
     D  DPSUB#                 4      8  0
     D  DPFND                  9     11  0
     D  DPFYR                 12     14  0
     D  DPCASE                15     21  0
     D  DPCERT                22     36
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Outcome/Savings')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0280R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D*** BEGIN ADD *** 02/23/99 **************************************
     D CHGNUM          C                   CONST('SA020R')
     D*** END   ADD *** 02/23/99 **************************************
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D*** BEGIN CHG *** 05/29/02 **************************************
     D**********    7                     C         SF1PAG
     D SF1PAG          C                   CONST(6)
     D*** END   CHG *** 05/29/02 **************************************
     D HDMS            C                   CONST('Svc Code:')
     D HDPC            C                   CONST('Pre-Cert#:')
      *
      * Program specific constants
      *
      *================================================================
      *  PLIST definitions
      *================================================================
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FDOSP
     C                   PARM                    @@RRN1            9 0          FAMSP
     C                   PARM                    @@RRN2            9 0          UMPCP
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          FDOSP
     C                   PARM      @@RRN1        ##RRN1            9 0          FAMSP
     C                   PARM      @@RRN2        ##RRN2            9 0          UMPCP
     C*** BEGIN ADD *** 02/23/99 **************************************
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*** END   ADD *** 02/23/99 **************************************
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
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 02/23/99 **************************************
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 02/23/99 **************************************
     C                   KFLD                    KKDATE
     C                   KFLD                    KKCODE
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 02/23/99 **************************************
     C                   KFLD                    KKCODE
     C                   KFLD                    KKDATE
     C*** BEGIN ADD *** 02/23/99 **************************************
      *
     C     KEYB          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
      *
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKDATE
     C                   KFLD                    KKCODE
      *
     C     KEY2B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKCODE
     C                   KFLD                    KKDATE
     C*** END   ADD *** 02/23/99 **************************************
      *
      *  Medical Service Code Table File
      *
     C     MCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
      *
      * Outcome/Savings Table File Key
      *
     C     OTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCODE
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    OSCO#         KKCO#
     C     *LIKE         DEFINE    OSSUB#        KKSUB#
     C     *LIKE         DEFINE    OSFND         KKFND
     C     *LIKE         DEFINE    OSFYR         KKFYR
     C     *LIKE         DEFINE    OSCASE        KKCASE
     C     *LIKE         DEFINE    OSMSCD        KKMSCD
     C     *LIKE         DEFINE    OSSEQ#        KKSEQ#
     C     *LIKE         DEFINE    OSCERT        KKCERT
     C     *LIKE         DEFINE    OSDATE        KKDATE
     C     *LIKE         DEFINE    OSCODE        KKCODE
     C     *LIKE         DEFINE    MCKEY         KKKEY
      *
      *
      *
     C/COPY SCOPYWW01C
      *
     C*** BEGIN ADD *** 02/23/99 **************************************
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C   21              Z-ADD     DBRRNA        SCDBRR
     C   22              Z-ADD     DBRRNB        SCDBRR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 02/23/99 **************************************
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C*** BEGIN ADD *** 04/21/99 **************************************
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  STAT <> blanks
     C*** END ADD *** 04/21/99 ****************************************
     C                   ENDSR
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C***  BEGIN ADD  ***  02/23/99  *********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  02/23/99  *********************************
      *
      * Refresh WW screen after delete
      *
     C     SCSEL         IFEQ      '4'
     C                   MOVE      *ON           AUTORF
     C                   ENDIF
     C*** BEGIN DEL *** 09/28/99 **************************************
     C***** BEGIN ADD **** 03/30/99 ***********************************
     C********             MOVE *ON       VLDOPT
     C***** END ADD ****** 03/30/99 ***********************************
     C*** END DEL *** 09/28/99 ****************************************
     C                   ENDSR
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C*** BEGIN ADD *** 04/21/99 **************************************
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF                                                  STAT <> blank
     C*** END ADD *** 04/21/99 ****************************************
     C                   ENDSR
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C*** BEGIN ADD *** 04/21/99 **************************************
     C                   MOVEL     'A'           SSSTAT
     C*** END ADD *** 04/21/99 ****************************************
     C                   ENDSR
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C*** BEGIN ADD *** 02/23/99 **************************************
      *
      * Get key information
      *
     C                   SELECT
      *
      *    When called from WW Case Management Services F0276R
      *
     C     @@RRN1        WHENNE    *ZERO
     C                   EXSR      SRMSKY
     C                   SETON                                        16
      *
      *    When called from WW Pre-Certifications
      *
     C     @@RRN2        WHENNE    *ZERO
     C                   EXSR      SRPCKY
     C                   SETOFF                                       16
      *
     C                   ENDSL
     C*** END   ADD *** 02/23/99 **************************************
      *
     C                   Z-ADD     *HIVAL        KKDATE
     C                   MOVEL(P)  *LOVAL        KKCODE
      *
     C                   ENDSR
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
      *
      *  RIGHT JUSTIFY
     C     ' '           CHECKR    SCPST1:8      X                 3 0
     C     8             SUB       X             X1                3 0
     C                   MOVEL(P)  *BLANKS       XXPST1            8
     C                   CAT(P)    SCPST1:X1     XXPST1
      *
     C                   MOVE      XXPST1        XXTMP2            8 0
     C                   Z-ADD     XXTMP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE
     C                   MOVEL(P)  *BLANKS       SCPST1
      *
     C                   MOVEL(P)  *LOVAL        KKCODE
      *
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  SCPST2        KKCODE
     C                   Z-ADD     *HIVAL        KKDATE
      *
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVE      SCDATE        XXTMP2            8 0
     C                   Z-ADD     XXTMP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE
      *
     C                   MOVEL(P)  SCCODE        KKCODE
      *
     C                   ENDSR
      *================================================================
      * KNDO - F14=DARTS.
      *================================================================
     C     KNDO          BEGSR
      *
      * ACCESS DARTS
      *
     C                   SELECT
      *
      *    When called from WW Case Management Services F0276R
      *
     C     @@RRN1        WHENNE    *ZERO
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C                   MOVE      KKFYR         #1FYR
     C                   MOVE      KKCASE        #1CASE
     C                   MOVE      KKMSCD        #1MSCD
     C                   MOVE      KKSEQ#        #1SEQ#
     C                   CALL      'FA709R'
     C                   PARM      *BLANK        #1RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1MSCD            4
     C                   PARM                    #1SEQ#            5
     C                   PARM      *BLANKS       #1KEY            40
      *
      *    When called from WW Pre-Certifications
      *
     C     @@RRN2        WHENNE    *ZERO
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C                   MOVE      KKFYR         #1FYR
     C                   MOVE      KKCASE        #1CASE
     C                   MOVE      KKCERT        #1CERT
     C                   CALL      'FA708R'
     C                   PARM      *BLANK        #1RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1CERT           15
     C                   PARM      *BLANKS       #1KEY            40
      *
     C                   ENDSL
      *
      *
     C                   ENDSR
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
     C*** BEGIN ADD *** 02/23/99 **************************************
     C*
     C     VLDOPT        IFEQ      'N'
     C*
     C                   EXSR      ASSRRN
     C***** BEGIN DEL **** 03/30/99 ***********************************
     C**********           ELSE
     C**********
     C**********           MOVE 'N'       VLDOPT
     C***** END DEL ****** 03/30/99 ***********************************
     C                   ENDIF                                                  VLDOPT<>Y
     C*** END   ADD *** 02/23/99 **************************************
      *
     C                   Z-ADD     OSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
      *
     C                   MOVEL(P)  OSCODE        SCCODE
     C                   MOVEL(P)  SCCODE        KKCODE
     C     OTKY          CHAIN     POT01                              60
     C  N60              MOVEL(P)  OTDESC        SCCDDS
     C   60              MOVEL(P)  *BLANKS       SCCDDS
      *
     C                   MOVEL     SRDTO         XXDATE            8
      *
     C                   SELECT
      *
      *  VIEW 1
      *
     C     VIEW#         WHENEQ    1
     C     2             SUBST(P)  XXDATE:1      XXMM              2
     C     2             SUBST(P)  XXDATE:3      XXDD              2
     C*** BEGIN CHG *** 04/21/99 **************************************
     C********   4         SUBSTXXDATE:5  XXYY    4 P
     C     2             SUBST(P)  XXDATE:7      XXYY              2
     C*** END CHG *** 04/21/99 ****************************************
     C                   MOVEL(P)  XXMM          SCDATA
     C                   CAT(P)    '/':0         SCDATA
     C                   CAT(P)    XXDD:0        SCDATA
     C                   CAT(P)    '/':0         SCDATA
     C                   CAT(P)    XXYY:0        SCDATA
      *
     C                   MOVEL(P)  SCCODE        XXTMP3           11
     C*** BEGIN CHG *** 04/21/99 **************************************
     C********   XXTMP3    CAT  SCCDDS    XXTMP5 24 P
     C     XXTMP3        CAT(P)    SCCDDS        XXTMP5           22
     C*** END CHG *** 04/21/99 ****************************************
     C                   MOVE      XXTMP5        SCDATA
      *
      *  VIEW 2
      * ZZZ
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  SCCODE        XXTMP3           11
     C*** BEGIN CHG *** 04/21/99 **************************************
     C********   XXTMP3    CAT  SCCDDS    XXTMP5 24 P
     C     XXTMP3        CAT(P)    SCCDDS        XXTMP5           22
     C*** END CHG *** 04/21/99 ****************************************
     C                   MOVEL(P)  XXTMP5        SCDATA
      *
     C     2             SUBST(P)  XXDATE:1      XXMM              2
     C     2             SUBST(P)  XXDATE:3      XXDD              2
     C*** BEGIN CHG *** 04/21/99 **************************************
     C********   4         SUBSTXXDATE:5  XXYY    4 P
     C     2             SUBST(P)  XXDATE:7      XXYY              2
     C********             MOVELXXMM      XXTMP6 10 P
     C                   MOVEL(P)  XXMM          XXTMP6            8
     C*** END CHG *** 04/21/99 ****************************************
     C                   CAT(P)    '/':0         XXTMP6
     C                   CAT(P)    XXDD:0        XXTMP6
     C                   CAT(P)    '/':0         XXTMP6
     C                   CAT(P)    XXYY:0        XXTMP6
     C                   MOVE      XXTMP6        SCDATA
      *
      *
      *
     C                   OTHER
     C                   MOVEL(P)  *BLANKS       SCDATA
      *
     C                   ENDSL
      *
      *
      *
     C                   Z-ADD     OSNUMB        SCNUMB
     C                   Z-ADD     OSRATE        SCRATE
     C                   Z-ADD     OSSAVE        SCSAVE
     C*** BEGIN ADD *** 04/21/99 **************************************
     C                   MOVE      OSSTAT        SCSTAT
     C*** END ADD *** 04/21/99 ****************************************
      *
      *  Calculate Total Savings
      *
     C                   EXSR      SRTSAV
      *
     C                   ENDSR
     C*** BEGIN ADD *** 04/21/99 **************************************
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   Z-ADD     SCDBRR        ##RRN
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*** END ADD *** 04/21/99 ****************************************
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C                   ENDCS
      *
     C                   ENDSR
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C*** BEGIN ADD *** 04/21/99 **************************************
     C     SCSEL         CASEQ     '20'          OPT20                          Change status
     C                   ENDCS
     C*** END ADD *** 04/21/99 ****************************************
     C                   ENDSR
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C*** BEGIN DEL *** 02/23/99 **************************************
      *
      * Get key information
      *
     C**********           SELEC
      *
      *    When called from WW Case Management Services F0276R
      *
     C********** @@RRN1    WHNE *ZERO
     C**********           EXSR SRMSKY
      *
      *    When called from WW Pre-Certifications
      *
     C********** @@RRN2    WHNE *ZERO
     C**********           EXSR SRPCKY
      *
     C**********           ENDSL
     C*** END   DEL *** 02/23/99 **************************************
      *
      *  Calculate Total Savings
      *
     C                   EXSR      SRTSAV
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   MOVEL     'N'           VLDOPT
     C*** END   ADD *** 02/23/99 **************************************
     C                   ENDSR
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C*** BEGIN CHG *** 09/28/99 **************************************
     C***** BEGIN ADD **** 03/30/99 ***********************************
     C********             MOVE *OFF      VLDOPT
     C                   MOVEL     'N'           VLDOPT
     C***** END ADD ****** 03/30/99 ***********************************
     C*** END CHG *** 09/28/99 ****************************************
     C*
     C*** BEGIN ADD *** 02/23/99 **************************************
     C     *IN16         IFEQ      *ON
     C*** END   ADD *** 02/23/99 **************************************
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   ELSE
     C*
     C   21KEYB          READE     POS03                                  95
     C   22KEYB          READE     POS04                                  95
     C*
     C                   ENDIF
     C*** END   ADD *** 02/23/99 **************************************
     C                   ENDSR
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C*** BEGIN CHG *** 09/28/99 **************************************
     C***** BEGIN ADD **** 03/30/99 ***********************************
     C********             MOVE *OFF      VLDOPT
     C                   MOVEL     'N'           VLDOPT
     C***** END ADD ****** 03/30/99 ***********************************
     C*** END CHG *** 09/28/99 ****************************************
     C*
     C*** BEGIN ADD *** 02/23/99 **************************************
     C     *IN16         IFEQ      *ON
     C*** END   ADD *** 02/23/99 **************************************
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   ELSE
     C*
     C   21KEYB          READPE    POS03                                  95
     C   22KEYB          READPE    POS04                                  95
     C*
     C                   ENDIF
     C*** END   ADD *** 02/23/99 **************************************
     C                   ENDSR
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C*** BEGIN ADD *** 02/23/99 **************************************
     C     *IN16         IFEQ      *ON
     C*** END   ADD *** 02/23/99 **************************************
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   ELSE
     C*
     C   21KEY1B         SETGT     POS03                              96
     C   22KEY2B         SETGT     POS04                              96
     C*
     C                   ENDIF
     C*** END   ADD *** 02/23/99 **************************************
     C                   ENDSR
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C*** BEGIN ADD *** 02/23/99 **************************************
     C     *IN16         IFEQ      *ON
     C*** END   ADD *** 02/23/99 **************************************
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   ELSE
     C*
     C   21KEY1B         SETLL     POS03                              96
     C   22KEY2B         SETLL     POS04                              96
     C*
     C                   ENDIF
     C*** END   ADD *** 02/23/99 **************************************
     C                   ENDSR
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C                   Z-ADD     SCDBRR        @@RRN
      *
     C                   ENDSR
      *
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
     C*** BEGIN ADD *** 04/21/99 **************************************
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     OSSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  STAT <> blanks
     C*** END ADD *** 04/21/99 ****************************************
     C                   ENDSR
      *
      *
      *================================================================
      * SRMSKY - Get key and header information from Medical Services
      *================================================================
     C     SRMSKY        BEGSR
      *
      *  Get Key and Header from Medical Services file (FAMSP)
      *
     C     @@RRN1        CHAIN     PMS01                              60
     C                   Z-ADD     MSCO#         KKCO#
     C                   Z-ADD     MSSUB#        KKSUB#
     C                   Z-ADD     MSFND         KKFND
     C                   Z-ADD     MSFYR         KKFYR
     C                   Z-ADD     MSCASE        KKCASE
     C                   MOVEL(P)  MSMSCD        KKMSCD
     C                   Z-ADD     MSSEQ#        KKSEQ#
      *
      *    Get Header Information
      *
     C                   MOVE      MSCO#         #1CO#             3
     C                   MOVE      MSSUB#        #1SUB#            5
     C                   MOVE      MSFND         #1FND             3
     C                   MOVE      MSFYR         #1FYR             3
     C                   MOVE      MSCASE        #1CASE            7
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
      *
      *  Create 3rd line of Header
      *
     C                   MOVEL(P)  HDMS          SCL3HD
     C                   CAT(P)    MSMSCD:1      SCL3HD
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   MOVEL(P)  MSSEQ#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXSEQ#            5
     C*
     C                   CAT(P)    XXSEQ#:1      SCL3HD
     C*** END   ADD *** 02/23/99 **************************************
     C                   MOVEL(P)  MSMSCD        KKKEY
     C     MCKY          CHAIN     PMC01                              60
     C  N60              CAT(P)    MCDESC:2      SCL3HD
      *
      *
     C                   ENDSR
      *
      *
      *
      *================================================================
      * SRPCKY - Get key and header information from Pre-cert
      *================================================================
     C     SRPCKY        BEGSR
      *
      *  Get Key and Header from Pre-Cert file (UMPCP)
      *
     C     @@RRN2        CHAIN     PPC01                              60
     C                   Z-ADD     PCCO#         KKCO#
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL(P)  PCCERT        KKCERT
      *
      *    Get Header Information
      *
     C                   MOVE      PCCO#         #1CO#             3
     C                   MOVE      PCSUB#        #1SUB#            5
     C                   MOVE      PCFND         #1FND             3
     C                   MOVE      PCFYR         #1FYR             3
     C                   MOVE      PCCASE        #1CASE            7
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
      *
      *  Create 3rd line of Header
      *
     C                   MOVEL(P)  HDPC          SCL3HD
     C                   CAT(P)    PCCERT:1      SCL3HD
     C                   CAT(P)    'Ext.':2      SCL3HD
     C                   MOVE      PCEXT         XXTMP1            3
     C                   CAT(P)    XXTMP1:1      SCL3HD
      *
     C                   ENDSR
      *
      *
      *================================================================
      * SRTSAV - Calculate Total Savings
      *================================================================
     C     SRTSAV        BEGSR
      *
     C                   Z-ADD     *ZERO         SCTSAV
     C*** BEGIN CHG *** 02/23/99 **************************************
     C********** KEY       SETLLPOS02                60
     C********** KEY       READEPOS02                    60
     C   16KEY           CHAIN     POS02                              60
     C  N16KEYB          CHAIN     POS05                              60
     C*** END   CHG *** 02/23/99 **************************************
     C     *IN60         DOWEQ     '0'
      *
     C                   ADD       OSSAVE        SCTSAV
      *
     C*** BEGIN CHG *** 02/23/99 **************************************
     C********** KEY       READEPOS02                    60
     C   16KEY           READE     POS02                                  60
     C  N16KEYB          READE     POS05                                  60
     C*** END   CHG *** 02/23/99 **************************************
     C                   ENDDO                                                  *IN60
      *
     C                   ENDSR
      *
      *
      *
      *
      *
** Column headings
Opt Date     Code       Description Number         rate         amount    Sts
Opt Code       Description Date     Number         rate         amount    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
214F14=DARTS                YNN
217F17=Subset               YYY
3
