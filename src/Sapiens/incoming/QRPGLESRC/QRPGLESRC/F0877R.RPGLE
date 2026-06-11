     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H/TITLE TITLE - WORK WITH EDI LOG ERRORS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program display all transaction records for the    **
     H**     selected log record.                                    **
     H**   CALLED FROM:                                              **
     H**     F0875R - Work with EDI Log History                      **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   06/21/99      PROJECT: KY010                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H*  06/09/20  C:212 Project : SY420
     H*                  Mask SSN
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      General Usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      STSTL  (PST01) : System Wide State Code table          **
     H**                       (KEY/ST1A3 = 'Y')                     **
     H**      FEE1L1 (DBRCDF): Error file                            **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#/LSEQ/LSQ#)**
     H**      FELGP  (PFRCDF): EDI Record                            **
     H**      FMCDL  (PCD01) : Claimant Demographics (SS#)           **
     H**      FMCML1 (PCM01) : Claim File (CO#/FND/FYR/CASE)         **
     H*****************************************************************
     H*
     FF0877F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FSTSTL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFELGP     IF   E             DISK
     F                                     RENAME(PLG01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFEE1L1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PE101:DBRCDF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

      /COPY SCOPYWW01D
     D*
     D DL1             S              1    DIM(75)                              Detail line
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
     D OPTNM           C                   CONST('EDI Log Errors')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0877R01')
     D COMAPI          C                   CONST('FA013R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D ERREC           C                   CONST('Entry Complete')
     D ERREI           C                   CONST('Entry Incomplt')
     D ERREX           C                   CONST('In Extract fil')
     D ERRSN           C                   CONST('Sent')
     D ERRTA           C                   CONST('Accepted')
     D ERRTE           C                   CONST('Accept error')
     D ERRTR           C                   CONST('Rejected')
     D*
     D* ---------------------------------------------------------------
     D* CASE NUMBER WORK AREA
     D* ---------------------------------------------------------------
     D                 DS
     D  XXFYC                  1     15
     D  XXFND                  1      3
     D  XXD1                   4      4    INZ('-')
     D  XXFYR                  5      7
     D  XXD2                   8      8    INZ('-')
     D  XXCASE                 9     15

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    ##RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*          FEE1L1 (DBRCDF): Error File
     C*          ------------------------------------------------------
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLSEQ
     C                   KFLD                    KKLSQ#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLSEQ
     C*
     C*          STSTL  (PST01) : System Wide State Code table
     C*          ------------------------------------------------------
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*          WMCDL  (PCD01) : Claimant Demographics (SS#)
     C*          ------------------------------------------------------
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*          WMCML1 (PCM01) : Claim File (CO#/FND/FYR/CASE)
     C*          ------------------------------------------------------
     C     CMKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    E1CO#         KKCO#
     C     *LIKE         DEFINE    E1SUB#        KKSUB#
     C     *LIKE         DEFINE    E1FND         KKFND
     C     *LIKE         DEFINE    E1FYR         KKFYR
     C     *LIKE         DEFINE    E1CASE        KKCASE
     C     *LIKE         DEFINE    E1EOB#        KKEOB#
     C     *LIKE         DEFINE    E1LSEQ        KKLSEQ
     C     *LIKE         DEFINE    E1LSQ#        KKLSQ#
     C     *LIKE         DEFINE    STKEY         KKKEY
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
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SHCO#         KKCO#
     C                   MOVE      SHSUB#        KKSUB#
     C                   MOVE      SHFND         KKFND
     C                   MOVE      SHFYR         KKFYR
     C                   MOVE      SHCASE        KKCASE
     C                   MOVE      SHEOB#        KKEOB#
     C                   MOVE      SHLSEQ        KKLSEQ
     C                   MOVE      SHLSQ#        KKLSQ#
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      E1CO#         SHCO#
     C                   MOVE      E1SUB#        SHSUB#
     C                   MOVE      E1FND         SHFND
     C                   MOVE      E1FYR         SHFYR
     C                   MOVE      E1CASE        SHCASE
     C                   MOVE      E1EOB#        SHEOB#
     C                   MOVE      E1LSEQ        SHLSEQ
     C                   MOVE      E1LSQ#        SHLSQ#
     C                   CLEAR                   DL1
     C                   Z-ADD     1             I
     C                   MOVE      E1DN#         XXDN#             5
     C                   MOVEA     XXDN#         DL1(I)
     C                   Z-ADD     9             I
     C                   MOVEA     E1DND         DL1(I)
     C                   Z-ADD     41            I
     C                   MOVEA     E1MSG#        DL1(I)
     C                   Z-ADD     45            I
     C                   MOVEA     E1MSGD        DL1(I)
     C                   MOVEA     DL1           SCDATA
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
     C     ##RRN         CHAIN     PFRCDF                             60
     C  N60CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      LGFND         XXFND
     C                   MOVE      LGFYR         XXFYR
     C                   MOVE      LGCASE        XXCASE
     C                   MOVEL     XXFYC         SCCASE
     C                   MOVEL(P)  LGJST         SCST
     C                   MOVEL(P)  LGJST         KKKEY
     C     KKKEY         CHAIN     PST01                              60
     C                   MOVEL(P)  STNAME        SCSTD
     C                   MOVEL(P)  LGTSET        SCTSET
     C                   MOVEL     LGSTS         SCSTS
     C                   MOVEL     LGEOB#        SCEOB#
     C     SCEOB#        COMP      0                                      31
     C                   SELECT
     C     SCSTS         WHENEQ    'EC'
     C                   MOVEL(P)  ERREC         SCSTSD
     C     SCSTS         WHENEQ    'EI'
     C                   MOVEL(P)  ERREI         SCSTSD
     C     SCSTS         WHENEQ    'EX'
     C                   MOVEL(P)  ERREX         SCSTSD
     C     SCSTS         WHENEQ    'SN'
     C                   MOVEL(P)  ERRSN         SCSTSD
     C     SCSTS         WHENEQ    'TA'
     C                   MOVEL(P)  ERRTA         SCSTSD
     C     SCSTS         WHENEQ    'TE'
     C                   MOVEL(P)  ERRTE         SCSTSD
     C     SCSTS         WHENEQ    'TR'
     C                   MOVEL(P)  ERRTR         SCSTSD
     C                   ENDSL
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CDLSN         ##LAST           56
     C                   MOVEL     CDFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCNAME
     C                   ELSE                                                   *IN99 = Off
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN99 = Off
     C                   MOVE      LGCO#         KKCO#
     C                   MOVE      LGSUB#        KKSUB#
     C                   MOVE      LGFND         KKFND
     C                   MOVE      LGFYR         KKFYR
     C                   MOVE      LGCASE        KKCASE
     C                   MOVE      LGEOB#        KKEOB#
     C                   MOVE      LGLSEQ        KKLSEQ
     C                   MOVE      *LOVAL        KKLSQ#
     C                   ELSE                                                   *IN60 = Off
     C                   SETON                                        LR
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY2          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY2          READPE    DBRCDF                                 95
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt Element Description                    Err# Description
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                NNN
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
