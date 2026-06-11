     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  06/09/20  C:212 Project : SY420                                *
      *                  Mask SSN                                       *
      * *****************************************************************
     H*
     FF0348F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDCAP     IF   E             DISK
     F                                     RENAME(PCA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFDCALH    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCA01:DBRCDF)
     FFDCAL18   IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCA01:DBRCDF2)
     FFMPRLFL   IF   E           K DISK
     FFMRCL1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFAEOP     IF   E             DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(7) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(7) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Detail - Data Subm C-
     D                                     hgs')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0348R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(6)
     D*
     D*  Date Data Structure used for Century
     D                 DS
     D  XXFDAT                 1      8
     D  XXMTH                  1      2
     D  XXDAY                  3      4
     D  XXYEAR                 5      8
     D*
     D*  VIEW 1 Data Structure
     D                 DS
     D  VIEW1                  1     58
     D  X1FDAT                 1     10
     D  V1CPT4                12     16
     D  V1DSC                 18     58
     D*
     D*  VIEW 2 Data Structure
     D                 DS
     D  VIEW2                  1     58
     D  V2CPT4                 1      5
     D  X2FDAT                 7     16
     D  V2DSC                 18     58

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM                    @@CNTL           20
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
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
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKFDAT
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKCPT4
     C*
     C     RCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EOKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKEOB#
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CACO#         QQCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    CASS#         KKSS#
     C     *LIKE         DEFINE    EODIA1        XXDIA
     C     *LIKE         DEFINE    EOCNTL        QQCNTL
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    SCDATA        XXDATA
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
     C* KBDO - F2=Process
     C*================================================================
     C     KBDO          BEGSR
     C*
     C                   MOVE      SCCNTL        QQCNTL
     C                   MOVEL     SCCONM        XXCONM           40
     C                   MOVEL     SCUSER        XXUSER           10
     C                   MOVEL     SCEOMB        XXEOB#            9 0
     C                   MOVEL     CACO#         QQCO#
     C*
     C                   CALL      'F0347R'
     C                   PARM                    QQCO#
     C                   PARM                    QQCALL            1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQCNTL
     C                   PARM                    XXCONM           40
     C                   PARM                    XXUSER           10
     C                   PARM                    XXEOB#            9 0
     C*
     C     *INKL         IFEQ      *OFF
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     ##RRN2        CHAIN     PEO01                              90
     C*
     C                   MOVEL     EOCO#         KKCO#
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVEL     EOFYR         KKFYR
     C                   MOVEL     EOCASE        KKCASE
     C                   MOVEL     EOEOB#        KKEOB#
     C*
     C                   Z-ADD     0             KKFDAT
     C                   MOVEL     *BLANKS       KKCPT4
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKFDAT
     C   22              MOVEL     SCPST2        KKCPT4
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCFDAT        KKFDAT
     C                   MOVEL(P)  SCCPT4        KKCPT4
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*******    CAKY      CHAINPCA01                60
     C                   MOVEL     CASS#         KKSS#
     C                   MOVEL     CACPT4        KKCPT4
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** Get Claimant name
     C                   MOVE      *BLANKS       SCCNAM
     C*
     C                   MOVEL(P)  CDLSN         ##LAST           56
     C                   MOVEL(P)  CDFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCCNAM
     C*
     C                   ENDIF                                                  *IN99
     C*
     C* CONCAT FOR CASE NUMBER
     C*
     C     XXSUB#        CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXFND:0       SCCASE
     C                   CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXFYR:0       SCCASE
     C                   CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXCASE:0      SCCASE
     C*
     C     ##RRN2        CHAIN     PEO01                              60
     C                   MOVE      EOFEIN        SCFEIN
     C                   MOVE      EOMOD         SCMOD
     C                   MOVE      EOPNAM        SCPNAM
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*                    MOVE EODIA1    SCDIA1
     C*                    MOVE EODIA2    SCDIA2
     C*                    MOVE EODIA3    SCDIA3
     C*                    MOVE EODIA4    SCDIA4
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CASS#         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CASS#':CASS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   CLEAR                   VIEW1
     C                   CLEAR                   VIEW2
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDAT
     C                   Z-ADD     CAFDAT        SCFDAT
     C                   MOVE      CACPT4        SCCPT4
     C*
     C     CARCFL        IFEQ      'Y'
     C     RCKY          CHAIN     PRC01                              60
     C                   MOVEL     RCDSC1        V1DSC
     C                   MOVEL     RCDSC1        V2DSC
     C                   ELSE
     C     PRKY          CHAIN     PPR01                              60
     C                   MOVEL     PRDSC1        V1DSC
     C                   MOVEL     PRDSC1        V2DSC
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    '1'
     C                   MOVEL     CACPT4        V1CPT4
     C     XXMTH         CAT       '/':0         X1FDAT
     C                   CAT       XXDAY:0       X1FDAT
     C                   CAT       '/':0         X1FDAT
     C                   CAT       XXYEAR:0      X1FDAT
     C                   MOVEL     VIEW1         SCDATA
     C*
     C     *IN22         WHENEQ    '1'
     C                   MOVEL     CACPT4        V2CPT4
     C     XXMTH         CAT       '/':0         X2FDAT
     C                   CAT       XXDAY:0       X2FDAT
     C                   CAT       '/':0         X2FDAT
     C                   CAT       XXYEAR:0      X2FDAT
     C                   MOVEL     VIEW2         SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKB         CASEQ     *ON           KBDO
     C                   ENDCS
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
     C                   MOVEL(P)  EOSUB#        XXSUB#            5
     C                   MOVEL(P)  EOFND         XXFND             3
     C                   MOVEL(P)  EOFYR         XXFYR             3
     C                   MOVEL(P)  EOCASE        XXCASE            7
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
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
Opt  Service    Code  Description
Opt  Code  Service    Description
** Description--------------MISN
1022=Change                 YNN
1055=Display                YYY
202F2=Process               YYY
203F3=Exit                  YYN
211F11=Change view          YNN
212F12=Previous             YYY
3
