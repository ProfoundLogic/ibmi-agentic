     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*
     H*****************************************************************
     H*  F0356R - WORK WITH EOB CODES/NOTES             C:62 06/23/98 *
     H*                                                               *
     H*  CALLED FROM: F0390R                                          *
     H*  CALLS TO:    F0355R01                                        *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  08/02/22 C:333 Support : 103056
     H*                 - Work With Bill Detail EOB Codes/Notes Not
     H*                   Paging Properly
     H*
     H*  06/09/20 C:212 Project : SY420
     H*                 Mask SSN
     H*                 ReCompile only
     H*
     H*  11/15/12 C:287 Project: MC367
     H*                 Disable function code F14 for GS
     H*
     H*  05/31/02 C:062 PROJECT: 114MDC
     H*                 -CHANGE SO EOBS/NOTES AREN'T HARD DELETED
     H*                 -PRE-SUBSET TO ACTIVE RECORDS
     H*
     H*  01/07/02 C:062 PROJECT: FS235F
     H*                 ADD PRINT FIELD TO SUBFILE.
     H*
     H*  02/08/00 C:31  Project: FSMNT
     H*                 FSMS Maintenance
     H*                 1. Add "Source" c
     H*
     H*  08/03/98  C:94                                               *
     H*            TOOK OUT SERVICING PROVIDER IN API HEADING         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0356F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDDEP     IF   E             DISK
     F                                     RENAME(PDE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     D*** Beg Chg *** 07/18/23 **************************************
     F****FDDEL1    IF   E           K DISK
     FFDDEL6    IF   E           K DISK
     D*** End Chg *** 07/18/23 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDE01:DBRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFDDEL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PDE01:DBRCDF2)
     FFAEOP     IF   E             DISK
     FFDCAP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Detail EOB Co-
     D                                     des/Notes')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0356R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D*** BEGIN ADD *** 05/31/02 **************************************
     D DELETD          C                   CONST('*Deleted')

     I*** END   ADD *** 05/31/02 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      @@RRN3        ##RRN3            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       #1RTCD            1
     C                   PARM      ##WCIN        #1WCIN            1
     C                   PARM      ##WCDK        #1WDCK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       ##KEY            40
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
     C                   KFLD                    KKDSEQ
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKEOB
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKNOT1
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DECO#         KKCO#
     C     *LIKE         DEFINE    DESUB#        KKSUB#
     C     *LIKE         DEFINE    DEFND         KKFND
     C     *LIKE         DEFINE    DEFYR         KKFYR
     C     *LIKE         DEFINE    DECASE        KKCASE
     C     *LIKE         DEFINE    DEEOB#        KKEOB#
     C     *LIKE         DEFINE    DEDSEQ        KKDSEQ
     D*** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         KKEOB
     C     *LIKE         DEFINE    DEEOBCD       KKEOB
     D*** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    DENOT1        KKNOT1
     C     *LIKE         DEFINE    DENOT1        U1PDSC
     C     *LIKE         DEFINE    DENOT1        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSNOT1        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C*
     C                   SETON                                        11
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     SCSEL         IFEQ      '4'
     C     SCRCN         ANDNE     EORCN
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCSEL         IFEQ      '2'
     C     SCRCN         ANDNE     EORCN
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEGIN ADD *** 05/31/02 **************************************
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '4'
     C                   MOVEL     '99'          SCSEL
     C                   ENDIF
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C  N90SSSTAT        IFNE      ' '
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
     C*** BEGIN ADD *** 05/31/02 **************************************
     C                   MOVEL(P)  'A'           SSSTAT
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN3        CHAIN     PCA01                              99
     C*
     C                   Z-ADD     CACO#         KKCO#
     C                   Z-ADD     CASUB#        KKSUB#
     C                   Z-ADD     CAFND         KKFND
     C                   Z-ADD     CAFYR         KKFYR
     C                   Z-ADD     CACASE        KKCASE
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   Z-ADD     CALIN#        SCLIN#
     C                   Z-ADD     CADSEQ        SCDSEQ
     C*
     C                   MOVEL     *LOVAL        KKEOB
     C                   MOVEL     *LOVAL        KKNOT1
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKEOB
     C                   MOVEL(P)  SCPST2        KKNOT1
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCEOB         KKEOB
     C                   MOVEL(P)  SCNOT1        KKNOT1
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*
     C                   CALL      'FA703R'      DRTSPL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** Beg Chg *** 07/18/23 **************************************
     C*****              MOVEL(P)  DEEOB         SCEOB
     C                   MOVEL(P)  DEEOBCD       SCEOB
     C*** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  DENOT1        SCNOT1
     C                   MOVEL(P)  DENOT2        SCNOT2
     C                   MOVEL     DERCN         SCRCN
     C*
     C*** Beg Chg *** 07/18/23 **************************************
     C***21              MOVEL(P)  DEEOB         SCDATA
     C   21              MOVEL(P)  DEEOBCD       SCDATA
     C*** End Chg *** 07/18/23 **************************************
     C*** BEGIN DEL *** 01/07/02 **************************************
     C***21*****           MOVE DENOT1    SCDATA
     C*** END   DEL *** 01/07/02 **************************************
     C   21              MOVE(P)   DENOT2        SCDAT2
     C*** BEGIN CHG *** 01/07/02 **************************************
     C*** BEGIN ADD *** 02/08/00 **************************************
     C***21*****           MOVE '    '    SCDATA
     C***21*****           MOVELDEESRC    XXWK3   3 P
     C***21*****           MOVE XXWK3     SCDATA
     C*** END ADD *** 02/08/00 ****************************************
     C*** Beg Chg *** 07/18/23 **************************************
     C***21              MOVEL     DENOT1        XXNOT1           55
     C   21              MOVEL     DENOT1        XXNOT1           52
     C*** End Chg *** 07/18/23 **************************************
     C   21              MOVE      XXNOT1        SCDATA
     C                   MOVEL(P)  DEESRC        SCSRC
     C                   MOVEL(P)  DEPRNT        SCPRNT
     C*** END   CHG *** 01/07/02 **************************************
     C*
     C*** BEGIN CHG *** 01/07/02 **************************************
     C***22*****           MOVELDENOT1    SCDATA    P
     C   22              MOVEL     DENOT1        XXNOT1
     C   22              MOVEL(P)  XXNOT1        SCDATA
     C*** END   CHG *** 01/07/02 **************************************
     C*** BEGIN CHG *** 02/08/00 **************************************
     C***22***             MOVE DEEOB     SCDATA
     C*** BEGIN CHG *** 01/07/02 **************************************
     C***22*****           MOVE *BLANKS   XXWK12 12
     C***22*****           MOVE XXWK12    SCDATA
     C***22*****           MOVELDEEOB     XXWK11 11 P
     C***22*****           MOVE XXWK11    SCDATA
     C***22*****           MOVELDEESRC    XXWK3   3 P
     C***22*****           MOVE XXWK3     SCDATA
     C*** Beg Chg *** 07/18/23 **************************************
     C***22              MOVE      DEEOB         SCDATA
     C   22              MOVE      DEEOBCD       SCDATA
     C*** End Chg *** 07/18/23 **************************************
     C*** END   CHG *** 01/07/02 **************************************
     C*** END CHG *** 02/08/00 ****************************************
     C   22              MOVEL(P)  DENOT2        SCDAT2
     C*** BEGIN ADD *** 05/31/02 **************************************
     C     XXSEL         IFEQ      '99'
     C                   EXSR      SF1UPD
     C                   CLEAR                   XXSEL
     C                   ENDIF
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/31/02 **************************************
     C*================================================================
     C* OPT99 - Soft Deletion option
     C*================================================================
     C     OPT99         BEGSR
     C*
     C                   Z-ADD     SFRRN1        SCSF1R
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   MOVEL(P)  SCSEL         XXSEL             2
     C*
     C                   ENDSR
     C*** END   ADD *** 05/31/02 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*** BEGIN ADD *** 05/31/02 **************************************
     C     SCSEL         CASEQ     '99'          OPT99
     C                   ENDCS
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETON                                        12
     C*
     C*** BEG CHG *** 08/02/22 ******************************************
     C***                Z-ADD     7             SF1PAG            5 0
     C                   Z-ADD     6             SF1PAG            5 0
     C*** END CHG *** 08/02/22 ******************************************
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANKS       ##L1             78
     C     SCL2HD        PARM      *BLANKS       ##L2             78
     C*
     C                   CALL      'FA707R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C     SCL3HD        PARM      *BLANKS       ##L1             78
     C*********  SCL4HD    PARM *BLANKS   ##L2   78        DEL 08/03/98
     C*
     C                   MOVEL(P)  EONTWK        SCNTWK
     C                   MOVE      EOEOB#        SCEOB#
     C*
     C*** Beg Add *** MC367 *** 11/15/12 *****************************
      /Free
      /If Not Defined(WEB)
       //Disable function F14
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '14';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/15/12 *****************************
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
     C*** BEGIN ADD *** 05/31/02 **************************************
     C                   MOVEL(P)  DELETD        SCDATA
     C                   MOVE      *BLANKS       SCDAT2
     C*** END   ADD *** 05/31/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
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
     C                   PARM      45            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      45            PATLEN            3 0
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
     C     SSNOT1        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  DENOT1        U1PDSC
     C     LO:HI         XLATE(P)  SSNOT1        U2PDSC
     C*
     C                   EXSR      STRING
     C*
     C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     DESTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt EOB     Note                                                 Source Print
Opt Note                                                  EOB    Source Print
**  Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
214F14=DARTS                YYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
