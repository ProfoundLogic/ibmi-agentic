     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H*  F8047R - Work with Upload Bills with Errors                  *
     H*                                                               *
     H*  Called from: Menu                                            *
     H*  Calls to   : F8047R01 - Upload Bill Information              *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/20/2004 by c:62 Project 51MFN                  *
     H*****************************************************************
     H*  Revisions:
     H*
     H*  06/09/20  C:212 Project : SY420
     H*                  Mask SSN
     H*
     H*  05/11/20  C:212  Project: MC437
     H*                   Redesign per CS standards
     H*                   - Call to new CS standard program and return
     H*
     H*  11/14/14 C:079 Project: FSMNT
     H*                 - Change option 7 name
     H*
     H*  12/19/13 C:274 SUPPORT 83617
     H*                 - Add Control Number (U1TRID) to key for FAU1L5
     H*
     H*  08/26/13 C:079 MC133 83092
     H*                 - Soft delete FAU2P records
     H*
     H*  08/05/13 c:280 seq83067 - fix bug with function keys
     H*
     H*  02/21/12 c:280 133MMC ADD NEW PROCESS FOR
     H*                 - Add work with uploads by date process
     H*
     H*  04/20/10 c:198 Support 72046
     H*                 - Changed the text of transfer to PDS.
     H*
     H*  02/01/08 C:180 SUPPORT 61964
     H*                 When payment file contained multiple records
     H*                 with the same claim#, the claim # view
     H*                 would show only 12 records even though
     H*                 there were more than 12 records of the same
     H*                 claim #. Fixed so all records are shown.
     F*
     H*  06/30/06 C:62 PROJECT 132MFO                                 *
     H*                -EXPAND DUPLICATE COLUMN                       *
     H*                                                               *
     H*  08/15/05 C:62 PROJECT 126MCT                                 *
     H*                -ADD DUPLICATE COLUMN                          *
     H*                -ADD DUPLICATE TO SUBSET                       *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FF8047F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAU1P     IF   E             DISK
     F                                     RENAME(PU101:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFAU1L3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PU101:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
      ****** BEGIN CHANGE *******  02/01/08 ***************************
     F****FAU1L4  IF  E           K        DISK
     FFAU1L4B   IF   E           K DISK
      ****** END  CHANGE ********  02/01/08 ***************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PU101:DBRCDF2)
     F*
     F*  Tertiary logical file definition (for displaying items)
     FFAU1L5    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PU101:DBRCDF3)
     F*
     F*** BEGIN ADD *** 02/21/12 **************************************
     F*
     FFAU1L10   IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PU101:DBRCDF4)
     F*
     F*** END   ADD *** 02/21/12 **************************************
     F*
     F*** BEGIN ADD *** 08/15/05 **************************************
     FNMNWL     IF   E           K DISK
     FNMNWP     IF   E             DISK
     F                                     RENAME(PNW01:PNW02)
     F*** END   ADD *** 08/15/05 **************************************
     F*** BEG ADD *** 08/26/13 **************************************
     F*
     FFAU2L1    UF   E           K DISK
     F*** END ADD *** 08/26/13 **************************************

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D***** Beg Add *** 05/11/20 **************************************
     D PBUplErr        pr                  EXTPGM('MTPBULERR')
     D ppCall                         1
     D ppRrn                          9
     D ppNtwk                         5
     D ppMode                         1
     D ppTabInd                       5
     D***** End Add *** 05/11/20 **************************************
      /COPY SCOPYWW01D
     D*
     D******* BEGIN CHG ********** 02/21/12 ***************************
     D*
     D***                 HD      1   3 79               Column headings
     D***                 OF      1  13  1   OFD    35   Options, Function
     D*
     D HD              S             79    DIM(4) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D*
     D******* END   CHG ********** 02/21/12 ***************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*
     D******* BEGIN ADD ********** 02/21/12 ***************************
     D*
     D                 DS
     D  DATA                   1     70
     D  XXMTH                  1      2
     D  SLS1                   3      3
     D  XXDAY                  4      5
     D  SLS2                   6      6
     D  XXYR                   7     10
     D  XXBIL#                15     29
     D  XXNAM                 33     58
     D  XXERR                 63     63
     D  XXEST                 70     70
     D                 DS
     D  XXULDT                 1      8
     D  XMTH                   1      2
     D  XDAY                   3      4
     D  XYR                    5      8
     D*
     D******* END   ADD ********** 02/21/12 ***************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Upload Bills')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F8047R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D CHGNUM          C                   CONST('SA020R')
     D*
     D******* BEGIN ADD ********** 02/21/12 ***************************
     D*
     D ERRMNT          C                   CONST('F2550R')
     D*
     D******* END   ADD ********** 02/21/12 ***************************
     D*
     D INFOMX          C                   CONST(1)
     D*
     D*** BEG CHG ***** 02/21/12 **************************************
     D*
     D*             3                     C         VIEWMX
     D SLASH           C                   CONST('/')
     D*
     D*** END CHG ***** 02/21/12 **************************************
     D*
     D SF1PAG          C                   CONST(12)
     D DELETE          C                   CONST('*** DELETED ***-
     D                                      DELETED *** D-
     D                                     ELETED ***')
     D TRNSFR          C                   CONST('**TRANSFERRED*T-
     D                                     RANSFERRED*TRA-
     D                                     NSFERRED**')

     D***** Beg Add *** 05/11/20 **************************************
     d ppCall          s              1    inz
     d ppRRN           s              9    inz
     d ppNtwk          s              5    inz
     d ppMode          s              1    inz
     d ppTabInd        s              5    inz
     D***** Beg Add *** 05/11/20 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C*** BEGIN ADD *** 08/15/05 **************************************
     C                   PARM                    @@NTWK            5
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CALL            1
     C*** BEGIN ADD *** 08/15/05 **************************************
     C                   PARM      *BLANKS       ##RTCD            1
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C*  Change numeric field to character field.
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
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
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKTRID
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCLM#
      **** BEGIN ADD ************** 02/01/08 **************************
     C                   KFLD                    KKTRID
      **** END   ADD ************** 02/01/08 **************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCLSN
     C                   KFLD                    KKCFSN
     C*** Begin Add *** 12/19/13 **************************************
     C                   KFLD                    KKTRID
     C*** End   Add *** 12/19/13 **************************************
     C*** BEGIN ADD *** 08/15/05 **************************************
     C*
     C     NWKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNWCD
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKULDT
     C                   KFLD                    KKULTM
     C                   KFLD                    KKTRID
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*** BEG ADD *** 08/26/13 **************************************
     C     U2KY          KLIST
     C                   KFLD                    K1TRID
     C*** END ADD *** 08/26/13 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    U1TRID        KKTRID
     C     *LIKE         DEFINE    U1CLM#        KKCLM#
     C     *LIKE         DEFINE    U1CLSN        KKCLSN
     C     *LIKE         DEFINE    U1CFSN        KKCFSN
     C     *LIKE         DEFINE    U1CLSN        U1PDSC
     C     *LIKE         DEFINE    U1CLSN        U2PDSC
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C     *LIKE         DEFINE    U1ULDT        KKULDT
     C     *LIKE         DEFINE    U1ULTM        KKULTM
     C     *LIKE         DEFINE    U1TRID        ##TRID
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*** BEG ADD *** 08/26/13 **************************************
     C     *LIKE         DEFINE    U1TRID        K1TRID
     C*** END ADD *** 08/26/13 **************************************
     C*
     C***** Beg Add *** 05/11/20 **************************************
     C/free
        If q1prms >= 1;
           ppCall = @@call;
        Else;
           ppCall = ' ';
        EndIf;
        If q1prms >= 4;
           ppRrn = %EditC(@@rrn:'X');
        Else;
           ppRrn = '000000000';
        EndIf;
        If q1prms >= 5;
           ppNtwk = @@ntwk;
        Else;
           ppNtwk = *blanks;
        EndIf;
        ppMode = 'C';
        ppTabInd = '0';
        Callp PBUplErr(ppCall:ppRrn:ppNtwk:ppMode:ppTabInd);
        *inlr = *on;
        Return;
      /end-free
     C***** End Add *** 05/11/20 **************************************
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       ssSS# = UnMaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C     SSCLSN        IFNE      *BLANKS
     C     SSSS#         ORNE      *BLANKS
     C     SSPRFL        ORNE      *BLANKS
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     SSDERR        ORNE      *BLANKS
     C*** END   ADD *** 08/15/05 **************************************
     C*** BEG ADD *** 02/21/12 ****************************************
     C     SSULDT        ORNE      *ZEROS
     C     SSSTAT        ORNE      *BLANKS
     C*** END ADD *** 02/21/12 ****************************************
     C                   SETON                                        11
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '6'
     C     SCERR         ANDEQ     'R'
     C                   SETON                                        9073
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         IFEQ      '4'
     C     SCPRFL        IFEQ      'E'
     C     SCPRFL        OREQ      'R'
     C                   MOVEL     '99'          SCSEL
     C                   ELSE
     C                   SETON                                        9071
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         IFEQ      '6'
     C     SCPRFL        ANDNE     'E'
     C                   SETON                                        9072
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     *INKJ         IFEQ      *ON
     C                   EXSR      KJDO
     C                   ELSE
     C*** END   ADD *** 08/15/05 **************************************
     C     SSPRFL        IFNE      'E'
     C     SSPRFL        ANDNE     'R'
     C     SSPRFL        ANDNE     ' '
     C                   SETON                                        8990
     C                   ENDIF
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     *IN90         IFEQ      *OFF
     C     SSDERR        IFNE      'D'
     C     SSDERR        ANDNE     'P'
     C*** BEGIN ADD *** 06/30/06 **************************************
     C     SSDERR        ANDNE     'S'
     C*** END   ADD *** 06/30/06 **************************************
     C     SSDERR        ANDNE     ' '
     C                   SETON                                        8890
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SSNTWK        ANDNE     *BLANKS
     C                   MOVEL(P)  SSNTWK        KKNWCD
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   SCNWDS
     C                   SETON                                        8790
     C                   ELSE
     C                   MOVEL(P)  NWNAME        SCNWDS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     Q1PRMS        IFGT      4
     C                   MOVEL(P)  @@NTWK        SSNTWK
     C                   MOVEL(P)  SSNTWK        KKNWCD
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  NWNAME        SCNWDS
     C                   ELSE
     C                   CLEAR                   SCNWDS
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVEL(P)  *BLANKS       KKTRID
     C                   MOVEL(P)  *BLANKS       KKCLM#
     C                   MOVEL(P)  *BLANKS       KKCLSN
     C                   MOVEL(P)  *BLANKS       KKCFSN
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C                   Z-ADD     *HIVAL        KKULDT
     C                   Z-ADD     *ZEROS        KKULTM
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL(P)  SCPST1        KKTRID
      ******* BEGIN ADD ********** 02/01/08 ***************************
     C   22              MOVEL(P)  SCPST1        KKTRID
      ******* END ADD ************ 02/01/08 ***************************
     C   22              MOVEL(P)  SCPST2        KKCLM#
     C   23              MOVEL(P)  SCPST3        KKCLSN
     C   23              MOVEL(P)  SCPSTF        KKCFSN
     C*
     C******* BEGIN ADD ********** 02/21/12 ***************************
     C*
     C     *IN24         IFEQ      *ON
     C*
     C* Convert date
     C*
     C                   MOVE      SCPST4        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKULDT
     C                   Z-ADD     *HIVAL        KKULTM
     C*
     C                   ENDIF
     C*
     C******* END   ADD ********** 02/21/12 ***************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCTRID        KKTRID
     C                   MOVEL(P)  SCCLM#        KKCLM#
     C                   MOVEL(P)  SCCLSN        KKCLSN
     C                   MOVEL(P)  SCCFSN        KKCFSN
     C*
     C*** BEG ADD *** 02/21/12 ****************************************
     C*
     C                   Z-ADD     SCULDT        KKULDT
     C                   Z-ADD     SCULTM        KKULTM
     C*
     C*** END ADD *** 02/21/12 ****************************************
     C*
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 08/15/05 **************************************
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SSNTWK'
     C*
     C                   CALL      'N2325R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C     ##RRN         CHAIN     PNW02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     NWNWCD        SSNTWK
     C                   MOVEL     NWNAME        SCNWDS
     C                   ENDIF                                                  *IN99 = Off
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 08/15/05 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
B01  C     UPDLIN        IFEQ      *OFF
     C*
     C                   MOVEL(P)  U1TRID        SCTRID
     C                   MOVEL(P)  U1CLM#        SCCLM#
     C                   MOVEL(P)  U1CLSN        SCCLSN
     C                   MOVEL(P)  U1CFSN        SCCFSN
     C                   MOVEL(P)  U1PRFL        SCPRFL
     C*
     C                   CALL      'SRNAME'
     C                   PARM      U1CLSN        ##LAST           56
     C                   PARM      U1CFSN        ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL     ##LAST        XXNAME           33
     C*
     C   21              MOVEL(P)  U1CLM#        XXDATA           54
     C   21              MOVE      XXNAME        XXDATA
     C   21              MOVEL(P)  U1TRID        SCDATA
     C   21              MOVE      XXDATA        SCDATA
     C*
     C   22              MOVEL(P)  U1CLM#        XXDATA
     C   22              MOVE      XXNAME        XXDATA
     C   22              MOVEL(P)  XXDATA        SCDATA
     C   22              MOVE      U1TRID        SCDATA
     C*
     C   23              MOVEL(P)  XXNAME        XXDATA
     C   23              MOVE      U1CLM#        XXDATA
     C   23              MOVEL(P)  XXDATA        SCDATA
     C   23              MOVE      U1TRID        SCDATA
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
B02  C     *IN24         IFEQ      *ON
     C*
     C                   MOVEL(P)  XXNAME        XXDATA
     C                   MOVE      U1CLM#        XXDATA
     C*
     C* Convert date
     C*
     C                   Z-ADD     U1ULDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXULDT
     C*
     C                   MOVEL(P)  U1TRID        SCTRID
     C*
B03  C     XXULDT        IFNE      *BLANKS
     C*
     C                   Z-ADD     U1ULDT        SCULDT
     C                   Z-ADD     U1ULTM        SCULTM
     C*
     C                   MOVE      XMTH          XXMTH
     C                   MOVE      SLASH         SLS1
     C                   MOVE      XDAY          XXDAY
     C                   MOVE      SLASH         SLS2
     C                   MOVE      XYR           XXYR
     C*
X03  C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        SCULDT
     C                   Z-ADD     *ZEROS        SCULTM
     C*
     C                   MOVE      *BLANKS       XXMTH
     C                   MOVE      *BLANKS       SLS1
     C                   MOVE      *BLANKS       XXDAY
     C                   MOVE      *BLANKS       SLS2
     C                   MOVE      *BLANKS       XXYR
     C*
E03  C                   ENDIF
     C*
     C                   MOVEL     U1TRID        XXBIL#
     C                   MOVEL     XXNAME        XXNAM
     C                   MOVE      U1PRFL        XXERR
     C                   MOVE      U1ESTS        XXEST
     C*
     C                   MOVE      *BLANKS       SCDATA
     C                   MOVEL     DATA          SCDATA
     C*
X02  C                   ELSE
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*
     C                   MOVEL     U1PRFL        SCERR
     C*
     C*** BEGIN ADD *** 08/15/05 **************************************
     C*
     C                   MOVE      U1DLRE        SCDERR
     C*
     C*** END   ADD *** 08/15/05 **************************************
     C*
E02  C                   ENDIF
     C*
X01  C                   ELSE
     C                   MOVEL     *OFF          UPDLIN
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - Transfer to PDS
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   MOVEL     *OFF          UPDLIN
     C                   MOVE      'C'           ##MODE
     C                   MOVE      ' '           ##CALL
     C                   CALL      ACDPGM        ACDPL                  99
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C*
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     ##RTCD        IFNE      *BLANKS
     C                   MOVEL     ##RTCD        SCDERR
     C                   ELSE
     C*** END   ADD *** 08/15/05 **************************************
     C                   MOVEL(P)  TRNSFR        SCDATA
     C                   MOVEL     'Y'           SCPRFL
     C                   MOVEL     *ON           UPDLIN
     C*** BEGIN ADD *** 08/15/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C                   ENDIF
     C                   MOVEL     *BLANKS       ##CALL
     C*
     C                   ENDSR
     C*
     C*** BEG ADD *** 02/21/12 ****************************************
     C*
     C*================================================================
     C* OPT7 - Work with errors
     C*================================================================
     C*
     C     OPT7          BEGSR
     C*
     C     CLTID         IFEQ      '133'
     C                   MOVEL     *OFF          UPDLIN
     C                   MOVE      'C'           ##MODE
     C                   MOVE      ' '           ##CALL
     C                   CALL      ERRMNT        ACDPL                  99
     C*
     C                   MOVEL     *ON           UPDLIN
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*** END ADD *** 02/21/12 ****************************************
     C*
     C*================================================================
     C* OPT99 - Delete Bill
     C*================================================================
     C     OPT99         BEGSR
     C*
     C                   MOVEL     *OFF          UPDLIN
     C                   MOVE      'C'           ##MODE
     C                   MOVE      'D'           ##CALL
     C                   CALL      ACDPGM        ACDPL                  99
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  DELETE        SCDATA
     C                   MOVEL     'Y'           SCPRFL
     C                   MOVEL     *ON           UPDLIN
     C*** BEG ADD *** 08/26/13 ****************************************
     C*
     C*** Soft delete FAU2P records
     C*
     C                   EVAL      K1TRID = SCTRID
     C*
     C     U2KY          CHAIN     PU201                              99
     C*
     C                   DOW       *IN99 = *OFF
     C*
     C                   EVAL      U2PRFL = 'D'
     C                   UPDATE    PU201
     C*
     C     U2KY          READE     PU201                                  99
     C*
     C                   ENDDO
     C*** END ADD *** 08/26/13 ****************************************
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       ##CALL
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
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*
     C     SCSEL         CASEQ     '99'          OPT99
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
     C                   MOVEL     *OFF          UPDLIN            1
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C     CLTID         IFEQ      '133'
     C*
     C                   Z-ADD     4             VIEWMX            1 0
     C                   Z-ADD     4             VIEW#
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 08/05/13 **************************************
     C*
     C*****              Z-ADD     1             VIEWMX
     C*****              Z-ADD     1             VIEW#
     C*
     C                   Z-ADD     3             VIEWMX
     C                   Z-ADD     3             VIEW#
     C*
     C*** END CHG ***** 08/05/13 **************************************
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '07'          XXNMBR
     C*
     C                   EXSR      UPDOFA
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 02/21/12 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C   24              READ      DBRCDF4                                95
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C   23              READP     DBRCDF3                                95
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C   24              READP     DBRCDF4                                95
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C   24KEY4          SETGT     DBRCDF4                            96
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C*
     C*** BEGIN ADD *** 02/21/12 **************************************
     C*
     C   24KEY4          SETLL     DBRCDF4                            96
     C*
     C*** END   ADD *** 02/21/12 **************************************
     C*
     C*
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
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
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
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       ssSS# = UnMaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
B01  C     SSCLSN        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  U1CLSN        U1PDSC
     C     LO:HI         XLATE(P)  SSCLSN        U2PDSC
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSSS#         IFNE      *BLANKS
     C     SSSS#         ANDNE     U1SS#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSPRFL        IFNE      *BLANKS
     C     SSPRFL        IFNE      U1PRFL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 08/15/05 **************************************
     C*
     C     SSDERR        IFNE      *BLANKS
     C                   MOVE(P)   U1DLRE        XXDERR            1
     C     SSDERR        IFNE      XXDERR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSNTWK        IFNE      *BLANKS
     C     SSNTWK        ANDNE     U1NTWK
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C*** BEG ADD *** 02/21/12 *************************************************
     C*
     C* SUBSET BY UPLOAD DATE
     C*
     C     SSULDT        IFNE      *ZEROS
     C*
     C* Convert date
     C                   MOVE      SSULDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKULDT            7 0
     C*
     C     WKULDT        IFNE      U1ULDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* SUBSET BY STATUS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C*
     C     SSSTAT        IFNE      U1ESTS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 02/21/12 *************************************************
     C*
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       ssSS# = MaskData('SSN':'SSSS#':SSSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ENDSR
** Column headings
Opt Bill#           Claim#               Claimant name                    Errs
Opt Claim#               Claimant Name                     Bill#          Errs
Opt Claimant name                     Claim#               Bill#          Errs
Opt Upload Date   Bill#             Claimant Name               Errs   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1066=Transfer to Prov. BillsYNN                                    04/20/10
1077=Error Status Maint     YYY
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   NNN
211F11=Chg View             YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
