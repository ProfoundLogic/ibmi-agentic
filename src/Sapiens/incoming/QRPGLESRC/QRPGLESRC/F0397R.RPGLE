     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0396R - WORK WITH HEADER AMOUNTS
     H*****************************************************************
     H*  Created: 07/13/98  C:62 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
    H*                                                               *
     H*  03/19/24  C:079 Ticket: 99-115253                            *
     H*                  Account for state not installed (FTABLxx)    *
     H*
     H*  11/15/12 C:287 Project: MC367
     H*                 Disabled Function F14 for GS access to Bill Entry
     H*                                                               *
     H*  04/27/01 C:062 PROJECT: FS209                                *
     H*                 ADD F0391R TO DEFAULT VIEW 2                  *
     H*                                                               *
     H*  03/23/01 C:062 PROJECT: FS209                                *
     H*                 CHANGE TO FAEAL1.                             *
     H*                                                               *
     H*  05/12/00 C:079 Support: MC048/#22868                         *
     H*                 1.Load SCFDAT and SCTRDT so paging will work  *
     H*                   correctly.                                  *
     H*                                                               *
     H*  05/11/00 C:079 Support: MC048/#22867                         *
     H*                 1.Use DATRDT instead of DADT to display date  *
     H*                   changed.                                    *
     H*                                                               *
     H*  07/07/99 C:117 Project: MC152                                *
     H*                 Added revisions for Dental Bill Processing    *
     H*                                                               *
     H*  07/01/99 C:56 Project: MC152                                 *
     H*                Change CAFDAT with DADT when referencing Date  *
     H*                of Change                                      *
     H*                                                               *
     H*  03/01/99 C:94 PROJECT: MC152                                 *
     H*             1. CHANGE WHERE CODE AND DESCRIPTION IS BEING     *
     H*                PULLED FROM                                    *
     H*             2. ADD OPTION 5=DISPLAY                           *
     H*                                                               *
     H*****************************************************************
     H*
     FF0397F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*** PHYSICAL FILE DEFINITION ***
     F*
     F*** BEGIN CHG *** 03/23/01 **************************************
     F**********FAEAP   IF  E                    DISK
     FFAEAL1    IF   E           K DISK
     F*** END   CHG *** 03/23/01 **************************************
     F*** BEGIN ADD *** 03/23/01 **************************************
     FFAEAP     IF   E             DISK
     F                                     RENAME(PEA01:PEA02)
     F*** END   ADD *** 03/23/01 **************************************
     F*
     FFAEOP     IF   E             DISK
     F*** BEG ADD *** 03/01/99 **************************************
     F                                     INFDS(DBFDS6)
     F*** END ADD *** 03/01/99 **************************************
     F*
     FFDCAP     IF   E             DISK
     F                                     RENAME(PCA01:PCA03)
     F*** BEG CHG *** 03/01/99 **************************************
     F**********FDCALF  IF  E           K        DISK
     FFDCAL5    IF   E           K DISK
     F                                     INFDS(DBFDS7)
     F*** END CHG *** 03/01/99 **************************************
     F                                     RENAME(PCA01:PCA02)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDDAP     IF   E             DISK
     F                                     RENAME(PDA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEG CHG *** 05/12/00 ****************************************
     F************FDDAL   IF  E           K        DISK
     FFDDAL3    IF   E           K DISK
     F*** END CHG *** 05/12/00 ****************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDA01:DBRCDF)
     FFDDAL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PDA01:DBRCD2)
     FFMCXL     IF   E           K DISK
     F                                     RENAME(PCX01:PCX02)
     FFMPRLFL   IF   E           K DISK
     F*
     F*** BEG ADD *** 03/01/99 ***************************************
     FFDDUL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F*
     FFDDHL     IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F*
     FFDDPL     IF   E           K DISK
     F                                     INFDS(DBFDS5)
     F*** BEG ADD *** 07/07/99 ****************************************
     FFDDDL     IF   E           K DISK
     F                                     INFDS(DBFDS8)
     F*** END ADD *** 07/07/99 ****************************************
     F*
     FFMBEL     IF   E           K DISK
     F*** END ADD *** 03/01/99 ***************************************
     F*
     d*** Beg Add *** 03/19/24 *********************************************
     d strCmd          s            256    Inz
     d inAdjState      s                   Like(EOast)
      /Copy sPrRunCmd
     d*** End Add *** 03/19/24 *********************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D DATA            S             45    DIM(2)
     D DTA1            S              1    DIM(45)
     D DTA2            S              1    DIM(45)
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*** BEG ADD *** 03/01/99 **************************************
     D*  HOSPITAL CODE DATA STRUCTURE
     D                 DS
     D  HOSCOD                 1     11
     D  XXRVCD                 1      3
     D  XXRATE                 4      8
     D  XXUNTS                 9     11
     D*** END ADD *** 03/01/99 **************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Bill Detail-
     D                                      Amounts')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D*** BEG DEL *** 03/01/99 ***************************************
     D**********    'F0397R02'            C         ACDPGM
     D*** END DEL *** 03/01/99 ***************************************
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D*** BEG CHG *** 05/12/00 ****************************************
     D************* 12                    C         SF1PAG
     D SF1PAG          C                   CONST(6)

     I*** END CHG *** 05/12/00 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@1PGM            6
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      SCDBR3        ##RRN             9 0          FDDHP,FDDUP,FDDPP
     C                   PARM      @@RRN         ##RRN2            9 0          FAEOP
     C                   PARM      SCDBR7        ##RRN3            9 0          FDCAP
     C                   PARM      *BLANKS       ##NWCD            5
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
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C* PARM LIST FOR DARTS API
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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEG ADD *** 03/01/99 ***************************************
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C*
     C*** END ADD *** 03/01/99 ***************************************
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN#
     C                   KFLD                    KKTRDT
     C*** BEG ADD *** 05/12/00 ****************************************
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 05/12/00 ****************************************
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN#
     C     KEY2A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKTRDT
     C     KEY2B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEG ADD *** 03/01/99 **************************************
     C                   KFLD                    KKEOB#
     C*** END ADD *** 03/01/99 **************************************
     C                   KFLD                    KKDSEQ
     C*** BEGIN ADD *** 03/23/01 **************************************
     C     EAKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CARCN
     C*** END   ADD *** 03/23/01 **************************************
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCODE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DACO#         KKCO#
     C     *LIKE         DEFINE    DASUB#        KKSUB#
     C     *LIKE         DEFINE    DAFND         KKFND
     C     *LIKE         DEFINE    DAFYR         KKFYR
     C     *LIKE         DEFINE    DACASE        KKCASE
     C     *LIKE         DEFINE    DAEOB#        KKEOB#
     C     *LIKE         DEFINE    DADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DARCN#        KKRCN#
     C     *LIKE         DEFINE    DATRCD        KKTRCD
     C     *LIKE         DEFINE    DATRDT        KKTRDT
     C     *LIKE         DEFINE    #1WCIN        ##WCIN
     C     *LIKE         DEFINE    #1WDCK        ##WCDK
     C*** BEG DEL *** 03/01/99 ***************************************
     C********** *LIKE     DEFN CACPT4    ALCODE
     C*** END DEL *** 03/01/99 ***************************************
     C     *LIKE         DEFINE    CACPT4        KKCODE
     C     *LIKE         DEFINE    CAFDAT        KKEFDT
     C     *LIKE         DEFINE    EOEOB#        HLEOB#
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
     C*** BEG ADD *** 03/01/99 ***************************************
     C     SCSEL         IFEQ      '5'
       //*** Beg Add *** 03/19/24 *************************************
       // If the Adj State is not an installed state (FTABLxx not found)
       // then dont call the override routines and pass blank to FA730R
       inAdjState = EOast;
       strCmd = 'CHKOBJ OBJ(' + 'FTABL' + %TRIM(EOAST) + ') ' +
       'OBJTYPE(*FILE)';
       Monitor;
       RunCmd(%Trim(strCmd):%Len(%Trim(strCmd)));
       On-error;
          inAdjState = ' ';
       Endmon;
       //*** End Add *** 03/19/24 *************************************
     C*
       //*** Beg Chg *** 03/19/24 *************************************
     C***  BEKY          CHAIN     PBE01                              99
     C***  *IN99         IFEQ      *OFF
       Chain (inAdjState:EOsys:EObTyp) pbe01;
       If %Found(FMBEL);
       //*** End Chg *** 03/19/24 *************************************
     C                   MOVEL(P)  BEDEPG        ACDPGM           10
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*
     C                   MOVEL     DADSEQ        KKDSEQ
     C     CAKY          CHAIN     PCA02                              99
     C                   Z-ADD     DBRRN7        SCDBR7            9 0
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'N'
     C     EOSYS         OREQ      'V'
     C     EOBTYP        ANDEQ     'V'
     C     KEY2B         CHAIN     PDH01                              99
     C                   Z-ADD     DBRRN4        SCDBR3            9 0
     C                   ENDIF                                                  EOSYS = 'P'
     C*
     C     EOSYS         IFEQ      'H'
     C     EOBTYP        ANDEQ     'H'
     C     KEY2B         CHAIN     PDU01                              99
     C                   Z-ADD     DBRRN3        SCDBR3
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'X'
     C     KEY2B         CHAIN     PDP01                              99
     C                   Z-ADD     DBRRN5        SCDBR3
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C*** BEG ADD *** 07/07/99 ****************************************
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'D'
     C     KEY2B         CHAIN     PDD01                              99
     C                   Z-ADD     DBRRN8        SCDBR3
     C                   ENDIF                                                  EOSYS = 'H'
     C*** BEG ADD *** 07/07/99 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 03/01/99 ***************************************
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
     C                   MOVE      *LOVAL        KKTRCD
     C                   MOVE      *LOVAL        KKTRDT
     C                   MOVE      *BLANKS       SCHMRA
     C                   MOVE      *BLANKS       SCMRA
     C                   MOVE      *BLANKS       SCHOT1
     C                   MOVE      *BLANKS       SCOTHR
     C                   MOVE      *BLANKS       SCHOT2
     C                   MOVE      *BLANKS       SCOTH2
     C                   MOVE      *BLANKS       SCHNET
     C                   MOVE      *BLANKS       SCNDIS
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*** VIEW 1 ***
     C                   MOVE      SCPST1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCFDAT        KKTRDT
     C                   MOVEL     SCTRCD        KKTRCD
     C*** BEG ADD *** 05/12/00 ****************************************
     C                   MOVE      SCDSEQ        KKDSEQ
     C*** END ADD *** 05/12/00 ****************************************
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   CALL      'FA703R'      DRTSPL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      DACO#         KKCO#
     C                   MOVE      DASUB#        KKSUB#
     C                   MOVE      DAFND         KKFND
     C                   MOVE      DAFYR         KKFYR
     C                   MOVE      DACASE        KKCASE
     C                   MOVE      DAEOB#        KKEOB#
     C                   MOVE      DATRDT        KKTRDT
     C*** BEG ADD *** 05/12/00 **************************************
     C                   Z-ADD     DATRDT        SCFDAT
     C                   MOVEL     DADSEQ        SCDSEQ
     C*** END ADD *** 05/12/00 **************************************
     C*
     C*** BEG DEL *** 03/01/99 **************************************
     C********** *IN21     IFEQ *ON
     C*** END DEL *** 03/01/99 **************************************
     C                   MOVE      DADSEQ        KKDSEQ
     C*
     C*** BEG CHG *** 03/01/99 ***************************************
     C     EOBTYP        IFEQ      'N'
     C     EOBTYP        OREQ      'V'
     C     KEY2B         CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DHPRCD        ALCODE           11
     C                   MOVEL(P)  DHPRDS        SCDESC
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOBTYP        IFEQ      'H'
     C     KEY2B         CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DURVCD        XXRVCD
     C                   MOVEL(P)  DURATE        XXRATE
     C                   MOVE(P)   *BLANKS       XXUNTS
     C                   MOVEL(P)  HOSCOD        ALCODE
     C                   MOVEL(P)  DUDESC        SCDESC
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOBTYP        IFEQ      'X'
     C     KEY2B         CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DPNDC         ALCODE
     C                   MOVEL(P)  DPDESC        SCDESC
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG ADD *** 07/07/99 ****************************************
     C     EOBTYP        IFEQ      'D'
     C     KEY2B         CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DDPRCD        ALCODE
     C                   MOVEL(P)  DDPRDS        SCDESC
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 07/07/99 ****************************************
     C*
     C**********
     C********** CAKY      CHAINPCA02                99
     C**********
     C**********           MOVELKKDSEQ    SCDSEQ
     C**********           Z-ADDCAFDAT    KKEFDT
     C**********           MOVELCACPT4    ALCODE    P
     C**********
     C**********           MOVE ALCODE    KKCODE
     C********** PRKY      SETGTPPR01                95
     C********** PRKY1     REDPEPPR01                    95
     C**********           MOVELPRDSC1    SCDESC    P
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C*
     C     CAKY          CHAIN     PCA02                              99
     C                   Z-ADD     DBRRN7        SCDBR7            9 0
     C*** BEG CHG *** 07/01/99 ****************************************
     C**********           Z-ADDCAFDAT    SRDTI
     C*** BEG CHG *** 05/11/00 ****************************************
     C******************** Z-ADDDADT      SRDTI
     C                   Z-ADD     DATRDT        SRDTI
     C*** END CHG *** 05/11/00 ****************************************
     C*** END CHG *** 07/01/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ALFDAT           10
     C*** END CHG *** 03/01/99 *************************************
     C*
     C                   MOVEL(P)  ALFDAT        QQNUM            80
     C                   MOVEL(P)  'DATE'        QQMASK           10
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         ALFDAT
     C*
     C                   MOVEL     DACHRG        SCCHRG
     C*
     C                   MOVEL     DAPAID        SCPAID
     C                   Z-ADD     DARCN#        SCRCN#
     C*
     C     CXKY          CHAIN     PCX02                              99
     C                   MOVEL(P)  CXHMRA        SCHMRA
     C                   MOVEL(P)  CXHNET        SCHNET
     C                   MOVEL(P)  CXHOT1        SCHOT1
     C                   MOVEL(P)  CXHOT2        SCHOT2
     C*
     C                   MOVEL(P)  DAMRA         QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXMRA            14
     C                   MOVE(P)   XXMRA         SCMRA
     C*
     C                   MOVEL(P)  DAOTHR        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXOTHR           14
     C                   MOVE(P)   XXOTHR        SCOTHR
     C*
     C                   MOVEL(P)  DAOTH2        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXOTH2           14
     C                   MOVE(P)   XXOTH2        SCOTH2
     C*
     C                   MOVEL(P)  DANDIS        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXNDIS           14
     C                   MOVE(P)   XXNDIS        SCNDIS
     C*
     C*
     C                   SELECT
     C*
     C     HD#           WHENEQ    1
     C*
     C                   CLEAR                   SCDATA
     C*
     C*** BEG CHG *** 03/01/99 ************************************
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     ALFDAT        DTA1(Y)
     C                   ADD       12            Y
     C                   MOVEA     ALCODE        DTA1(Y)
     C                   ADD       14            Y
     C                   MOVEA     SCDESC        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C**********
     C**********           Z-ADD1         Y       20
     C**********           MOVEAALFDAT    DTA1,Y
     C**********           ADD  12        Y
     C**********           MOVEAALCODE    DTA1,Y
     C**********           ADD  8         Y
     C**********           MOVEASCDESC    DTA1,Y
     C**********           MOVEADTA1      SCDATA    P
     C*** END CHG *** 03/01/99 ************************************
     C*
     C     HD#           WHENEQ    2
     C*
     C                   CLEAR                   SCDATA
     C*
     C*** BEG CHG *** 03/01/99 ************************************
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     ALFDAT        DTA1(Y)
     C                   ADD       12            Y
     C                   MOVEA     ALCODE        DTA1(Y)
     C                   ADD       14            Y
     C                   MOVEA     SCDESC        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C**********
     C**********           Z-ADD1         Y       20
     C**********           MOVEAALFDAT    DTA1,Y
     C**********           ADD  12        Y
     C**********           MOVEAALCODE    DTA1,Y
     C**********           ADD  8         Y
     C**********           MOVEASCDESC    DTA1,Y
     C**********           MOVEADTA1      SCDATA    P
     C*** END CHG *** 03/01/99 ************************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKN         CASEQ     *ON           KNDO                           F10=search
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
     C     @@1PGM        IFEQ      'F0390R'
     C*** BEGIN ADD *** 04/27/01 **************************************
     C     @@1PGM        OREQ      'F0391R'
     C*** END   ADD *** 04/27/01 **************************************
     C                   Z-ADD     2             VIEW#
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN12
     C*
     C** Get standard screen headings
     C*
     C     @@RRN         CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C                   MOVEL     EOEOB#        HLEOB#
     C                   MOVEL(P)  HLEOB#        QQNUM            80
     C                   MOVEL(P)  *BLANKS       QQMASK           10
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         SCEOB#
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVEL     EOEOB#        KKEOB#
     C*
     C     VIEW#         IFEQ      1
     C                   MOVEL     EONTWK        SCNTW2
     C*** BEGIN DEL *** 03/23/01 **************************************
     C********** @@RRN2    CHAINPEA01                99
     C**********           MOVELEARCN     SCRCN
     C**********           MOVELEARCN     KKRCN#
     C*** END   DEL *** 03/23/01 **************************************
     C*** BEG ADD *** 03/01/99 *************************************
     C     @@RRN2        CHAIN     PCA03                              99
     C*** BEGIN ADD *** 03/23/01 **************************************
     C     @@1PGM        IFEQ      'F0390R'
     C     @@1PGM        OREQ      'F0391R'
     C     EAKY          CHAIN     PEA01                              99
     C                   ELSE
     C     @@RRN2        CHAIN     PEA02                              99
     C                   ENDIF
     C                   MOVE      EACO#         KKCO#
     C                   MOVE      EASUB#        KKSUB#
     C                   MOVE      EAFND         KKFND
     C                   MOVE      EAFYR         KKFYR
     C                   MOVE      EACASE        KKCASE
     C                   MOVE      EAEOB#        KKEOB#
     C                   MOVEL     EARCN         SCRCN
     C                   MOVEL     EARCN         KKRCN#
     C*** END   ADD *** 03/23/01 **************************************
     C*
     C                   MOVEL     CADSEQ        KKDSEQ
     C*
     C*** END ADD *** 03/01/99 *************************************
     C                   ENDIF
     C*
     C     VIEW#         IFEQ      2
     C                   MOVEL     EONTWK        SCNTWK
     C     @@RRN2        CHAIN     PCA03                              99
     C*
     C                   MOVEL     CADSEQ        KKDSEQ
     C*
     C                   MOVEL     CALIN#        ##NUM            80
     C                   MOVE      *BLANKS       ##MASK           10
     C                   Z-ADD     0             ##DEC             3 0
     C                   MOVE      *BLANK        ##THOU            1
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         SCLIN#
     C*
     C*****                MOVELCADSEQ    SCDSEQ
     C                   MOVEL     CADSEQ        ##NUM
     C                   MOVE      *BLANKS       ##MASK
     C                   Z-ADD     0             ##DEC
     C                   MOVE      *BLANK        ##THOU
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   PARM                    ##DEC
     C                   PARM                    ##THOU
     C                   MOVEL     ##NUM         SCDSEQ
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL(P)  CACPT4        ALCODE
     C*
     C                   MOVE      ALCODE        KKCODE
     C     PRKY          SETGT     PPR01                              95
     C     PRKY1         READPE    PPR01                                  95
     C                   MOVEL(P)  PRDSC1        SCDESC
     C                   ENDIF
     C*
    C*** Beg Add *** MC367 *** 11/15/12 **********************************
      /Free
      /If Not Defined(WEB)
       //Disable Function code F14
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '14';
           Exsr UPDOFA;
      /EndIf
      /End-Free
    C*** End Add *** MC367 *** 11/15/12 **********************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1B         READE     DBRCDF                                 95
     C   22KEY2B         READE     DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1B         READPE    DBRCDF                                 95
     C   22KEY2B         READPE    DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1A         SETGT     DBRCDF                             96
     C   22KEY2A         SETGT     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
     C   22KEY2A         SETLL     DBRCD2                             96
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
Opt  Change      Code          Description          Charged          Paid  Rcn
Opt  Change      Code          Description          Charged          Paid  Rcn
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
211F11=Change View          NNN
212F12=Cancel               YYN
214F14=DARTS                YYN
215F15=Fold/Truncate        YYN
217F17=Subset               NNN
3
