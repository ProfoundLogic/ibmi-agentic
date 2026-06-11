     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  F0490R - WORK WITH EOMBS BILL DETAIL                         *
     H*                                                               *
     H*  CALLED FROM: MANY PLACES                                     *
     H*  CALLS TO: F0453R                                             *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 07/18/23 C:342 Project: NY060
     H*                NY Compliance: CARC/RARC Update
     H*
     H* 04/18/18  C:079 Support: 99 70430
     H*                 Call newly CompSuited EDI program
     H*
     H* 06/13/08  C:079 Project: FL061
     H*                 CHECK TO SEE IF IN CAPTURE POLICY STATE MODE
     H*
     H* 05/15/07  C:182 Project: FL067
     H*                 Change made to show HCPCS code on Final
     H*                 Detail Screen.
     H*
     H* 02/21/07  C:188 SUPPORT: 57052
     H*                 ADDED CALL TO FA744R TO CORRECT
     H*                 DIAGNOSIS INDICATOR EDIT ISSUE
     H*
     H* 05/03/07  C:188 Project: FL061
     H*                 Use policy state when not blank to F0493R call
     H*
     H* 02/21/07  C:188  SUPPORT: 57052
     H*                 ADDED CALL TO FA744R TO CORRECT
     H*                 DIAGNOSIS INDICATOR EDIT ISSUE
     H*
     H* 08/03/05  C:79  Support: 48400
     H*                 Pass CO# in to F0493r when no records in FDCAP
     H*
     H* 04/26/04  C:62  Project: FS279
     H*                 Display additional fields.
     H*
     H* 08/19/03  C:62  Project: FL045
     H*                 Display the billed code not the paid code
     H*
     H* 05/14/02  C:79  Support: MC025 31715
     H*                 Add option 11=EOB codes/notes
     H*                 Add F11=Header EOB/notes
     H*
     H* 01/24/02  C:79  Support: MC044 30854
     H*                 Correct postion to date so it works correctly
     H*
     H* 01/21/02  C:79  Support: MC044 30758
     H*                 Replace heading "Code" with "Paid Code"
     H*
     H* 03/09/00  C:53  Project: KY010
     H*                 ADD POPULATION OF SCDBR2
     H*
     H*****************************************************************
     FF0490F    CF   E             WORKSTN
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
     FFMCDL     IF   E           K DISK
     FFAEOP     IF   E             DISK
     FFDDHL     IF   E           K DISK
     F                                     INFDS(DBFDSB)
     FFDDUL     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FFDDPL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FFDDDL     IF   E           K DISK
     F                                     INFDS(DBFDSG)
     F*** BEGIN ADD *** 04/26/04 **************************************
     FFDDEL     IF   E           K DISK
     F*** END   ADD *** 04/26/04 **************************************
      /COPY SCOPYWW01D
     D*
     D**** Beg Chg *** 07/18/23 **************************************
     D**** HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D HD              S             80    DIM(2) CTDATA PERRCD(1)              Column headings
     D**** Beg Chg *** 07/18/23 **************************************
     D*** BEG CHG *** 05/14/02 ****************************************
     D OF              S              1    DIM(9) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(9) ALT(OF)
     D*** END CHG *** 05/14/02 ****************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSG          DS
     D  DBRRNG               397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Detail - Data Subm C-
     D                                     hgs')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D CHGNUM          C                   CONST('SA020R')
     D******        'F0453R'              C         ACDPGM
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(6)
     D*
     D*  Date Data Structure used for Century
     D                 DS
     D  XXDAT                  1      8
     D  XXMTH                  1      2
     D  XXDAY                  3      4
     D  XXYEAR                 5      8
     D*
     D                 DS
     D  ##NUM                  1     80
     D  XXNUM                  6      9
     D*
     D*  VIEW 1 Data Structure
     D                 DS
     D  XADATA                 1     60
     D  XAFDAT                 1     10
     D  XACPT4                12     23
     D*** BEGIN CHG *** 04/26/04 **************************************
     D**********                             25  60 XADESC
     D  XADESC                25     40
     D*** END   CHG *** 04/26/04 **************************************
     D*
     D*  VIEW 2 Data Structure
     D                 DS
     D  XBDATA                 1     60
     D  XBCPT4                 1     12
     D  XBFDAT                14     23
     D*** BEGIN CHG *** 04/26/04 **************************************
     D**********                             25  60 XBDESC
     D  XBDESC                25     60
     D*** END   CHG *** 04/26/04 **************************************
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    ##RRN4            9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM                    ##RRN             9 0
     C                   PARM                    @@CNTL           20
     C                   PARM                    @@PGM            10
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBR2        ##RRN3            9 0          ADD 03/09/00
     C                   PARM                    ##RRN2            9 0
     C                   PARM      SCDBRR        ##RRN4            9 0
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
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C*
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
     C                   KFLD                    KKDSEQ
     C*** BEG ADD *** 01/24/02 ****************************************
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKFDAT
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 01/24/02 ****************************************
     C*
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
     C     DHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*** BEGIN ADD *** 04/26/04 **************************************
     C*
     C     DEKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*** END   ADD *** 04/26/04 **************************************
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
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
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
     C     ##RRN2        CHAIN     PEO01                              60
     C                   MOVE      EOCNTL        QQCNTL
     C                   MOVEL     SCCONM        XXCONM           40
     C                   MOVEL     SCUSER        XXUSER           10
     C                   MOVEL     EOEOB#        XXEOB#            9 0
     C*** BEG ADD *** 05/03/07 ****************************************
     C     EOPOST        IFNE      *BLANKS
     C*** BEG ADD *** 06/13/08 ****************************************
     C     EOCPST        ANDEQ     'Y'
     C*** END ADD *** 06/13/08 ****************************************
     C                   MOVEL     EOPOST        XXST              2
     C                   ELSE
     C                   MOVEL     EOAST         XXST              2
     C                   ENDIF
     C*** END ADD *** 05/03/07 ****************************************
     C*** BEG CHG *** 08/03/05 ****************************************
     C******************** MOVELCACO#     QQCO#
     C                   MOVEL     EOCO#         QQCO#
     C*** END CHG *** 08/03/05 ****************************************
     C*
     C*** Beg Chg *** 04/18/18 ****************************************
     C***
     C***                CALL      'F0493R'
     C***                PARM                    QQCO#
     C***                PARM                    QQCALL            1
     C***  *INKC         PARM                    QQKC              1
     C***  *INKL         PARM                    QQKL              1
     C***                PARM                    QQCNTL
     C***                PARM                    XXCONM           40
     C***                PARM                    XXUSER           10
     C***                PARM                    XXEOB#            9 0
     C***                PARM                    XXST              2
     C                   Call      'MTEDISUB'
     C                   Parm                    qqKc              1
     C                   Parm                    qqKl              1
     C                   Parm                    xxEOB#            9 0
     C                   Parm                    qqRtCd            1
     C                   If        qqKc = *on
     C                   ExSr      PgmEnd
     C                   Endif
     C*** End Chg *** 04/18/18 ****************************************
     C*
     C                   ENDSR
     C*** BEG ADD *** 05/14/02 ****************************************
     C*================================================================
     C* KHDO - F8=Header EOB/Notes
     C*================================================================
     C     KHDO          BEGSR
     C*
     C                   CALL      'F0355R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      ##RRN2        @@RRN2            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 05/14/02 ****************************************
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
     C*** BEG ADD *** 01/24/02 ****************************************
     C                   Z-ADD     0             KKDSEQ
     C*** END ADD *** 01/24/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*** BEG DEL *** 01/24/02 ****************************************
     C** 21 ************** MOVELSCPST1    KKFDAT
     C*** END DEL *** 01/24/02 ****************************************
     C*
     C*** BEG ADD *** 01/24/02 ****************************************
      *
     C   21              MOVE      SCPST1        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFDAT
     C*** END ADD *** 01/24/02 ****************************************
     C*
     C   22              MOVEL     SCPST2        KKCPT4
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCFDAT        KKFDAT
     C                   MOVEL(P)  SCCPT4        KKCPT4
     C*** BEG ADD *** 01/24/02 ****************************************
     C                   Z-ADD     SCDSEQ        KKDSEQ
     C*** END ADD *** 01/24/02 ****************************************
     C                   ENDSR
     C*** BEGIN ADD *** 04/26/04 **************************************
     C*================================================================
     C* CHRPMT- Highlight line if payment does not match charged amount
     C*================================================================
     C     CHRPMT        BEGSR
     C*
     C* THIS EDIT IS NECESSARY ONLY IF THE ADJUDICATON STATE IS TEXAS
     C* AND FA204R IS INSTALLED ON SYSTEM
     C*
     C     CAAST         IFEQ      'TX'
     C*
     C                   CALL      'FA722C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'FA204R'      ##OBJ            10
     C                   PARM      '*PGM'        ##TYP             7
     C*
     C     ##RTN         IFNE      'F'
     C*
     C                   CALL      'FA204R'
     C                   PARM      ' '           ##RTN1            1
     C                   PARM      @@RRN         ##RRN1            9 0
     C                   PARM      SCDSEQ        ##DSEQ            5 0
     C*
     C     ##RTN1        IFEQ      'F'
     C                   SETON                                        34
     C                   ELSE
     C                   SETOFF                                       34
     C                   ENDIF                                                  ##RTN1 = 'F'
     C*
     C                   ENDIF                                                  ##RTN NE 'F'
     C                   ENDIF                                                  CAAST = 'TX'
     C*
     C                   ENDSR
     C*** END   ADD *** 04/26/04 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     CASS#         KKSS#
     C                   MOVEL     CACPT4        KKCPT4
     C*** BEG CHG *** 01/24/02 ****************************************
     C******************** MOVE CAFDAT    SCFDAT
     C                   Z-ADD     CAFDAT        SCFDAT
     C*** END CHG *** 01/24/02 ****************************************
     C*** BEG ADD *** 01/24/02 ****************************************
     C                   Z-ADD     CADSEQ        SCDSEQ
     C                   MOVEL     CACPT4        SCCPT4
     C*** END ADD *** 01/24/02 ****************************************
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   MOVEL     CACO#         KKCO#
     C                   MOVEL     CASUB#        KKSUB#
     C                   MOVEL     CAFND         KKFND
     C                   MOVEL     CAFYR         KKFYR
     C                   MOVEL     CACASE        KKCASE
     C                   MOVEL     CAEOB#        KKEOB#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDAT            8 0
     C*
     C                   MOVEL(P)  XXFDAT        ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         X1FDAT           10
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*
     C                   CLEAR                   XADATA
     C                   CLEAR                   XBDATA
     C*
     C*  IF PHYSICIAN BILL
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C*
     C     DHKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNB        SCDBR2                         ADD 03/09/00
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVELDHPRCD    XXCPT4 12 P
     C**********           MOVELDHPRDS    X1DESC 13 P
     C                   MOVEL(P)  DHBPRC        XXCPT4           12
     C                   MOVEL(P)  DHBPRD        X1DESC           13
     C*** END   CHG *** 08/19/03 **************************************
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'H'
     C*
     C     DUKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNE        SCDBR2                         ADD 03/09/00
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVELDURVCD    KKRVCD
     C**********           MOVE DURATE    KKHCPC
     C                   MOVEL     DUBPRC        KKRVCD
     C                   MOVE      DUBRAT        KKHCPC
     C*** END   CHG *** 08/19/03 **************************************
     C                   MOVE      *BLANKS       KKUNTS
     C                   MOVEL(P)  KKCMSV        XXCPT4
     C*** BEGIN CHG *** 08/19/03 **************************************
     C**********           MOVELDUDESC    X1DESC    P
     C                   MOVEL(P)  DUBPRD        X1DESC
     C*** END   CHG *** 08/19/03 **************************************
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'X'
     C     DPKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNF        SCDBR2                         ADD 03/09/00
    C*** BEG ADD *** 05/15/07 ****************************************
     C     DPRXPT        IFEQ      'D'
    C*** END ADD *** 05/15/07 ****************************************
     C                   MOVEL(P)  DPNDC         XXCPT4
    C*** BEG ADD *** 05/15/07 ****************************************
     C                   ELSE
     C                   MOVEL(P)  DPHCPC        XXCPT4
     C                   ENDIF
    C*** END ADD *** 05/15/07 ****************************************
     C                   MOVEL(P)  DPDESC        X1DESC
     C                   ELSE
     C                   MOVE      *BLANKS       X1DESC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     EOBTYP        WHENEQ    'D'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNG        SCDBR2                         ADD 03/09/00
     C                   MOVEL(P)  DDPRCD        XXCPT4
     C                   MOVEL(P)  DDPRDS        X1DESC
     C                   ELSE
     C                   MOVE      *BLANKS       X1DESC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDSL                                                  EOBTYP N,V,H,XD
     C*
     C     *IN21         IFEQ      *ON
     C                   MOVEL     X1FDAT        XAFDAT
     C                   MOVEL(P)  XXCPT4        XACPT4
     C                   MOVEL(P)  X1DESC        XADESC
     C                   MOVEL(P)  XADATA        SCDATA
     C                   ENDIF                                                  *IN21 = *ON
     C*
     C     *IN22         IFEQ      *ON
     C                   MOVEL     X1FDAT        XBFDAT
     C                   MOVEL(P)  XXCPT4        XBCPT4
     C                   MOVEL(P)  X1DESC        XBDESC
     C                   MOVEL(P)  XBDATA        SCDATA
     C                   ENDIF                                                  *IN22 = *ON
     C*** BEGIN ADD *** 04/26/04 **************************************
     C                   Z-ADD     CACHRG        SCCHRG
     C                   Z-ADD     CAPAID        SCPAID
     C                   Z-ADD     CARCN         SCRCN
     C*
     C* GET MOST RECENTLY ENTERED ACTIVE EOMB CODE
     C*
     C                   CLEAR                   SCEOB
     C                   MOVE      *ZEROS        COUNT2            2 0
     C*
     C     DEKY          SETGT     PDE01
     C     DEKY          READPE    PDE01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C*** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C     DESTAT        ANDNE     'I'
     C     COUNT2        IFEQ      0
     C*** Beg Chg *** 07/18/23 **************************************
     C*****              MOVEL     DEEOB         SCEOB
     C                   MOVEL     DEEOBCD       SCEOB
     C*** End Chg *** 07/18/23 **************************************
     C                   ENDIF
     C                   ADD       1             COUNT2
     C                   ENDIF
     C*
     C     DEKY          READPE    PDE01                                  99
     C                   ENDDO
     C*
     C     COUNT2        IFGT      1
     C                   SETON                                        51
     C                   ELSE
     C                   SETOFF                                       51
     C                   ENDIF                                                  COUNT GT 1
     C*
     C* IF PAYMENT AMOUNT IN NOT EQUAL THE CHARGE AMOUNT, CALL
     C* SUBROUTINE AND DO EDIT TO CHECK IF EOB CODE IS ASSIGNED
     C* EXCLUDING FSMRA
     C*
     C                   SETOFF                                       34
     C     SCPAID        IFNE      SCCHRG
     C     CADELF        ANDNE     'X'
     C                   EXSR      CHRPMT
     C                   ENDIF                                                  SCPAID NE SCCHRG
     C*** END   ADD *** 04/26/04 **************************************
     C*
     C                   ENDSR
     C*** BEG ADD *** 05/14/02 ****************************************
     C*================================================================
     C* OPT11  - EOB/Notes
     C*================================================================
     C     OPT11         BEGSR
     C*
     C                   CALL      'F0356R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      ##RRN2        @@RRN2            9 0
     C                   PARM      SCDBRR        @@RRN3            9 0
     C*
     C                   ENDSR
     C*** END ADD *** 05/14/02 ****************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKB         CASEQ     *ON           KBDO
     C*** BEG ADD *** 05/14/02 ****************************************
     C     *INKH         CASEQ     *ON           KHDO                           F8
     C*** END ADD *** 05/14/02 ****************************************
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*** BEG ADD *** 05/14/02 ****************************************
     C     SCSEL         CASEQ     '11'          OPT11
     C                   ENDCS
     C*** END ADD *** 05/14/02 ****************************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** BEG ADD *** 02/21/07 ****************************************
     C*
     C* CALL PGM TO UPDATE DIAGNOSIS INDICATORS
     C*
     C                   CALL      'FA744R'
     C                   PARM                    @@RRN2
     C                   PARM                    RTNCOD            1
     C*
     C*** END ADD *** 02/21/07 ****************************************
     C*** Beg Add *** 04/18/18 ****************************************
     C                   Eval      *InLr = *on
     C                   Return
     C*** End Add *** 04/18/18 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL     @@PGM         ACDPGM           10
     C*
     C                   MOVEL(P)  EOSUB#        XXSUB#            5
     C                   MOVEL(P)  EOFND         XXFND             3
     C                   MOVEL(P)  EOFYR         XXFYR             3
     C                   MOVEL(P)  EOCASE        XXCASE            7
     C*
     C** Get standard screen headings
     C*
     C     ##RRN2        CHAIN(N)  PEO01                              99
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
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    ##RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
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
     C*** BEG CHG *** 01/24/02 ****************************************
     C** 21 **** KEY2      SETGTDBRCDF               96
     C   21KEY4          SETGT     DBRCDF                             96
     C*** END CHG *** 01/24/02 ****************************************
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
Opt Service    Billed Code  Description           Charged       Paid EOB     Rcn
Opt Billed Code  Service    Description           Charged       Paid EOB     Rcn
** Description--------------MISN
1022=Change                 YNN
1055=Display                YYY
11111=EOB/Notes             YYY
202F2=Process               YYY
203F3=Exit                  YYN
208F8=Header EOB/Notes      YYN
211F11=Change view          YNN
211F12=Previous             NNN
3
