     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0395R - WORK WITH BILL STATUS HISTORY         C:62 06/22/98 *
     H*                                                               *
     H*  CALLED FROM: F0350R                                          *
     H*  CALLS TO:    F0395R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/24/08 C:62 PROJECT: SY156                                 *
     H*                CORRECT PAGING                                 *
     H*                                                               *
     H*  07/16/99 C:62 PROJECT: MC208                                 *
     H*                -ADD RCN# AS COLUMN                            *
     H*                                                               *
     H*  06/24/99 C:62 PROJECT: MC202                                 *
     H*                -ADD OPTION 30=LETTER                          *
     H*                -ADD FIELD TO INDICATE IF THERE IS A LETTER    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0395F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAESP     IF   E             DISK
     F                                     RENAME(PES01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFAESL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PES01:DBRCDF)
     FFAEOP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Status History')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0395R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FAESP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          FAESP
     C                   PARM      @@RRN2        ##RRN2            9 0          FAEOP
     C                   PARM      SCBLST        ##BLST            1
     C                   PARM      ' '           ##STCD            1
     C                   PARM      *BLANKS       ##REAS           60
     C                   PARM      *BLANKS       #1PGM             8
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
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
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
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    ESCO#         KKCO#
     C     *LIKE         DEFINE    ESSUB#        KKSUB#
     C     *LIKE         DEFINE    ESFND         KKFND
     C     *LIKE         DEFINE    ESFYR         KKFYR
     C     *LIKE         DEFINE    ESCASE        KKCASE
     C     *LIKE         DEFINE    ESEOB#        KKEOB#
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*** BEGIN ADD *** 06/24/99 **************************************
     C     SCSEL         IFEQ      '30'
     C     SCLTFL        ANDNE     'Y'
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C                   Z-ADD     *HIVAL        KKIDT
     C*** BEGIN ADD *** 06/24/08 **************************************
     C                   Z-ADD     *HIVAL        KKITIM
     C*** END   ADD *** 06/24/08 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKIDT
     C*
     C                   Z-ADD     0             KKITIM
     C     SCPSTA        IFEQ      *ZEROS
     C                   Z-ADD     240000        KKITIM
     C                   ELSE
     C                   MOVEL     SCPSTA        KKITIM
     C                   ENDIF                                                  SCPSTA = *ZEROS
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCIDT         KKIDT
     C*** BEGIN ADD *** 06/24/08 **************************************
     C                   Z-ADD     SCITM2        KKITIM
     C*** END   ADD *** 06/24/08 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      ESIDT         SCIDT
     C                   MOVEL(P)  ESBLST        SCBLST
     C                   MOVEL(P)  ESSTCD        SCSTCD
     C                   MOVEL(P)  ESREAS        SCREAS
     C*
     C                   Z-ADD     ESIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   MOVEL     ESITIM        SCITIM
     C*** BEGIN ADD *** 06/24/08 **************************************
     C                   Z-ADD     ESITIM        SCITM2
     C*** END   ADD *** 06/24/08 **************************************
     C*** BEGIN ADD *** 07/16/99 **************************************
     C                   Z-ADD     ESRCN         SCRCN
     C*** END   ADD *** 07/16/99 **************************************
     C                   MOVEL(P)  ESIUSR        SCIUSR
     C*
     C*** BEGIN ADD *** 06/24/99 **************************************
     C*
     C* DETERMINES IF EOMB STATUS ITEM HAS A LETTER ATTACHED TO IT
     C*
     C     ESBLST        IFEQ      'P'
     C*
     C     ESLTR#        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  'Y'           SCLTFL
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'N'           SCLTFL
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCLTFL
     C*
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 06/24/99 **************************************
     C*================================================================
     C* OPT30  - Pend Letter
     C*================================================================
     C     OPT30         BEGSR
     C*
     C                   CALL      'F0399R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FALTP
     C                   PARM      @@RRN2        ##RRN2            9 0          FAEOP
     C                   PARM      SCDBRR        ##RRN3            9 0          FAESP
     C*
     C                   ENDSR
     C*** END   ADD *** 06/24/99 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*** BEGIN ADD *** 06/24/99 **************************************
     C     SCSEL         CASEQ     '30'          OPT30
     C                   ENDCS
     C*** END   ADD *** 06/24/99 **************************************
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
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR
** Column headings
Opt Sts Date  Time Stat Typ Reason                           User     Rcn Ltr?
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
13030=Letter                YNN
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
