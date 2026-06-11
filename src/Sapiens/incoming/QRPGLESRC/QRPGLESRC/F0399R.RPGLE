     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  F0399R - WORK WITH EOMB LETTERS                C:62 06/24/99 *
     H*                                                 MC202         *
     H*  CALLED FROM: F0350R AND F0395R                               *
     H*  CALLS TO:    F0751R AND F0766C                               *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  04/19/06 C:163 PROJECT 114WMF
     H*                 Back to Base - EOMB Processing (114MEO)
     H*
     H*  02/21/00 C:31  Project: FSMNT #88
     H*                 Correct last print date
     H*
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0399F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFALTP     IF   E             DISK
     F                                     RENAME(PLT01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFALTL7    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PLT01:DBRCDF)
     FFAEOP     IF   E             DISK
     FFAESP     IF   E             DISK
     FFTBSL     IF   E           K DISK
     FFAESL2    IF   E           K DISK
     F                                     RENAME(PES01:PES02)
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
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EOMB Letters')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D*** BEGIN DEL *** 04/19/06 **************************************
     D************* 'F0751R'              C         ACDPGM
     D*** END   DEL *** 04/19/06 **************************************
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D CLROPT          C                   CONST('                    -
     D                                            NNN')
     D PRDS            C                   CONST('       Request Lette-
     D                                     r Maintenance       ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FALTP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@RRN3            9 0          FAESP
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      PRDS          ##PRDS           40
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     ACDPL2        PLIST
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN2        ##RRN             9 0          FAEOP
     C                   PARM      SCDBRR        ##RRN2            9 0          FALTP
     C     *INKL         PARM      *BLANKS       ##KL              1
     C*** END   ADD *** 04/19/06 **************************************
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
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKLTR#
     C*
     C     BSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     ESKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    LTCO#         KKCO#
     C     *LIKE         DEFINE    LTSUB#        KKSUB#
     C     *LIKE         DEFINE    LTFND         KKFND
     C     *LIKE         DEFINE    LTFYR         KKFYR
     C     *LIKE         DEFINE    LTCASE        KKCASE
     C     *LIKE         DEFINE    LTEOB#        KKEOB#
     C     *LIKE         DEFINE    LTRCN         KKRCN
     C     *LIKE         DEFINE    LTLTR#        KKLTR#
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
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
     C     SSLTR#        IFNE      *ZEROS
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
     C* DON'T ALLOW THE USE OF OPTION '2' IF NO LONGER IN PEND STATUS.
     C*
     C     SCSEL         IFEQ      '2'
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKIDT
     C*
     C                   Z-ADD     DSTIME        KKITIM
     C*
     C     ESKY2         SETGT     PES02
     C     ESKY          READPE    PES02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     ESBLST        ANDNE     'P'
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '5'
     C     SCFLG2        IFEQ      'T'
     C     SCSEL         IFEQ      '2'
     C                   MOVEL(P)  'C'           XXMODE            1
     C                   ELSE
     C                   MOVEL(P)  'D'           XXMODE
     C                   ENDIF
     C                   MOVEL     '99'          SCSEL
     C                   MOVEL(P)  'F0757R'      ACDPGM           10
     C                   ELSE
     C                   MOVEL(P)  'F0751R'      ACDPGM
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL         IFEQ      '6'
     C     SCFLG2        IFEQ      'T'
     C                   MOVEL(P)  'F0758C'      RPTPGM           10
     C                   ELSE
     C                   MOVEL(P)  'F0765C'      RPTPGM
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 04/19/06 **************************************
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
     C     @@RRN3        IFNE      *ZEROS
     C*
     C     @@RRN3        CHAIN     PES01                              99
     C*
     C                   Z-ADD     ESLTR#        SSLTR#
     C                   SETON                                        16
     C*
     C                   ENDIF
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
     C                   Z-ADD     EORCN         KKRCN
     C*
     C                   Z-ADD     *HIVAL        KKLTR#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        KKLTR#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCLTR#        KKLTR#
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     LTLTR#        SCLTR#
     C                   Z-ADD     LTFEIN        SCFEIN
     C                   Z-ADD     LTMOD         SCMOD
     C*
     C*** BEGIN ADD *** 02/21/00 **************************************
     C********             Z-ADDLTTRDT    SRDTI
     C                   Z-ADD     LTPRDT        SRDTI
     C*** END ADD *** 02/21/00 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C                   Z-ADD     LTAMT         SCAMT
     C                   MOVEL(P)  LTFLG1        SCFLG1
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL(P)  LTFLG2        SCFLG2
     C*** END   ADD *** 04/19/06 **************************************
     C                   MOVEL(P)  LTSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - Print Pend Letter
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   MOVE      SCLTR#        #1LTR#
     C*
     C*** BEGIN CHG *** 04/19/06 **************************************
     C******************** CALL 'F0766C'
     C                   CALL      RPTPGM
     C*** END   CHG *** 04/19/06 **************************************
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM                    #1LTR#            9
     C                   PARM      'I'           #1FLAG            1
     C                   PARM      'QPRINT'      #1QUE            10
     C                   PARM      'D'           #1MODE            1
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 04/19/06 **************************************
     C*================================================================
     C* OPT99 - Alternative Change/Display Program
     C*================================================================
     C     OPT99         BEGSR
     C*
     C                   MOVE      XXMODE        ##MODE
     C                   CALL      ACDPGM        ACDPL2                 99
     C                   CLEAR                   SCSEL
     C*
     C                   ENDSR
     C*** END   ADD *** 04/19/06 **************************************
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
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     SCSEL         CASEQ     '99'          OPT99
     C*** END   ADD *** 04/19/06 **************************************
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
     C* 7GMSTR - Start of program processing
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
     C* LOAD THE THIRD HEADER LINE.
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   Z-ADD     EORCN         SCRCN
     C*
     C* LOAD THE FOURTH HEADER LINE
     C*
     C     @@RRN3        IFNE      *ZEROS
     C*
     C     @@RRN3        CHAIN     PES01                              99
     C*
     C                   MOVEL(P)  ESBLST        SCBLST
     C                   MOVEL(P)  ESSTCD        SCSTCD
     C*
     C                   MOVEL(P)  ESBLST        KKTYPE
     C                   MOVEL(P)  ESSTCD        KKCODE
     C*
     C     BSKY          CHAIN     PBS01                              99
     C*
     C  N99              MOVEL(P)  BSDESC        SCDESC
     C   99              MOVEL(P)  *BLANKS       SCDESC
     C*
     C                   Z-ADD     ESIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIDT
     C*
     C                   MOVEL     ESITIM        SCITIM
     C*
     C* IF COMING FROM F0395R THEN DISABLE OPTION '2' IF BILL IS
     C* NO LONGER IN A PEND STATUS.
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKIDT
     C*
     C                   Z-ADD     DSTIME        KKITIM
     C*
     C     ESKY2         SETGT     PES02
     C     ESKY          READPE    PES02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     ESBLST        ANDNE     'P'
     C*
     C                   MOVEL     CLROPT        OFD(2)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
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
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSLTR#        IFNE      *ZEROS
     C     SSLTR#        ANDNE     LTLTR#
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt    Letter#    Provider   Mod   Print Date      Charges  Req# Stat
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1066=Print                  YNN
203F3=Exit                  NNN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
