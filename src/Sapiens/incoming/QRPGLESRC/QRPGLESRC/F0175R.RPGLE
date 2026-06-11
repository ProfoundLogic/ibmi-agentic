     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H* PROGRAM: F0175R                                              *
     H* DESCRIPTION: CASE STATUS HISTORY FILE PROGRAM                *
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H*  06/09/20 C:212 Project : SY420                              *
     H*                 Mask SSN                                     *
     H*                 Recompile Only                               *
     H*                                                              *
     H*  03/02/00 C:62  PROJECT: MCMNT **BASE ONLY**                 *
     H*                 CHANGE LOGICAL TO EFDT DESCEND               *
     H*                                                              *
     H*  07/01/99 C:62  PROJECT: MC207                               *
     H*                 ADD USER TO COLUMN                           *
     H*                                                              *
     H* 10/14/98 C:110PROJECT: MC145                                 *
     H*               ADDED EFFECTIVE DATE POSITIONING               *
     H*                                                              *
     H*                                                              *
     H* 01/27/98 C:62 PROJECT: MC145                                 *
     H*               1) ADDED SA001R - ACCESS FLAGS                 *
     H*               2) ADDED FA703R - DARTS                        *
     H*                                                              *
     H* 01/08/98 C:86 PROJECT: MC145                                 *
     H*               DISPLAY FILE F0175F HAS BEEN CHANGED TO PRINT  *
     H*               A 2 LINE SUB-HEADING. THIS PROGRAM CALLS FA702R*
     H*               TO PRINT IT & USES LIKE DEFINES RO REDEFINE THE*
     H*               OMMITTED SCREEN FIELDS.                        *
     H*                                                              *
     H* 02/18/97 C:56  87MAZ                                         *
     H*                CREATED                                       *
     H****************************************************************
     FF0175F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFMCHP     IF   E             DISK
     F                                     RENAME(PCH01:PFRCDF)
     F*** BEGIN CHG *** 03/02/00 **************************************
     F**********FMCHL2  IF  E           K        DISK
     FFMCHL3    IF   E           K DISK
     F*** END   CHG *** 03/02/00 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCH01:DBRCDF)
     FFMCMP     IF   E             DISK
      /COPY SCOPYWW01D
     D********** BEGIN DELETE ******************* 01/08/97 ************
     F**********     FMCDL   IF  E           K        DISK
     D********** END   DELETE ******************* 01/08/97 ************
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column Headings
     D OF              S              1    DIM(8) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(8) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D OPTNM           C                   CONST('Case Status History')
     D ACDPGM          C                   CONST('F0175R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D SF1PAG          C                   CONST(9)
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     I*
     I* Program specific constants
     I*

     C*
     C     *LIKE         DEFINE    CHCO#         KKCO#
     C     *LIKE         DEFINE    CHSUB#        KKSUB#
     C     *LIKE         DEFINE    CHFND         KKFND
     C     *LIKE         DEFINE    CHFYR         KKFYR
     C     *LIKE         DEFINE    CHCASE        KKCASE
     C     *LIKE         DEFINE    CHEFDT        KKEFDT
     C*** BEGIN ADD *** 03/02/00 **************************************
     C     *LIKE         DEFINE    CHIDT         KKIDT
     C     *LIKE         DEFINE    CHITIM        KKITIM
     C*** END   ADD *** 03/02/00 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FMCHP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *BLANKS       ##OCST            1
     C                   PARM      SCDBRR        ##RRN             9 0          RRN FMCHP
     C                   PARM      @@RRN2        ##RRN2            9 0          RRN FMCMP
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       ##KEY            40
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEFDT
     C*** BEGIN ADD *** 03/02/00 **************************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*** END   ADD *** 03/02/00 **************************************
     C*
     C********** BEGIN DELETE ******************* 01/08/97 ************
     C********** CDKEY     KLIST
     C**********           KFLD           XXSS#
     C********** END   DELETE ******************* 01/08/97 ************
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
     C*
     C*  GET CLAIM MASTER RECORD.
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C*** BEGIN CHG *** 03/02/00 **************************************
     C**********           MOVEL*LOVAL    KKEFDT
     C                   MOVEL     *HIVAL        KKEFDT
     C                   MOVEL     *HIVAL        KKIDT
     C                   MOVEL     *HIVAL        KKITIM
     C*** END   CHG *** 03/02/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C*** BEG DEL *** 10/14/98 ****************************************
     C*******************  MOVE SCPST1    X1EFDT  80
     C*******************
     C*******************  Z-ADDX1EFDT    SRDTI
     C*******************  CALL 'SRCVTDAT'DTCYMD
     C*******************  MOVELSRDTO     KKEFDT
     C*** END DEL *** 10/14/98 ****************************************
     C*** BEG ADD *** 10/14/98 ****************************************
     C   21              MOVE      SCPST1        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*** BEGIN ADD *** 03/02/00 **************************************
     C                   Z-ADD     *HIVAL        KKIDT
     C                   Z-ADD     *HIVAL        KKITIM
     C*** END   ADD *** 03/02/00 **************************************
      *
     C*** END ADD *** 10/14/98 ****************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*** BEGIN ADD *** 03/02/00 **************************************
     C                   Z-ADD     SCIDT         KKIDT
     C                   Z-ADD     SCITIM        KKITIM
     C*** END   ADD *** 03/02/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA703R'      DRTSPL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCOCST
     C                   MOVE      *BLANKS       SCEXPL
     C                   Z-ADD     0             SCEFDT
     C*
     C                   MOVEL     CHOCST        SCOCST
     C                   MOVEL     CHEXPL        SCEXPL
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*** BEGIN ADD *** 07/01/99 **************************************
     C                   MOVEL(P)  CHUSER        SCUSER
     C*** END   ADD *** 07/01/99 **************************************
     C*** BEGIN ADD *** 03/02/00 **************************************
     C                   Z-ADD     CHIDT         SCIDT
     C                   Z-ADD     CHITIM        SCITIM
     C*** END   ADD *** 03/02/00 **************************************
     C*
     C                   ENDSR
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
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*** GET SYSTEM INFO
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
     C***                  Z-ADD7         SF1PAG  50
     C***                  MOVELOPTNM     ##DATA    P
     C***                  CALL 'SRCENTER'
     C***                  PARM           ##DATA 80
     C***                  PARM 40        ##LENG  30
     C***                  MOVEL##DATA    SCPRDS
     C*********** BEGIN DELETE ******************* 01/08/98 ***********
     C*********            MOVELCMSS#     XXSS#
     C*********  CDKEY     CHAINPCD01                98
     C*********  *IN98     IFEQ '0'
     C*********            MOVELCDLSN     ##LAST 56
     C*********            MOVELCDFSN     ##FRST 15
     C*********            MOVE '1'       ##TERM  1
     C*********            CALL 'SRNAME'
     C*********            PARM           ##LAST
     C*********            PARM           ##FRST
     C*********            PARM           ##TERM
     C*********            MOVEL##LAST    SCCLNM
     C*********            ELSE
     C*********            MOVEL*BLANKS   SCCLNM
     C*********            ENDIF
     C*********** END   DELETE ******************* 01/08/98 ***********
     C*********** BEGIN ADD ********************** 01/08/98 ***********
     C*********** BEGIN ADD ********************** 01/08/98 ***********
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
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
     C*********** END   ADD ********************** 01/08/98 ***********
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
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
     C                   ENDSR
** Column headings
Opt   Eff Date  Sts Explanation                                     User
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYN
206F6=Add                   NNN
212F12=Cancel               YYY
214F14=DARTS                YYY
3
