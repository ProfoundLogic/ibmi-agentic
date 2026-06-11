     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0176R
     H* DESCRIPTION: WORK HISTORY MAINTENANCE PROGRAM
     H**********************************************************
     H* INDICATORS:
     H**********************************************************
     H* REVISIONS:
     H*                                                              *
     H* 06/09/20 C:212 Project : SY420                               *
     H*                Mask SSN                                      *
     H*                Recompile Only                                *
     H*                                                              *
     H* 01/08/98 C:86 PROJECT: MC145                                 *
     H*          1.   DISPLAY FILE F0176F HAS BEEN CHANGED TO PRINT  *
     H*               A 2 LINE SUB-HEADING. THIS PROGRAM CALLS FA702R*
     H*               TO PRINT IT & USES LIKE DEFINES TO REDEFINE THE*
     H*               OMMITTED SCREEN FIELDS.                        *
     H*                                                              *
     H*          2.   DARTS CALL WITH OPTION 14 TO SC010R HAS  *
     H*               BEEN REPLACED WITH A CALL TO FA703R AND A*
     H*               CALL TO SA001R FOR CORRECT FLAGS & INFO  *
     H*                                                        *
     H* 04/01/97 C:32  PROJ: 87MAZ
     H*                CREATED PGM
     H*
     H* 06/12/97 C:56  PROJECT: 87MAZ  CO# 23
     H*             1. REMOVE LEVEL OF DUTY PERCENTAGE
     H*             2. ADD F5=REFRESH
     H**********************************************************
     FF0176F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFAWHP     IF   E             DISK
     F                                     RENAME(PWH01:PFRCDF)
     FFAWHL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PWH01:DBRCDF)
     FFMCMP     IF   E             DISK
     F************        FMCDL   IF  E           K        DISK
     FFTDLL     IF   E           K DISK
     F********* BEGIN DELETE  ******************* 01/08/98 ************
     F********* FMF1L   IF  E           K        DISK
     F********* END   DELETE  ******************* 01/08/98 ************
     FFTGTL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column Headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D OPTNM           C                   CONST('Work History')
     D ACDPGM          C                   CONST('F0176R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D*
     D* Program specific constants
     D*
     D DL              C                   CONST('* * * DELETED * * *')

     I*

     C*
     C     *LIKE         DEFINE    WHCO#         KKCO#
     C     *LIKE         DEFINE    WHSUB#        KKSUB#
     C     *LIKE         DEFINE    WHFND         KKFND
     C     *LIKE         DEFINE    WHFYR         KKFYR
     C     *LIKE         DEFINE    WHCASE        KKCASE
     C     *LIKE         DEFINE    WHWKDT        KKWKDT
     C     *LIKE         DEFINE    DLKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKGKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FAWHP
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
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
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
     C                   KFLD                    KKWKDT
     C*
     C     DLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKGKEY
     C*
     C*
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
     C* GET CLAIM MASTER RECORD.
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C                   MOVEL     *HIVAL        KKWKDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        XXWKDT            8 0
     C*
     C                   Z-ADD     XXWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKWKDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKWKDT
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
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   SETON                                        90
     C*
     C************** END   ADD ***************** 01/08/98 *************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCCODE
     C                   Z-ADD     0             SCWKDT
     C                   MOVE      *BLANKS       SCLVL
     C*
     C                   MOVEL(P)  'WKCD'        KKTBID
     C                   MOVEL(P)  WHCODE        KKGKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL     GTDESC        SCDESC
     C   99              MOVEL     *BLANKS       SCDESC
     C*
     C                   MOVEL(P)  WHCODE        SCCODE
     C*
     C                   Z-ADD     WHWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWKDT
     C*
     C                   MOVEL(P)  WHLVL         SCLVL
     C*** BEGIN DEL *** 06/12/97 **************************************
     C**********           Z-ADDWHPCT     SCPCT
     C*** END DEL *** 06/12/97 ****************************************
     C*
     C                   MOVE      *BLANKS       SCDESW
     C     WHLVL         IFNE      *BLANKS
     C                   MOVEL(P)  WHLVL         KKKEY
     C     DLKY          CHAIN     PDL01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DLDESC        SCDESW
     C                   ENDIF
     C                   ENDIF
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
     C************* BEGIN ADD ****************** 01/08/98 *************
     C*
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C************* END   ADD ****************** 01/08/98 *************
     C***                  Z-ADD6         SF1PAG  50
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
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
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
     C*
     C*********** END   ADD ********************** 01/08/98 ***********
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
     C*
     C                   MOVE      DL            SCDESC
     C*
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
Opt  Code Description             Date      Duty      Description
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYN
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=DARTS                YYN
3
