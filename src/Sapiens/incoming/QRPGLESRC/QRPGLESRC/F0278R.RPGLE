     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H* PROGRAM: F0278R                                              *
     H* DESCRIPTION: WORK WITH TREATMENT PLANS                       *
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 06/09/20 C:212  Project : SY420                              *
     H*                 Mask SSN                                     *
     H*                 Recompile Only                               *
     H*                                                              *
     H* 10/18/99 C:53 PROJECT: MCMNT                                 *
     H*               CHANGED EDI FLAGS TO NOT DISPLAY IN ARRAY      *
     H*               THEY ARE ONLY USED FOR OHIO.                   *
     H*                                                              *
     H* 02/08/98 C:62 PROJECT: MC145                                 *
     H*               1) REDUCED PARM LIST BY INCLUDING RRN          *
     H*                  OF FATRP.                                   *
     H*               2) PUT IN PLACE WW/MAINT STANDARDS.            *
     H*                                                              *
     H* 02/03/98 C:62 PROJECT: MC145                                 *
     H*               ***IMPLEMENTED CHANGES MADE FOR CLT087 FOR V4. *
     H*               1) ADD OPT19 - EDI LOG INFO.                   *
     H*               2) ADD OPT82 - SEND SUBSEQUENT 148.            *
     H*               3) FIX OPT82 - ADDED TSET TO F0070R6'S PARMS.  *
     H*                                                              *
     H* 01/30/98 C:62 PROJECT: MC145                                 *
     H*               - CHANGED CALL F0279R01 TO F0279R.             *
     H*                                                              *
     H* 01/08/98 C:86 PROJECT: MC145                                 *
     H*               DISPLAY FILE F0278F HAS BEEN CHANGED TO PRINT  *
     H*               A 2 LINE SUB-HEADING. THIS PROGRAM CALLS FA702R*
     H*               TO PRINT IT & USES LIKE DEFINES RO REDEFINE THE*
     H*               OMMITTED SCREEN FIELDS.                        *
     H*                                                              *
     H* 09/15/97 C:56  Project: MCO087 #8086 support                 *
     H*                Pass '1' for call to ICD9 option              *
     H*                                                              *
     H* 08/25/97 C:31  Project: 87MAZ  CO# 48                        *
     H*                Remove case management fields                 *
     H*                (Compile only)                                *
     H*                                                              *
     H****************************************************************
     FF0278F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFATRP     IF   E             DISK
     F                                     RENAME(PTR01:PFRCDF)
     F*  Primary logical file definition (for displaying items)
     FFATRL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PTR01:DBRCDF)
     FFMCMP     IF   E             DISK
     F********** BEGIN DELETE ******************* 01/08/97 ************
     F**********FMCDL   IF  E           K        DISK
     F********** END   DELETE ******************* 01/08/97 ************
     FFATIL1    IF   E           K DISK
     F                                     INFDS(DBFDSI)
     FFATDL1    IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D DBFDSI          DS
     D  DBRRNI               397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Treatment Plans    ')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0278R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D*** BEGIN ADD *** 02/04/98 **************************************
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')

     I*** END   ADD *** 02/04/98 **************************************
     I*
     I* Program specific constants
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FATRP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
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
     C*** BEGIN ADD *** 02/03/98 **************************************
     C*
     C*  EDI LOG INFO PARM LIST.
     C*
     C     LOGFPL        PLIST
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       ##ACTF            1
     C                   PARM      *BLANKS       ##MGID            7
     C                   PARM      *BLANKS       ##RTCD            1
     C*** END   ADD *** 02/03/98 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKBGDT
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C********** BEGIN DELETE ******************* 01/08/97 ************
     C********** CDKEY     KLIST
     C**********           KFLD           XXSS#
     C********** END   DELETE ******************* 01/08/97 ************
     C*
     C     TIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C*
     C     TDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    TRCO#         KKCO#
     C     *LIKE         DEFINE    TRSUB#        KKSUB#
     C     *LIKE         DEFINE    TRFND         KKFND
     C     *LIKE         DEFINE    TRFYR         KKFYR
     C     *LIKE         DEFINE    TRCASE        KKCASE
     C     *LIKE         DEFINE    TRBGDT        KKBGDT
     C     *LIKE         DEFINE    TITX#         KKTX#
     C*** BEGIN ADD *** 02/03/98 **************************************
     C*          *LIKE     DEFN LGEOB#    KKEOB#
     C*          *LIKE     DEFN LGTSET    KKTSET
     C*** END   ADD *** 02/03/98 **************************************
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
     C     ERRSTP        ENDSR
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
     C                   MOVE      *HIVAL        KKBGDT
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVE SCPST1    XXPST1  60
     C**********           Z-ADDXXPST1    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXPST1
     C**********           Z-ADDXXPST1    XXBGDT
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVE      SCPST1        XXBGDT            8 0
     C*
     C                   Z-ADD     XXBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKBGDT
     C*** END   CHG *** 12/02/97 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKBGDT
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
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C* CLEAR SCREEN FIELDS
     C                   MOVE      *BLANKS       SCICD9
     C                   MOVE      *ZEROS        SCBIDT
     C                   MOVE      *BLANKS       SCLOST
     C* GET ONLY ACTIVE RECORDS
     C*
     C                   Z-ADD     TRBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBGDT
     C*
     C                   Z-ADD     TRENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENDT
     C*
     C                   MOVE      TRMMI         SCMMI
     C                   MOVE      TRTX#         SCTX#
     C*
     C                   Z-ADD     TRLTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLTDT
     C*
     C                   MOVE      SCTX#         KKTX#
     C*
     C     TIKY          CHAIN     PTI01                              99
     C  N99TIPRIM        IFEQ      'Y'
     C                   MOVE      TIICD9        SCICD9
     C*
     C                   Z-ADD     TIBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBIDT
     C*
     C                   MOVE      TILOST        SCLOST
     C                   ENDIF
     C*
     C     TDKY          CHAIN     PTD01                              99
     C  N99TDTYPE        IFEQ      'D'
     C     TDSEQ#        ANDEQ     1
     C                   MOVEL     TDDESC        SCDESC           30
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDESC           30
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - ICD9 codes.
     C*================================================================
     C     OPT6          BEGSR
     C*
     C*** BEGIN CHG *** 01/30/98 **************************************
     C**********           CALL 'F0279R01'
     C                   CALL      'F0279R'
     C*** END   CHG *** 01/30/98 **************************************
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          RRN FATIP
     C                   PARM      SCDBRR        ##RRN2            9 0          RRN FMCMP
     C*
     C                   ENDSR
     C*================================================================
     C* OPT19 - EDI Log Info
     C*================================================================
     C     OPT19         BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      CMCASE        #1CASE
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMCO#         #1CO#
     C*
     C                   CALL      'W0270R5'
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM                    #1CASE            7
     C                   PARM                    #1FYR             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   ENDSR
     C*================================================================
     C* Opt82 - Send Subsequent 148
     C*================================================================
     C     OPT82         BEGSR
     C*
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      CMCO#         #1CO#                          COMPANY
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   ENDIF
     C*
     C                   CALL      'W0070R6'     LOGFPL
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
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '19'          OPT19
     C     SCSEL         CASEQ     '82'          OPT82
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      '1'           *INLR
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
     C*** BEGIN ADD *** 02/04/98 **************************************
     C*
     C* IF EDI IS INSTALLED THEN ALLOW OPT19 AND OPT82
     C*
     C     ##EDI         IFNE      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(4)
     C                   MOVEL     CLRFKY        OFD(5)
     C*
     C                   ENDIF
     C*** END   ADD *** 02/04/98 **************************************
     C*
     C*   SINGLE LINE DISPLAY ON ENTRY
     C                   MOVE      *ON           *IN12
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
     C*
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
Opt Begin Date  End Date     ICD9      Description of services          MMI
** Description--------------MISN
1022=Change                 YNN
1055=Display                YYY
1066=ICD9 Codes             YYY
11919=EDI Log Info          NNN
18282=Send Subsequent 148   NNN
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=DARTS                YYY
215F15=Fold/Truncate        YYY
3
