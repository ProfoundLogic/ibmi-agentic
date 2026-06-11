     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H* PROGRAM: F0279R                                              *
     H* DESCRIPTION: WORK WITH TREATMENT PLAN ICD9 CODES             *
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H*  02/09/98 C:62  PROJECT: MC145                               *
     H*                 1) REDUCED PARM LIST BY INCLUDING RRN        *
     H*                    OF FATRP.                                 *
     H*                 2) PUT IN PLACE WW/MAINT STANDARDS.          *
     H*                                                              *
     H*  02/01/98 C:62  Project: MC145                               *
     H*                 1) ADDED SA001R.                             *
     H*                 2) ADDED FA703R AND F14 FOR DARTS.           *
     H*                 3) ADDED FA702R FOR TWO LINE CASE HEADERS.   *
     H*                 4) CHANGED SF1PAG FROM 12 TO 7.              *
     H*                 5) FIXED PAGING.                             *
     H*                                                              *
     H*  08/25/97 C:31  Project: 87MAZ  CO# 48                       *
     H*                 Remove case management fields                *
     H*                 (Compile only)                               *
     H****************************************************************
     FF0279F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFATIP     IF   E             DISK
     F                                     RENAME(PTI01:PFRCDF)
     F*  Primary logical file definition (for displaying items)
     FFATIL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PTI01:DBRCDF)
     FFATRP     IF   E             DISK
     FFMDIL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(9) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(9) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D DL              C                   CONST('* * * DELETED * * *')
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Treatment Plan ICD9 -
     D                                     Code')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0279R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(7)

     I*
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FATIP
     C                   PARM                    @@RRN2            9 0          RRN FATRP
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
     C                   PARM      SCDBRR        ##RRN             9 0          RRN FATIP
     C                   PARM      @@RRN2        ##RRN2            9 0          RRN FATRP
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
     C* MAKE HEADERS PARM LIST
     C*
     C     MKHDPL        PLIST
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
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
     C                   KFLD                    KKTX#
     C                   KFLD                    KKICD9
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C     TRKY          KLIST
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
     C     *LIKE         DEFINE    TICO#         KKCO#
     C     *LIKE         DEFINE    TISUB#        KKSUB#
     C     *LIKE         DEFINE    TIFND         KKFND
     C     *LIKE         DEFINE    TIFYR         KKFYR
     C     *LIKE         DEFINE    TICASE        KKCASE
     C     *LIKE         DEFINE    TIICD9        KKICD9
     C     *LIKE         DEFINE    TILOST        KKLOST
     C     *LIKE         DEFINE    TITX#         KKTX#
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
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN2        CHAIN     PTR01                              99
     C*
     C                   MOVE      TRCO#         KKCO#
     C                   MOVE      TRSUB#        KKSUB#
     C                   MOVE      TRFND         KKFND
     C                   MOVE      TRFYR         KKFYR
     C                   MOVE      TRCASE        KKCASE
     C                   MOVE      TRTX#         KKTX#
     C*
     C                   MOVEL     *LOVAL        KKICD9
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCPST1        KKICD9
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCICD9        KKICD9
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      TRCO#         #1CO#
     C                   MOVE      TRSUB#        #1SUB#
     C                   MOVE      TRFND         #1FND
     C                   MOVE      TRFYR         #1FYR
     C                   MOVE      TRCASE        #1CASE
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
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      TIICD9        SCICD9
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           Z-ADDTIBGDT    SCIBDT
     C*** END   DEL *** 12/02/97 **************************************
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   Z-ADD     TIBGDT        SRDTI
     C*** END   CHG *** 12/02/97 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIBDT
     C                   MOVE      TILOST        SCLOST
     C                   MOVE      TIPRIM        SCPRIM
     C*
     C                   MOVE      TIICD9        KKICD9
     C                   MOVE      TILOST        KKLOST
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDESC
     C   99              MOVEL(P)  *BLANKS       SCDESC
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO                           DARTS
     C                   ENDCS
     C*
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
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1            WC INST
     C                   PARM      ' '           ##MCIF            1            MC INST
     C                   PARM      ' '           ##WCIN            1            WC INTERFACE
     C                   PARM      ' '           ##AR              1            MC AR INST
     C                   PARM      ' '           ##NTWK            1            NETWORK INST
     C                   PARM      ' '           ##UR              1            PRECERT INST
     C                   PARM      ' '           ##MED             1            MEDMGMT INST
     C                   PARM      ' '           ##EDI             1            EDI INST
     C                   PARM      ' '           ##WCDK            1            WC DARTS KEY
     C*
     C*
     C* CREATE HEADERS USING FA702R
     C*
     C                   MOVE      TRCO#         #1CO#
     C                   MOVE      TRSUB#        #1SUB#
     C                   MOVE      TRFND         #1FND
     C                   MOVE      TRFYR         #1FYR
     C                   MOVE      TRCASE        #1CASE
     C*
     C                   CALL      'FA702R'      MKHDPL
     C*
     C* CHAIN OUT TO FATRP FOR BEGIN DATE AND END DATE
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           Z-ADDTRBGDT    SCBGDT
     C**********           Z-ADDSCBGDT    SRDTI
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     TRBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBGDT
     C*
     C                   ENDIF                                                  IN99 = OFF
     C*** END   CHG *** 12/02/97 **************************************
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C* N99                Z-ADDTRENDT    SCENDT
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     TRENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENDT
     C*
     C                   ENDIF                                                  IN99 = OFF
     C*** END   CHG *** 12/02/97 **************************************
     C*
     C                   ENDSR
     C*
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
     C                   MOVE      DL            SCDESC
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
Opt ICD9   D/O  Description                      Prim    Begin date
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=DARTS                YYY
3
