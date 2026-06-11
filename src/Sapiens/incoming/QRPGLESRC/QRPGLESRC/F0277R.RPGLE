     H* (c) 2012 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* PROGRAM: F0277R
     H* DESCRIPTION: WORK WITH PROVIDERS/SPECIALISTS
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H*4/13/12 C:277 Project: 114MAW                              *
     H*               Option to Enter Multiple dates of treatment  *
     H*               Additional changes                           *
     H*                                                              *
     H*  01/31/08 C:62  Project: MC114 59729                         *
     H*                 BRING BACK TO BASE.                          *
     H*                                                              *
     H*  09/24/01 C:62  Project: MC248                               *
     H*                 IF CALLED FROM PRE-CERT INFO, PARM PASSED    *
     H*                 IN TO MAKE SURE ONLY CASE PROVIDERS WITH     *
     H*                 FEIN <> 0 TO BE DISPLAYED                    *
     H*                                                              *
     H*  07/14/98 C:61  Project: 98MAY                               *
     H*                 Validate provider types 1 and 4              *
     H*                 only.                                        *
     H*                                                              *
     H*  06/09/98 C:94  PROJECT: MC145                               *
     H*                 ADDED MOD TO POSITION TO FIELD.              *
     H*                                                              *
     H*  02/01/98 C:62  PROJECT: MC145                               *
     H*                 1) CHANGED OPTNM TO 'CASE PROVIDERS'.        *
     H*                 2) ADDED FA703R AND F14=DARTS.               *
     H*                 3) ADDED SA001R.                             *
     H*                 4) OPT7 NOW USES F0032R01.                   *
     H*                 5) SUBSET MESSAGE ADDED.                     *
     H*                                                              *
     H* 01/08/98 C:86 PROJECT: MC145                                 *
     H*               DISPLAY FILE F0277F HAS BEEN CHANGED TO PRINT  *
     H*               A 2 LINE SUB-HEADING. THIS PROGRAM CALLS FA702R*
     H*               TO PRINT IT & USES LIKE DEFINES RO REDEFINE THE*
     H*               OMMITTED SCREEN FIELDS.                        *
     H*                                                              *
     H*  12/18/97 C:86                                               *
     H*      Remove references to SSSTAT, duplicate to SSSTS         *
     H*                                                              *
     H*  08/25/97 C:31  Project: 87MAZ  CO# 48                       *
     H*                 Remove case management fields                *
     H*                 (Compile only)                               *
     H****************************************************************
     FF0277F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAPPP     IF   E             DISK
     F                                     RENAME(PPP01:PFRCDF)
     F*  Primary logical file definition (for displaying items)
     FFAPPL5    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPP01:DBRCDF)
     FFMCMP     IF   E             DISK
     F*FMPDL   IF  E           K        DISK
     FFMPDL3    IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D*******Beg Del********04/13/12**********************************
     D*/COPY SCOPYWW01E
     D*******End Del********04/13/12**********************************
     D*
     D*
     D*******Beg Del********04/13/12**********************************
     D*/COPY SCOPYWW01I
     D*******End Del********04/13/12**********************************
     D*
     D                 DS
     D  DATA1                  1     56
     D  SCNAME                 1     30
     D  SCSPCD                34     36
     D  SCPCP                 41     41
     D  SCWCCE                46     46
     D  SCRSTR                51     51
     D  SCSTS                 56     56
     D                 DS
     D  DATA2                  1     56
     D  SCNAM2                 1     26
     D  SCTSIT                29     38
     D  SCTCUR                41     41
     D  SCSPC2                45     47
     D  SCPCP2                51     51
     D  SCRST2                55     55
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Case Providers')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0277R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D*
     D* Program specific constants
     D*
     D DL              C                   CONST('* * * DELETED * * *')
     D*******Beg Add********04/13/12**********************************
     D #OFSCR          S              1  0
     D*******End Add********04/13/12**********************************
     C*******Beg Add********04/13/12**********************************
     C****/COPY SCOPYWW01C
     C*******End Add********04/13/12**********************************
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          FAPPP
     C                   PARM                    @@RRN2            9 0          FMCMP
     C                   PARM                    @@MODE            1
     C*
     C*******Beg Del********04/13/12**********************************
     C***  DTCYMD        PLIST
     C***                PARM                    SRDTI
     C***                PARM      *BLANKS       SRFFMT
     C***                PARM      '*CYMD'       SRTFMT
     C***                PARM                    SRDTO
     C***                PARM                    SRERR
     C*******End Del********04/13/12**********************************
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
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PPCO#         KKCO#
     C     *LIKE         DEFINE    PPSUB#        KKSUB#
     C     *LIKE         DEFINE    PPFND         KKFND
     C     *LIKE         DEFINE    PPFYR         KKFYR
     C     *LIKE         DEFINE    PPFEIN        KKFEIN
     C     *LIKE         DEFINE    PPMOD         KKMOD
     C     *LIKE         DEFINE    PPCASE        KKCASE
     C     *LIKE         DEFINE    PPSEQ#        KKSEQ#
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTS         IFNE      *BLANKS
     C     SSRSTR        ORNE      *BLANKS
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
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     *IN90         CABEQ     '1'           ERRSTP
     C*
     C* CHECK ACTIVE/INACTIVE
     C*
     C     SSSTS         IFNE      *BLANK
     C     SSSTS         ANDNE     'A'
     C     SSSTS         ANDNE     'I'
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C* CHECK RESTRICTIONS EQUAL TO Y/N
     C*
     C     SSRSTR        IFNE      *BLANK
     C     SSRSTR        ANDNE     'Y'
     C     SSRSTR        ANDNE     'N'
     C                   SETON                                        9075
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C* CHECK PRIM CARE EQUAL TO Y/N
     C*
     C*
     C     ERRSTP        ENDSR
     C*******Beg Add********04/13/12**********************************
     C*================================================================
     C* ERRCK3 - Used to determine errors on the third screen
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK4 - Used to determine errors on the fourth screen
     C*================================================================
     C     ERRCK4        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK5 - Used to determine errors on the fifth screen
     C*================================================================
     C     ERRCK5        BEGSR
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Used for any processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Used to update data base record
     C*================================================================
     C     UPDATR        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRITER - Used to write data base record
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - After displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHERR - Used for processing other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*******End Add********04/13/12**********************************
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
     C                   MOVE      *LOVAL        KKFEIN
     C                   MOVE      *LOVAL        KKMOD
     C                   MOVE      *LOVAL        KKSEQ#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCPST1        KKFEIN
     C                   MOVEL     SCPSTA        KKMOD
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCSEQ#        KKSEQ#
     C                   MOVEL     SCFEIN        KKFEIN
     C                   MOVEL     SCMOD         KKMOD
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
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
     C* GET ONLY ACTIVE RECORDS
     C*
     C                   MOVE      PPFEIN        SCFEIN
     C                   MOVE      PPMOD         SCMOD
     C                   MOVE      PPMOD         SCMODC
     C*
     C     SCFEIN        IFEQ      *ZEROS
     C*
     C                   SETON                                        16
     C                   MOVEL     PP40A1        SCNAME
     C                   MOVEL     PP40A1        SCNAM2
     C                   MOVE      *BLANKS       SCMODC
     C*
     C                   ENDIF
     C*
     C                   MOVE      PPSPCD        SCSPCD
     C                   MOVE      PPSTS         SCSTS
     C                   MOVE      PPRSTR        SCRSTR
     C                   MOVE      PPPCP         SCPCP
     C                   MOVE      PPSEQ#        SCSEQ#
     C                   MOVE(P)   PPSPCD        SCSPC2
     C                   MOVE(P)   PPRSTR        SCRST2
     C                   MOVE(P)   PPPCP         SCPCP2
     C                   MOVEL(P)  PPTSIT        SCTSIT
     C                   MOVEL(P)  PPTCUR        SCTCUR
     C*
     C                   MOVE      PPFEIN        KKFEIN
     C                   MOVE      PPMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C  N99              MOVEL     PDWCCE        SCWCCE
     C   99              MOVE      *BLANKS       SCWCCE
     C  N99              MOVEL     PDNAME        SCNAME
     C   99
     CANN16              MOVE      *BLANKS       SCNAME
     C  N99              MOVEL     PDNAME        SCNAM2
     C   99
     CANN16              MOVE      *BLANKS       SCNAM2
     C*
     C     *IN17         IFEQ      *OFF
     C                   MOVEL(P)  HD(1)         SCHDL1
     C                   MOVEL(P)  DATA1         SCDATA
     C                   ELSE
     C                   MOVEL(P)  HD(2)         SCHDL1
     C                   MOVEL(P)  DATA2         SCDATA
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
     C* OPT7 - OPTION 7 - PROVIDER MAINTENANCE
     C*================================================================
     C     OPT7          BEGSR
     C*
     C     SCFEIN        IFNE      *ZEROS
     C*
     C*  CALL API TO OBTAIN FMPDP RRN
     C*
     C                   MOVE      SCFEIN        X1FEIN            9
     C                   MOVE      SCMOD         X1MOD             3
     C*
     C                   CALL      'FA006R'
     C                   PARM                    X1FEIN
     C                   PARM                    X1MOD
     C                   PARM      *ZEROS        ##RRN             9 0          FMPDP RRN
     C*
     C*  CALL PROVIDER MAINTENANCE IN DISPLAY MODE
     C*
     C                   CALL      'F0032R01'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C                   ELSE
     C*
     C                   SETON                                        8090
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7                           PROVIDER MAINT
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
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *
     C                   SETOFF                                       17
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C     ##CLT#        IFEQ      '114'
     C                   SETON                                        17
     C                   MOVEL(P)  HD(2)         SCHDL1
     C*** Beg Add *** 04/13/12 *********************************************
     C                   If        %parms > 5
     C*
     C                   call      'F0277R1'
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn                          FAPPP
     C                   parm                    @@rrn2                         FMCMP
     C                   parm                    @@mode
     C                   Else
     C*
     C                   call      'F0277R1'
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn                          FAPPP
     C                   parm                    @@rrn2                         FMCMP
     C*
     C                   Endif
     C*
     C                   exsr      pgmend
     C                   return
     C*** End Add *** 04/13/12 *********************************************
     C                   ENDIF
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
     C*
     C                   MOVE      DL            SCNAME
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
     C*
     C* IF MODE IS PASSED IN AND IS 'Y', ONLY DISPLAY PROVIDERS WITH
     C* FEDERAL ID#S
     C*
     C     Q1PRMS        IFGT      5
     C     @@MODE        IFEQ      'Y'
     C     PPFEIN        IFEQ      *ZEROS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSSTS         IFNE      *BLANK
     C     SSSTS         ANDNE     PPSTS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSRSTR        IFNE      *BLANK
     C     SSRSTR        ANDNE     PPRSTR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*
     C                   ENDSR
** Column headings
Opt FEIN#       Mod  Name                            Code   Y/N Cert  Y/N  A/I
Opt FEIN#       Mod  Name                        Tx site    Site Code Y/N Y/N
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Provider Inquiry       YYN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=DARTS                YYY
217F17=Subset               YYY
3
