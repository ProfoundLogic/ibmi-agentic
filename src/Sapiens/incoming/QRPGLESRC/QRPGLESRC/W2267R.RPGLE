     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W2267R - CAMPUS CODE  MAINTENANCE
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM SEARCHES ALL EXISTING CAMPUS CODE TABLE RECORDS  *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CONVERTED TO WORK WITH 11/08/98                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/04/10 C:190 PROJECT: 25WRE                                *
     H*                 Added pgm  to current release.                *
     H*                                                               *
     H*  07/16/03 C:136 Project: 25WNL                                *
     H*                 Add Option 8=Policy Years.                    *
     H*                                                               *
     H*  11/05/01 C:062 Support WC025 29504                           *
     H*                 Add logic for status subset.                  *
     H*                                                               *
     H*  11/08/98 C:112 PROJECT 25WJZ                                 *
     H*                 Claim Maintenance: Campus Code                *
     H*                 Reimplementation of Project 25WGZ             *
     H*                 Claim Campus Code / Postal Zip Link           *
     H*                 This migrates this program to a work with     *
     H*                 program                                       *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW2267F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMCLP     IF   E             DISK
     F                                     RENAME(PCL01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMCLL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCL01:DBRCDF)
     FWMFNL     IF   E           K DISK
     FWMEMP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D D21             S              1    DIM(74)
     D D22             S              1    DIM(74)
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headin
      *** BEG CHG ***** 07/16/03 *************************************
     D******************* OF      1  11  1   OFD    35   Options, Func
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Func
     D OFD             S             35    DIM(12) ALT(OF)
      *** END CHG ***** 07/16/03 *************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Campus Code Maintena-
     D                                     nce')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2267R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKCMPS
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CLCO#         KKCO#
     C     *LIKE         DEFINE    CLFND         KKFND
     C     *LIKE         DEFINE    CLEMP#        KKEMP#
     C     *LIKE         DEFINE    CLDIV         KKDIV
     C     *LIKE         DEFINE    CLCMPS        KKCMPS
     C     *LIKE         DEFINE    CLCO#         @@CO#
     C     *LIKE         DEFINE    CLFND         @@FND
     C     *LIKE         DEFINE    CLEMP#        @@EMP#
     C     *LIKE         DEFINE    CLDIV         @@DIV
     C     *LIKE         DEFINE    CLEMP#        ##EMP#
     C     *LIKE         DEFINE    CLDIV         ##DIV
     C     *LIKE         DEFINE    CLCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLEMP#        XXEMP#
     C     *LIKE         DEFINE    CLDIV         XXDIV
     C     *LIKE         DEFINE    CLCMPS        XXCMPS
     C     *LIKE         DEFINE    CLDESC        XXDESC
     C     *LIKE         DEFINE    CLSTAT        XXSTAT
     C     *LIKE         DEFINE    CLUSER        XXUSER
     C     *LIKE         DEFINE    CLPGM         XXPGM
     C     *LIKE         DEFINE    CLTIME        XXTIME
     C     *LIKE         DEFINE    CLADDR        XXADDR
     C     *LIKE         DEFINE    CLCCTY        XXCCTY
     C     *LIKE         DEFINE    CLCST         XXCST
     C     *LIKE         DEFINE    CLCZIP        XXZIP
     C     *LIKE         DEFINE    CLDESC        XXNAME
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*** BEGIN ADD *** 11/05/01 **************************************
     C     SSSTAT        IFNE      *BLANKS
     C                   SETON                                        11
     C                   ENDIF
     C*** END   ADD *** 11/05/01 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C     SCSEL         IFEQ      '4'
     C                   MOVE      *ON           AUTORF
     C                   ENDIF
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
     C*** BEGIN ADD *** 11/05/01 **************************************
     C     @@CALL        IFEQ      '2'
     C                   MOVEL(P)  'A'           SSSTAT
     C                   ENDIF
     C*** END   ADD *** 11/05/01 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C     @@RRN         CHAIN     PEM01                              99
     C                   MOVE      EMCO#         KKCO#
     C                   MOVE      EMFND         KKFND
     C                   MOVE      EMEMP#        KKEMP#
     C                   MOVE      EMDIV         KKDIV
     C                   MOVE      *LOVAL        KKCMPS
     C                   MOVE      EMCO#         ##CO#
     C                   MOVE      EMFND         ##FND
     C                   MOVE      EMEMP#        ##EMP#
     C                   MOVE      EMDIV         ##DIV
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKCMPS
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL(P)  SCCMPS        KKCMPS
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     CLCMPS        SCCMPS
     C                   MOVEA     CLCMPS        D21(05)
     C                   MOVEA     CLDESC        D21(14)
     C                   MOVEA     CLSTAT        D21(57)
     C                   MOVEA     D21           SCDATA
     C*
     C                   ENDSR
      *** BEG ADD ***** 07/16/03 **************************************
     C*================================================================
     C* OPT08 - Policy years
     C*================================================================
     C     OPT08         BEGSR
     C*
     C                   CALL      'W2268R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR
      *** END ADD ***** 07/16/03 **************************************
     C*================================================================
     C* OPT20 - Process option 20
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
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
      *** BEG ADD ***** 07/16/03 **************************************
     C     SCSEL         CASEQ     '8'           OPT08
      *** END ADD ***** 07/16/03 **************************************
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      **** BEGIN ADD *** 11/08/98 ************************************
     C     @@CALL        IFEQ      '2'
     C     OF#           IFEQ      1
     C                   MOVE      SCDBRR        @@RRN
     C                   ELSE
     C                   MOVE      *ZERO         @@RRN
     C                   ENDIF
     C                   ENDIF
      **** END ADD ***** 11/08/98 ***********************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   MOVE      @@FND         SCFND
     C     FNKY          CHAIN     PFN01                              99
     C                   MOVEL     FNNAME        SCFNDN
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN     PEM01                              99
     C                   ENDIF
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      @@DIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
     C                   MOVEA(P)  OFD           ##OFD
     C                   MOVE      EMFND         SCFND
     C*
     C     KEY1          SETLL     DBRCDF                             96
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
     C   21KEY1A         SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
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
     C*** BEGIN ADD *** 11/05/01 **************************************
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CLSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 11/05/01 **************************************
     C                   ENDSR
** Column headings
Opt  Campus Code  Description                              Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1088=Policy years           YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
