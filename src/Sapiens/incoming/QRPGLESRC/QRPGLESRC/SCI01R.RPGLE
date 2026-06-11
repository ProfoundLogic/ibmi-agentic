     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.                *
     H* PROGRAM: SC150R  CREATED: 04/03/97   C:45                    *
     H* DESCRIPTION: IMAGES SEARCH BY DOCUMENT NAME                  *
     H*                                                              *
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 10/17/11 C:277 Project: SY195                                *
     H*                QDLS migration to IFS                         *
     H*                Add two parameters with command               *
     H*                                                              *
     H* 01/04/00 C:113 PROJECT: SUPPORT                              *
     H*                Corrected date format conversion.             *
     H*                                                              *
     H****************************************************************
      ** DISPLAY FILE
     FSCI01F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      ** DARTS FILE PHY
     FSDCMP     IF   E             DISK
     F                                     RENAME(PCM01:PFRCDF)
      ** DARTS FILE LF
     FSDCML6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCM01:DBRCDF)
      ** DARTS FILE LF
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM0A)
      ** APPL SYSTEM CONTROL FILE
     FSSYLF     IF   E           K DISK
      ** SECURITY FILE
     FSMSLL     IF   E           K DISK
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column Headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
      ** /COPY required constants
      *
     D OPTNM           C                   CONST('Image Documents')
     D ACDPGM          C                   CONST('SCI01R02')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SCMESG')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D SF1PAG          C                   CONST(10)
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *

     C                   TIME                    DSDTTM
      *
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMCDAT        XXCDAT
     C     *LIKE         DEFINE    DRAL02        OFCPGM
     C     *LIKE         DEFINE    CMKEY         #11KY
     C     *LIKE         DEFINE    CMSEQ#        #11SQ#
     C     *LIKE         DEFINE    CMKEY         #13KY
     C     *LIKE         DEFINE    CMKEY         #17KY
     C     *LIKE         DEFINE    CMSEQ#        #17SQ#
     C     *LIKE         DEFINE    CMPNAM        #17PNM
     C     *LIKE         DEFINE    CMKEY         #18KY
     C     *LIKE         DEFINE    CMSEQ#        #18SQ#
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
      *
      *** BEGIN ADD *** 01/04/00 ***********************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** END   ADD *** 01/04/00 ***********************************************
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
      **  KEY LIST FOR THE COMPANY FILE
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      **  KEY LIST FOR SUBFILE
      *
     C     KEY1          KLIST
     C                   KFLD                    XXIDOC           12
      *
      **  KEY LIST FOR THE SYSTEM CTL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      **  KEY LIST FOR DARTS FILE
      *
     C     CMKEYA        KLIST
     C                   KFLD                    #11KY
     C                   KFLD                    #11SQ#
      *
      **  KEY LIST FOR THE DARTS SECURITY FILE
      *
     C     SLKEY         KLIST
     C                   KFLD                    Q1USER
      *
     C/COPY SCOPYWW01C
      *
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
     C                   MOVE      *LOVAL        XXIDOC
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C     SCPST1        IFGT      *BLANKS
     C                   MOVE      SCPST1        XXIDOC
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SCIDOC        XXIDOC
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
      *
     C     MVDBSC        BEGSR
      *
     C                   MOVEL     CMKEY         SCKEY
     C                   MOVEL     CMRMCD        SCRMCD
     C                   MOVE      CMSEQ#        SCSEQ#
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C********** CMCDAT    MULT 100.0001  XXCDAT
     C**********           MOVE XXCDAT    SCCDAT
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
      *** END   CHG *** 01/04/00 ***********************************************
     C                   MOVEL     CMDCTP        SCDCTP
     C                   MOVEL     CMDCID        SCDCID
     C                   MOVEL     CMUSER        SCIUSR
     C                   MOVEL     CMCOMM        SCCOMM
     C                   MOVE      CMIMGF        SCIMGF
     C                   MOVE      CMFOLD        SCFOLD
     C                   MOVE      CMIDOC        SCIDOC
      *
     C                   ENDSR
      *
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
      *
     C     OTHFKS        BEGSR
      *
      *******************************
      * F1 OV/400                   *
      *******************************
      *
     C     *INKA         IFEQ      '1'
      *
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *BLANKS       OFCPGM
      *
      ** GET OFFICE CLP PROGRAM NAME FROM SDRPF/DRAL02
      *
     C                   MOVEL     'SC'          XXSYK
      *
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRAL02        OFCPGM
     C                   ENDIF
      *
     C                   CALL      OFCPGM
     C                   PARM                    QQRTCD
      *
     C                   ENDIF                                                  END *INKA
      *
     C                   ENDSR
      *
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
      *
     C     OTHOPT        BEGSR
      ************************************
      * OPTION 11 MARK AS COMPLETED      *
      ************************************
      *
     C     SCSEL         IFEQ      '11'
      *
      ** IF IMAGE ENABLED AND IS IMAGE REMARK, CALL PGM TO REQUE 1ST
      *
     C                   MOVE      SCKEY         #11KY
     C                   MOVE      SCSEQ#        #11SQ#
     C     CMKEYA        CHAIN     PCM0A                              40
     C     *IN40         IFEQ      '0'
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C********** DSDATE    MULT 10000.01  CMDCDT           DIARY COMPLTD DATE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTCYMD
     C                   MOVEL     SRDTO         CMDCDT
     C                   MOVEL     SRDTO         CMDT
      *
     C                   MOVEL     Q1PGM         CMPGM                          MAINT INFO
     C                   Z-ADD     DSTIME        CMTIME                             "
     C********** DSDATE    MULT 10000.01  CMDT                 "
      *** END   CHG *** 01/04/00 ***********************************************
     C                   UPDATE    PCM0A
     C                   ENDIF                                                  END 0 *IN40
      *
     C                   ENDIF                                                  END SEL EQ '11'
      *
      ************************************
      * OPTION 12 EDIT DOC               *
      ************************************
      *
     C     SCSEL         IFEQ      '12'
      *
     C                   MOVEA     '00000'       *IN(71)
      *
     C     SLKEY         CHAIN     PSL01                              40
     C     *IN40         IFEQ      '0'
     C     SLEDC         IFNE      'Y'
     C     SCIMGF        ANDNE     'Y'                                          not if image
     C                   MOVE      '1'           *IN71                          NOT ALLOWED TO EDIT
     C                   MOVE      '1'           *IN90
      *
     C                   GOTO      ENDO12
     C                   ENDIF                                                  END NE 'Y'
     C                   ENDIF                                                  END 0 *IN40
      *
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD
     C                   CALL      'SEDTDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN73
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO12
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN74
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO12
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN75
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO12
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN76
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO12
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C     ENDO12        TAG
      *
     C                   ENDIF                                                  END SEL EQ '12'
      *
      ************************************
      * OPTION 13 DIARY ANOTHER USER     *
      ************************************
      *
     C     SCSEL         IFEQ      '13'
      *
     C                   MOVEL     CMKEY         #13KY
     C                   MOVE      SCSEQ#        #13SQ#            5
     C                   MOVEL     *BLANK        QQRTCD
     C                   CALL      'SC016R'
     C                   PARM                    #13KY
     C                   PARM                    #13SQ#
     C                   PARM                    QQRTCD
      *
     C                   ENDIF                                                  END SEL EQ '13'
      *
      ************************************
      * OPTION 15 VIEW DOC               *
      ************************************
      *
     C     SCSEL         IFEQ      '15'
      *
     C                   MOVEA     '00000'       *IN(71)
      *
     C     SLKEY         CHAIN     PSL01                              40
     C     *IN40         IFEQ      '0'
     C     SLDDC         IFNE      'Y'
     C                   MOVE      '1'           *IN21                          NOT ALLOWED TO DISP
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO15
     C                   ENDIF
     C                   ENDIF
      *
     C     SCIMGF        IFNE      'Y'                                          IF NOT AN IMAGE TYPE
      *
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     SCKEY         XXKY             40
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SDSPDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C                   PARM                    QQRTCD
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       XXMS              1
     C                   PARM      *BLANKS       XXUNCN          255
     C                   PARM      *BLANKS       XXFSF             1
     C*** End   Add *** 10/17/11 **************************************
      *
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO15
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO15
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO15
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO15
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVEL     SCIDOC        QVDCTB          120
     C                   MOVEL     SCSEQ#        XXSQ#
     C                   MOVEL     Q1USER        QVUSER           10
     C                   MOVEL     Q1JOB         QVTRID           10
     C                   MOVEL     *BLANK        QVRTCD            6
     C                   CALL      'SCIMGV'                             60
     C                   PARM      SCIDOC        XXDCID           12
     C                   PARM      SCFOLD        XXFOLD
     C                   PARM      SCKEY         XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM      *BLANKS       QVRTCD
     C**************       PARM           QVDCTB
     C*                    PARM           QVUSER
     C*                    PARM           QVTRID
     C***********          PARM           QVRTCD
      *
     C     QVRTCD        IFNE      '     0'
     C     QVRTCD        ANDNE     '000000'
     C     QVRTCD        ANDNE     '      '
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO15
     C                   ENDIF
     C                   ENDIF
      *
     C     ENDO15        TAG
      *
     C                   ENDIF                                                  END SEL EQ '15'
      *
      ************************************
      * OPTION 16 PRINT DOC              *
      ************************************
      *
     C     SCSEL         IFEQ      '16'
      *
     C     SCIMGF        IFNE      'Y'                                          NOT AN IMAGE
      *
      ** IF AS/400 OFFICE NOT INSTALLED - DON'T ALLOW THIS FUNCTION
      ** CHECK SYSTEM CONTROL FILE INSTALL FLAGS FOR THIS CLIENT:
      *
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C     SYOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO16
     C                   ENDIF
      *
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     SCKEY         XXKY             40
     C                   MOVEL     *BLANK        QQRTCD
     C                   CALL      'SPRTDOC'                            60
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN73
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO16
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN74
     C                   MOVE      '1'           *IN90             1
     C                   GOTO      ENDO16
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN75
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO16
     C                   ELSE
      *
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN76
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO16
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ELSE                                                   IS AN IMAGE
      *
     C                   MOVEL     SCIDOC        QPDCID           12
     C                   MOVEL     Q1USER        QPUSER           10
     C                   MOVEL     Q1JOB         QPTRID           10
     C                   MOVEL     *BLANK        QPRTCD            6
      *
     C                   CALL      'SCIMGP'                             60
     C                   PARM                    QPDCID
     C                   PARM                    QPUSER
     C                   PARM                    QPTRID
     C                   PARM                    QPRTCD
      *
     C     QPRTCD        IFNE      '000000'                                       ERROR OCCURED
     C     QPRTCD        ANDNE     '      '                                       ERROR OCCURED
     C     QPRTCD        ANDNE     '     0'                                       ERROR OCCURED
     C                   MOVE      '1'           *IN26                              IMAGE
     C                   MOVE      '1'           *IN90
     C                   GOTO      ENDO16
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C     ENDO16        TAG
      *
     C                   ENDIF                                                  END SEL EQ '16'
      *
      ************************************
      * OPTION 17 ADDITIONAL REMARKS     *
      ************************************
      *
     C     SCSEL         IFEQ      '17'
      *
     C                   MOVEL     CMKEY         #17KY
     C                   MOVE      SCSEQ#        #17SQ#
     C                   MOVEL     CMPNAM        #17PNM
     C                   CALL      'SC014R'
     C                   PARM                    #17KY
     C                   PARM                    #17SQ#
     C                   PARM                    #17PNM
      *
     C                   ENDIF                                                  END SEL EQ '17'
      *
      ************************************
      * OPTION 18 VIEW REMARK DETAIL     *
      ************************************
      *
     C     SCSEL         IFEQ      '18'
      *
     C                   MOVEL     CMKEY         #18KY
     C                   MOVE      SCSEQ#        #18SQ#
     C                   MOVEL     *BLANK        QQRTCD
     C                   CALL      'SC012R'
     C                   PARM                    #18KY
     C                   PARM                    #18SQ#
     C                   PARM                    QQRTCD
      *
     C                   ENDIF                                                  END SEL EQ '18'
      *
      ************************************
      * OPTION 99 PRINT REMARK           *
      ************************************
      *
     C     SCSEL         IFEQ      '99'
      *
     C                   MOVE      SCSEQ#        #99SQ#            5
     C                   MOVEL     *BLANK        QQRTCD
     C                   MOVE      *BLANK        #99QUE           10
     C                   MOVEL     'QPRINT'      #99QUE
     C                   MOVE      'D'           #99MOD            1
     C                   CALL      'SC741C'
     C                   PARM                    CMKEY
     C                   PARM                    #99SQ#
     C                   PARM                    #99QUE
     C                   PARM                    #99MOD
      *
     C                   ENDIF
      *
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column Headings
Opt Seq #  Summ Code   Date      Type     Document ID        Diary User
** Description--------------MISN
11111=Mark Completed        NYN
11212=Edit Doc              NYN
11313=Diary Other User      NYN
11515=View Doc              NYN
11616=Print Doc             NYN
11717=Additional Remarks    NYN
11818=View Rmk Detail       NYN
19999=Print Remark          NYN
201F1=OV/400                NYN
203F3=Exit                  NYN
205F5=Refresh               NYN
215F15=Fold/Unfold          NYN
3
