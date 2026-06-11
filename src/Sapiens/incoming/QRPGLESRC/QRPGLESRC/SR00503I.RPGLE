     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SR00503I - ADDITIONAL INPUT INFO FOR WC000503A
      *****************************************************************
      *  PROGRAM DESCRIPTION:                                         *
      *    Prompts the user to enter additional information for       *
      *    endorsement WC000503A - Retrospective Premium Endorsement. *
      *                                                               *
      *  PROGRAMMER: C:78                                             *
      *  CREATED:    11/24/98               PROJECT: CM027            *
      *  REWRITE:    07/07/99  C:113        PROJECT: CM027            *
      *===============================================================*
      *  NOTE:                                                        *
      *  This program collects custom data for endorsements and       *
      *  stores it in file WXFMP2.  The endorsement OV document must  *
      *  use query ENDORSE2, otherwise it won't have the data from    *
      *  this program.                                                *
      *                                                               *
      *===============================================================*
      *  REVISIONS:                                                   *
      *                                                               *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      *===============================================================*
      ** Display File
     FSR00503F  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      ** Company Master File
     FSMCOL     IF   E           K DISK
      ** Group Master File
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
      ** Employer Master File
     FWMEML     IF   E           K DISK
      ** Endorsement workfile
     FWXFMP2    UF A E           K DISK
      *=========================================================================
      *  Data Structure and Constant definitions
      *=========================================================================

     D XXFORM          C                   CONST('WC000503A ')
      *
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      ** External data structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *
      ** Company Master file

     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      ** Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      ** Group Master file
     C     FMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
      *
      ** Endorsement workfile
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *=========================================================================
      *  PLIST definitions
      *=========================================================================
      ** Entry parameters
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
      *
      ** Date check/conversion parameters
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
      *=========================================================================
      *  Main
      *=========================================================================
     C                   Z-ADD     *ZEROS        XXFYR             3 0
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVEL     XXFORM        XXFRM1           15
      *
     C                   Z-ADD     XXFND         SCFND
     C                   Z-ADD     XXEMP#        SCEMP#
     C                   Z-ADD     XXDIV         SCDIV
     C                   Z-ADD     XXFYR         SCFYR
      *
      ** Load the record format information for the first time
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      ** Get the Company record
     C     COKEY         CHAIN     PCO01                              60
      *
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      ** Get the Group record
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
      *
      ** Get the Employer record
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCENAM
      *
     C                   MOVE      Q1USER        SCUSER
      *
      ** Get the Endorsement workfile record
     C     FMKY          CHAIN     PFM02                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      ** Load the screen fields
     C                   EXSR      MVDBSC
      *
     C                   ENDIF
      **************************************************************************
      *  Display all formats
      **************************************************************************
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
      **************************************************************************
      *  Process all Function keys
      **************************************************************************
     C     *IN11         IFEQ      *ON
      *
      ** F3=Exit
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   END
      *
      ** F12=Cancel
     C     *INKL         IFEQ      *ON
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      TERM
     C                   ENDIF                                                  SCR01
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR02
      *
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR03
      *
     C                   ENDIF                                                  INKL=ON
      *
     C                   ENDIF                                                  IN11=ON
      **************************************************************************
      *  End of Function key processing
      **************************************************************************
      *
      ** Goto the appropriate screen
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
      **************************************************************************
      * Process Screen 01
      **************************************************************************
     C     SCR01T        TAG
      *
      ** All edits passed for screen 1, go to next screen
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      **************************************************************************
      * Process Screen 02
      **************************************************************************
     C     SCR02T        TAG
      *
      ** All edits passed for screen 2, go to next screen
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
      **************************************************************************
      * Process Screen 03
      **************************************************************************
     C     SCR03T        TAG
      *
      ** All edits passed for screen 3, write or update record
     C   60              CLEAR                   PFM02
      *
     C                   EXSR      MVSCDB
      *
     C   60              WRITE     PFM02
     C  N60              UPDATE    PFM02
      **************************************************************************
      * End of program processing
      **************************************************************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *=========================================================================
      * MVDBSC - Move database fields to the screen fields
      *=========================================================================
     C     MVDBSC        BEGSR
      *
      ** SCR01 fields
     C                   Z-ADD     F4LLIM        SCLLIM                         LOSS LIMITIATION
     C                   Z-ADD     F4LCON        SCLCON                         LOSS CONV. FACTOR
     C                   Z-ADD     F4MINR        SCMINR                         MIN. RETROSPECTIVE P
     C                   Z-ADD     F4MAXR        SCMAXR                         MAX. RETROSPECTIVE P
      *
     C                   Z-ADD     F4E050        SCE050                         EST. STANDARD PR..50
     C                   Z-ADD     F4E100        SCE100                         EST. STANDARD PR.100
     C                   Z-ADD     F4E150        SCE150                         EST. STANDARD PR.150
     C                   Z-ADD     F4B050        SCB050                         BAS. STANDARD PR..50
     C                   Z-ADD     F4B100        SCB100                         BAS. STANDARD PR.100
     C                   Z-ADD     F4B150        SCB150                         BAS. STANDARD PR.150
      *
      ** SCR02 fields
     C                   MOVEL     F4ST01        SCST1                          STATE 1
     C                   MOVEL     F4ST02        SCST2                          STATE 2
     C                   MOVEL     F4ST03        SCST3                          STATE 3
     C                   MOVEL     F4ST04        SCST4                          STATE 4
     C                   MOVEL     F4ST05        SCST5                          STATE 5
     C                   MOVEL     F4ST06        SCST6                          STATE 6
     C                   MOVEL     F4ST07        SCST7                          STATE 7
     C                   MOVEL     F4ST08        SCST8                          STATE 8
     C                   MOVEL     F4ST09        SCST9                          STATE 9
     C                   MOVEL     F4ST10        SCST10                         STATE 10
      *
     C                   Z-ADD     F4A541        SCA541                         GENERIC AMOUNT 1
     C                   Z-ADD     F4A542        SCA542                         GENERIC AMOUNT 2
     C                   Z-ADD     F4A543        SCA543                         GENERIC AMOUNT 3
     C                   Z-ADD     F4A544        SCA544                         GENERIC AMOUNT 4
     C                   Z-ADD     F4A545        SCA545                         GENERIC AMOUNT 5
     C                   Z-ADD     F4A546        SCA546                         GENERIC AMOUNT 6
     C                   Z-ADD     F4A547        SCA547                         GENERIC AMOUNT 7
     C                   Z-ADD     F4A548        SCA548                         GENERIC AMOUNT 8
     C                   Z-ADD     F4A549        SCA549                         GENERIC AMOUNT 9
     C                   Z-ADD     F45410        SCA54A                         GENERIC AMOUNT 10
     C                   Z-ADD     F45411        SC5411                         GENERIC AMOUNT 11
     C                   Z-ADD     F45412        SC5412                         GENERIC AMOUNT 12
     C                   Z-ADD     F45413        SC5413                         GENERIC AMOUNT 13
     C                   Z-ADD     F45414        SC5414                         GENERIC AMOUNT 14
     C                   Z-ADD     F45415        SC5415                         GENERIC AMOUNT 15
     C                   Z-ADD     F45416        SC5416                         GENERIC AMOUNT 16
     C                   Z-ADD     F45417        SC5417                         GENERIC AMOUNT 17
     C                   Z-ADD     F45418        SC5418                         GENERIC AMOUNT 18
     C                   Z-ADD     F45419        SC5419                         GENERIC AMOUNT 19
     C                   Z-ADD     F45420        SC5420                         GENERIC AMOUNT 20
     C                   Z-ADD     F45421        SC5421                         GENERIC AMOUNT 21
     C                   Z-ADD     F45422        SC5422                         GENERIC AMOUNT 22
     C                   Z-ADD     F45423        SC5423                         GENERIC AMOUNT 23
     C                   Z-ADD     F45424        SC5424                         GENERIC AMOUNT 24
     C                   Z-ADD     F45425        SC5425                         GENERIC AMOUNT 25
     C                   Z-ADD     F45426        SC5426                         GENERIC AMOUNT 26
     C                   Z-ADD     F45427        SC5427                         GENERIC AMOUNT 27
     C                   Z-ADD     F45428        SC5428                         GENERIC AMOUNT 28
     C                   Z-ADD     F45429        SC5429                         GENERIC AMOUNT 29
     C                   Z-ADD     F45430        SC5430                         GENERIC AMOUNT 30
     C                   Z-ADD     F45431        SC5431                         GENERIC AMOUNT 31
     C                   Z-ADD     F45432        SC5432                         GENERIC AMOUNT 32
     C                   Z-ADD     F45433        SC5433                         GENERIC AMOUNT 33
     C                   Z-ADD     F45434        SC5434                         GENERIC AMOUNT 34
     C                   Z-ADD     F45435        SC5435                         GENERIC AMOUNT 35
     C                   Z-ADD     F45436        SC5436                         GENERIC AMOUNT 36
     C                   Z-ADD     F45437        SC5437                         GENERIC AMOUNT 37
     C                   Z-ADD     F45438        SC5438                         GENERIC AMOUNT 38
     C                   Z-ADD     F45439        SC5439                         GENERIC AMOUNT 39
     C                   Z-ADD     F45440        SC5440                         GENERIC AMOUNT 40
      *
      ** SCR03 fields
     C                   Z-ADD     F45441        SC5441                         GENERIC AMOUNT 41
     C                   Z-ADD     F45442        SC5442                         GENERIC AMOUNT 42
     C                   Z-ADD     F45443        SC5443                         GENERIC AMOUNT 43
     C                   Z-ADD     F45444        SC5444                         GENERIC AMOUNT 44
     C                   Z-ADD     F45445        SC5445                         GENERIC AMOUNT 45
     C                   Z-ADD     F45446        SC5446                         GENERIC AMOUNT 46
     C                   Z-ADD     F45447        SC5447                         GENERIC AMOUNT 47
     C                   Z-ADD     F45448        SC5448                         GENERIC AMOUNT 48
     C                   Z-ADD     F45449        SC5449                         GENERIC AMOUNT 49
     C                   Z-ADD     F45450        SC5450                         GENERIC AMOUNT 50
     C                   Z-ADD     F45451        SC5451                         GENERIC AMOUNT 51
     C                   Z-ADD     F45452        SC5452                         GENERIC AMOUNT 52
     C                   Z-ADD     F45453        SC5453                         GENERIC AMOUNT 53
     C                   Z-ADD     F45454        SC5454                         GENERIC AMOUNT 54
     C                   Z-ADD     F45455        SC5455                         GENERIC AMOUNT 55
     C                   Z-ADD     F45456        SC5456                         GENERIC AMOUNT 56
     C                   Z-ADD     F45457        SC5457                         GENERIC AMOUNT 57
     C                   Z-ADD     F45458        SC5458                         GENERIC AMOUNT 58
     C                   Z-ADD     F45459        SC5459                         GENERIC AMOUNT 59
     C                   Z-ADD     F45460        SC5460                         GENERIC AMOUNT 60
     C                   Z-ADD     F45461        SC5461                         GENERIC AMOUNT 61
     C                   Z-ADD     F45462        SC5462                         GENERIC AMOUNT 62
     C                   Z-ADD     F45463        SC5463                         GENERIC AMOUNT 63
     C                   Z-ADD     F45464        SC5464                         GENERIC AMOUNT 64
     C                   Z-ADD     F45465        SC5465                         GENERIC AMOUNT 65
     C                   Z-ADD     F45466        SC5466                         GENERIC AMOUNT 66
     C                   Z-ADD     F45467        SC5467                         GENERIC AMOUNT 67
     C                   Z-ADD     F45468        SC5468                         GENERIC AMOUNT 68
     C                   Z-ADD     F45469        SC5469                         GENERIC AMOUNT 69
     C                   Z-ADD     F45470        SC5470                         GENERIC AMOUNT 70
      *
     C                   ENDSR                                                  MVDBSC
      *=========================================================================
      *  MVSCDB - Move screen fields to database fields
      *=========================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     XXCO#         F4CO#
     C                   Z-ADD     XXFND         F4FND
     C                   Z-ADD     XXFYR         F4FYR
     C                   Z-ADD     XXEMP#        F4EMP#
     C                   Z-ADD     XXDIV         F4DIV
     C                   MOVEL     XXFORM        F4FORM
      *
      ** SCR01 fields
     C                   Z-ADD     SCLLIM        F4LLIM                         LOSS LIMITIATION
     C                   Z-ADD     SCLCON        F4LCON                         LOSS CONV. FACTOR
     C                   Z-ADD     SCMINR        F4MINR                         MIN. RETROSPECTIVE P
     C                   Z-ADD     SCMAXR        F4MAXR                         MAX. RETROSPECTIVE P
      *
     C                   Z-ADD     SCE050        F4E050                         EST. STANDARD PR..50
     C                   Z-ADD     SCE100        F4E100                         EST. STANDARD PR.100
     C                   Z-ADD     SCE150        F4E150                         EST. STANDARD PR.150
     C                   Z-ADD     SCB050        F4B050                         BAS. STANDARD PR..50
     C                   Z-ADD     SCB100        F4B100                         BAS. STANDARD PR.100
     C                   Z-ADD     SCB150        F4B150                         BAS. STANDARD PR.150
      *
      ** SCR02 fields
     C                   MOVEL     SCST1         F4ST01                         STATE 1
     C                   MOVEL     SCST2         F4ST02                         STATE 2
     C                   MOVEL     SCST3         F4ST03                         STATE 3
     C                   MOVEL     SCST4         F4ST04                         STATE 4
     C                   MOVEL     SCST5         F4ST05                         STATE 5
     C                   MOVEL     SCST6         F4ST06                         STATE 6
     C                   MOVEL     SCST7         F4ST07                         STATE 7
     C                   MOVEL     SCST8         F4ST08                         STATE 8
     C                   MOVEL     SCST9         F4ST09                         STATE 9
     C                   MOVEL     SCST10        F4ST10                         STATE 10
      *
     C                   Z-ADD     SCA541        F4A541                         GENERIC AMOUNT 1
     C                   Z-ADD     SCA542        F4A542                         GENERIC AMOUNT 2
     C                   Z-ADD     SCA543        F4A543                         GENERIC AMOUNT 3
     C                   Z-ADD     SCA544        F4A544                         GENERIC AMOUNT 4
     C                   Z-ADD     SCA545        F4A545                         GENERIC AMOUNT 5
     C                   Z-ADD     SCA546        F4A546                         GENERIC AMOUNT 6
     C                   Z-ADD     SCA547        F4A547                         GENERIC AMOUNT 7
     C                   Z-ADD     SCA548        F4A548                         GENERIC AMOUNT 8
     C                   Z-ADD     SCA549        F4A549                         GENERIC AMOUNT 9
     C                   Z-ADD     SCA54A        F45410                         GENERIC AMOUNT 10
     C                   Z-ADD     SC5411        F45411                         GENERIC AMOUNT 11
     C                   Z-ADD     SC5412        F45412                         GENERIC AMOUNT 12
     C                   Z-ADD     SC5413        F45413                         GENERIC AMOUNT 13
     C                   Z-ADD     SC5414        F45414                         GENERIC AMOUNT 14
     C                   Z-ADD     SC5415        F45415                         GENERIC AMOUNT 15
     C                   Z-ADD     SC5416        F45416                         GENERIC AMOUNT 16
     C                   Z-ADD     SC5417        F45417                         GENERIC AMOUNT 17
     C                   Z-ADD     SC5418        F45418                         GENERIC AMOUNT 18
     C                   Z-ADD     SC5419        F45419                         GENERIC AMOUNT 19
     C                   Z-ADD     SC5420        F45420                         GENERIC AMOUNT 20
     C                   Z-ADD     SC5421        F45421                         GENERIC AMOUNT 21
     C                   Z-ADD     SC5422        F45422                         GENERIC AMOUNT 22
     C                   Z-ADD     SC5423        F45423                         GENERIC AMOUNT 23
     C                   Z-ADD     SC5424        F45424                         GENERIC AMOUNT 24
     C                   Z-ADD     SC5425        F45425                         GENERIC AMOUNT 25
     C                   Z-ADD     SC5426        F45426                         GENERIC AMOUNT 26
     C                   Z-ADD     SC5427        F45427                         GENERIC AMOUNT 27
     C                   Z-ADD     SC5428        F45428                         GENERIC AMOUNT 28
     C                   Z-ADD     SC5429        F45429                         GENERIC AMOUNT 29
     C                   Z-ADD     SC5430        F45430                         GENERIC AMOUNT 30
     C                   Z-ADD     SC5431        F45431                         GENERIC AMOUNT 31
     C                   Z-ADD     SC5432        F45432                         GENERIC AMOUNT 32
     C                   Z-ADD     SC5433        F45433                         GENERIC AMOUNT 33
     C                   Z-ADD     SC5434        F45434                         GENERIC AMOUNT 34
     C                   Z-ADD     SC5435        F45435                         GENERIC AMOUNT 35
     C                   Z-ADD     SC5436        F45436                         GENERIC AMOUNT 36
     C                   Z-ADD     SC5437        F45437                         GENERIC AMOUNT 37
     C                   Z-ADD     SC5438        F45438                         GENERIC AMOUNT 38
     C                   Z-ADD     SC5439        F45439                         GENERIC AMOUNT 39
     C                   Z-ADD     SC5440        F45440                         GENERIC AMOUNT 40
      *
      ** SCR03 fields
     C                   Z-ADD     SC5441        F45441                         GENERIC AMOUNT 41
     C                   Z-ADD     SC5442        F45442                         GENERIC AMOUNT 42
     C                   Z-ADD     SC5443        F45443                         GENERIC AMOUNT 43
     C                   Z-ADD     SC5444        F45444                         GENERIC AMOUNT 44
     C                   Z-ADD     SC5445        F45445                         GENERIC AMOUNT 45
     C                   Z-ADD     SC5446        F45446                         GENERIC AMOUNT 46
     C                   Z-ADD     SC5447        F45447                         GENERIC AMOUNT 47
     C                   Z-ADD     SC5448        F45448                         GENERIC AMOUNT 48
     C                   Z-ADD     SC5449        F45449                         GENERIC AMOUNT 49
     C                   Z-ADD     SC5450        F45450                         GENERIC AMOUNT 50
     C                   Z-ADD     SC5451        F45451                         GENERIC AMOUNT 51
     C                   Z-ADD     SC5452        F45452                         GENERIC AMOUNT 52
     C                   Z-ADD     SC5453        F45453                         GENERIC AMOUNT 53
     C                   Z-ADD     SC5454        F45454                         GENERIC AMOUNT 54
     C                   Z-ADD     SC5455        F45455                         GENERIC AMOUNT 55
     C                   Z-ADD     SC5456        F45456                         GENERIC AMOUNT 56
     C                   Z-ADD     SC5457        F45457                         GENERIC AMOUNT 57
     C                   Z-ADD     SC5458        F45458                         GENERIC AMOUNT 58
     C                   Z-ADD     SC5459        F45459                         GENERIC AMOUNT 59
     C                   Z-ADD     SC5460        F45460                         GENERIC AMOUNT 60
     C                   Z-ADD     SC5461        F45461                         GENERIC AMOUNT 61
     C                   Z-ADD     SC5462        F45462                         GENERIC AMOUNT 62
     C                   Z-ADD     SC5463        F45463                         GENERIC AMOUNT 63
     C                   Z-ADD     SC5464        F45464                         GENERIC AMOUNT 64
     C                   Z-ADD     SC5465        F45465                         GENERIC AMOUNT 65
     C                   Z-ADD     SC5466        F45466                         GENERIC AMOUNT 66
     C                   Z-ADD     SC5467        F45467                         GENERIC AMOUNT 67
     C                   Z-ADD     SC5468        F45468                         GENERIC AMOUNT 68
     C                   Z-ADD     SC5469        F45469                         GENERIC AMOUNT 69
     C                   Z-ADD     SC5470        F45470                         GENERIC AMOUNT 70
      *
     C                   ENDSR                                                  MBSCDB
