     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC330R01 - KEY DEFINITIONS UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays the KEY DEFINITIONS   **
     H**     codes file.                                             **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC330R - Work with KEY DEFINITIONS                      **
     H**   CALLS TO:                                                 **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   01/19/98      PROJECT: DR052                   **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/27/00 C:113 PROJECT: DR095                                 *
     H*                Changed to use the Work With Key Types program *
     H*                (SC092R) for key type searches.                *
     H*                                                               *
     H* 12/28/99 C:62  PROJECT: PR001                                 *
     H*                SWAP OUT WA001R WITH SA005R                    *
     H*                                                               *
     H* 06/10/98 C:94  PROJECT: DR052                                 *
     H*                DO NOT ALLOW A SEARCH IN FIELDS THAT CAN       *
     H*                NOT BE SEARCHED                                *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**                                                             **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions       **
     H**      40   Add mode                                          **
     H**      60   Used for chaining                                 **
     H**      74   SCR01 SC40002 Error - Duplicate record (PGMN)     **
     H**      76   SCR01 Error - Total > 32(SCDLTL)                  **
     H**      77   SCR01 Error - Total > 40(SCHDTL)                  **
     H**      79   SCR01 SCM0503 Error - Required field (PGMN)       **
     H**      81   SCR01 SCM0503 Error - Required field (DS40)       **
     H**      82   SCR01 SCM0503 Error - Required field (PATH)       **
     H**      83   SCR01 SCM0503 Error - Required field (SEQ#)       **
     H**      84   SCR01 SCM0503 Error - Required field (SEQ#)       **
     H**      85   SCR01 SCM0503 Error - Required field (SEQ#)       **
     H**      86   SCR01 SCM0503 Error - Required field (CODE)       **
     H**      90   Used as general purpose error indicator           **
     H**      99   Used when chaining to file being updated          **
     H**                                                             **
     H*****************************************************************
     FSC330F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F**
     F** Physical file definition (for maintaining record)
     FSMKDP     UF A E             DISK
     F                                     RENAME(PKD01:DBRCDF)
     FSMKDL     IF   E           K DISK
     FSTGTP     IF   E             DISK
     FSTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN ADD *** 01/27/00 ***********************************************
     F*
     F** Key Type File
     FSMKTP     IF   E             DISK
     FSMKTL     IF   E           K DISK
     F                                     RENAME(PKT01:PKT02)
     F*** END   ADD *** 01/27/00 ***********************************************
      /COPY SCOPYMT01D
     D*
     D* ARRAYS FOR SCREEN FIELDS
     D* ARRAYS FOR PHYSICAL FILE
     D*
     D                 DS
     D  HD                     1     64
     D                                     DIM(8)                               HEAD DESCS
     D  SCHD01                 1      8
     D  SCHD02                 9     16
     D  SCHD03                17     24
     D  SCHD04                25     32
     D  SCHD05                33     40
     D  SCHD06                41     48
     D  SCHD07                49     56
     D  SCHD08                57     64
     D*
     D                 DS
     D  HL                     1     40  0
     D                                     DIM(8)                               HEAD LENGTHS
     D  SCHL01                 1      5  0
     D  SCHL02                 6     10  0
     D  SCHL03                11     15  0
     D  SCHL04                16     20  0
     D  SCHL05                21     25  0
     D  SCHL06                26     30  0
     D  SCHL07                31     35  0
     D  SCHL08                36     40  0
     D*
     D                 DS
     D  DS                     1     40  0
     D                                     DIM(8)                               DATA FLD START POS
     D  SCDS01                 1      5  0
     D  SCDS02                 6     10  0
     D  SCDS03                11     15  0
     D  SCDS04                16     20  0
     D  SCDS05                21     25  0
     D  SCDS06                26     30  0
     D  SCDS07                31     35  0
     D  SCDS08                36     40  0
     D*
     D                 DS
     D  DL                     1     40  0
     D                                     DIM(8)                               DATA FLD LENGTH
     D  SCDL01                 1      5  0
     D  SCDL02                 6     10  0
     D  SCDL03                11     15  0
     D  SCDL04                16     20  0
     D  SCDL05                21     25  0
     D  SCDL06                26     30  0
     D  SCDL07                31     35  0
     D  SCDL08                36     40  0
     D*
     D                 DS
     D  DD                     1     40
     D                                     DIM(8)                               DATA FLD DECIMALS
     D  SCDD01                 1      5
     D  SCDD02                 6     10
     D  SCDD03                11     15
     D  SCDD04                16     20
     D  SCDD05                21     25
     D  SCDD06                26     30
     D  SCDD07                31     35
     D  SCDD08                36     40
     D*
     D                 DS
     D  KH                     1     64
     D                                     DIM(8)                               HEAD DESCS
     D  KDHD01                 1      8
     D  KDHD02                 9     16
     D  KDHD03                17     24
     D  KDHD04                25     32
     D  KDHD05                33     40
     D  KDHD06                41     48
     D  KDHD07                49     56
     D  KDHD08                57     64
     D*
     D                 DS
     D  KL                     1     40  0
     D                                     DIM(8)                               HEAD LENGTHS
     D  KDHL01                 1      5  0
     D  KDHL02                 6     10  0
     D  KDHL03                11     15  0
     D  KDHL04                16     20  0
     D  KDHL05                21     25  0
     D  KDHL06                26     30  0
     D  KDHL07                31     35  0
     D  KDHL08                36     40  0
     D*
     D                 DS
     D  KS                     1     40  0
     D                                     DIM(8)                               DATA FLD START POS
     D  KDDS01                 1      5  0
     D  KDDS02                 6     10  0
     D  KDDS03                11     15  0
     D  KDDS04                16     20  0
     D  KDDS05                21     25  0
     D  KDDS06                26     30  0
     D  KDDS07                31     35  0
     D  KDDS08                36     40  0
     D*
     D                 DS
     D  KM                     1     40  0
     D                                     DIM(8)                               DATA FLD LENGTH
     D  KDDL01                 1      5  0
     D  KDDL02                 6     10  0
     D  KDDL03                11     15  0
     D  KDDL04                16     20  0
     D  KDDL05                21     25  0
     D  KDDL06                26     30  0
     D  KDDL07                31     35  0
     D  KDDL08                36     40  0
     D*
     D                 DS
     D  KD                     1     40
     D                                     DIM(8)                               DATA FLD DECIMALS
     D  KDDD01                 1      5
     D  KDDD02                 6     10
     D  KDDD03                11     15
     D  KDDD04                16     20
     D  KDDD05                21     25
     D  KDDD06                26     30
     D  KDDD07                31     35
     D  KDDD08                36     40
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Key Definition Infor-
     D                                     mation')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Work With Generic Table Item parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C     ACDPL1        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*** END   ADD *** 01/27/00 ***********************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKGTKY
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C     KTKY          KLIST
     C                   KFLD                    XXKTYP            2
     C*** END   ADD *** 01/27/00 ***********************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    KDTYPE        XXKYTP
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKGTKY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   SETOFF                                       7479
     C                   DO
     C*
     C* CHECK PROGRAM
     C*
     C  N90SCPGMN        IFEQ      *BLANKS
     C                   SETON                                        9079
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C     SCPGMN        IFNE      CPPGMN
     C     SCPGMN        CHAIN     PKD01                              60
     C  N60              SETON                                        7490
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                  NE ORIGINALPATH
     C                   ENDIF
     C*
     C* CHECK DESCRIPTION
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        9081
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK HEADING FOR KEY
     C*
     C  N90SCHD01        IFEQ      *BLANKS
     C                   SETON                                        9082
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK LENGTH OF HEAD
     C*
     C  N90SCHL01        IFEQ      *ZEROS
     C                   SETON                                        9083
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK DATA START POSITION
     C*
     C  N90SCDS01        IFEQ      *ZEROS
     C                   SETON                                        9084
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK LENGTH OF DATA FIELD
     C*
     C  N90SCDL01        IFEQ      *ZEROS
     C                   SETON                                        9085
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK KEY TYPE
     C*
     C  N90SCKYTP        IFEQ      *BLANKS
     C                   SETON                                        9086
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK LENGTH OF HEAD
     C*
     C  N90              XFOOT     HL            SCHLT1            2 0
     C                   MOVE      SCHLT1        SCHLTL
     C     SCHLT1        IFGT      32
     C                   SETON                                        9076
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK LENGTH OF DATA FIELD
     C*
     C  N90              XFOOT     DL            SCDLT1            2 0
     C                   MOVE      SCDLT1        SCDLTL
     C     SCDLT1        IFGT      40
     C                   SETON                                        9077
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KDTYPE - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   TIME                    DFDTTM
     C                   MOVE      *BLANKS       SCPGMN
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANKS       SCKYTP
     C                   MOVE      *BLANKS       SCCVTP
     C                   MOVE      *BLANKS       SCTPDS
     C                   MOVE      *BLANKS       SCPGDS
     C                   MOVE      *BLANKS       SCFIL
     C                   MOVE      *BLANKS       SCLIB
     C                   MOVE      *BLANKS       SCHLTL
     C                   MOVE      *BLANKS       SCDLTL
     C*
     C                   DO        8             A                 1 0
     C                   MOVE      *BLANKS       HD(A)
     C                   MOVE      *BLANKS       HL(A)
     C                   MOVE      *BLANKS       DS(A)
     C                   MOVE      *BLANKS       DL(A)
     C                   MOVE      *BLANKS       DD(A)
     C                   ENDDO
     C*
     C     *IN40         IFEQ      *OFF
     C                   MOVEL(P)  KDPGMN        SCPGMN
     C                   MOVEL(P)  KDPGMN        CPPGMN           10
     C                   MOVEL(P)  KDDESC        SCDESC
     C                   MOVEL(P)  KDSTAT        SCSTAT
     C                   MOVEL(P)  KDFILE        SCFIL
     C                   MOVEL(P)  KDLIB         SCLIB
     C                   MOVEL(P)  KDTYPE        SCKYTP
     C                   MOVEL(P)  KDCVTP        SCCVTP
     C*
     C* MOVE FIELDS
     C*
     C                   DO        8             A
     C                   MOVEL(P)  KH(A)         HD(A)
     C                   MOVE      KL(A)         HL(A)
     C                   MOVE      KS(A)         DS(A)
     C                   MOVE      KM(A)         DL(A)
     C                   MOVE      KD(A)         DD(A)
     C                   ENDDO
     C*
     C                   XFOOT     HL            SCHLT1
     C                   XFOOT     DL            SCDLT1
     C                   MOVE      SCHLT1        SCHLTL
     C                   MOVE      SCDLT1        SCDLTL
     C*
     C     KDTYPE        IFNE      *BLANKS
     C*** BEGIN DEL *** 01/27/00 ***********************************************
     C**********           MOVEL'KTYP'    KKTBID    P
     C**********           MOVELKDTYPE    KKGTKY    P
     C********** GTKY      CHAINPGT02                60
     C**********           MOVELGTDESC    SCTPDS    P
     C*** END   DEL *** 01/27/00 ***********************************************
     C*
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C                   MOVEL(P)  KDTYPE        XXKTYP
     C     KTKY          CHAIN     PKT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  KTPDES        SCTPDS
     C                   ELSE
     C                   MOVE      *BLANKS       SCTPDS
     C                   ENDIF
     C*** END   ADD *** 01/27/00 ***********************************************
     C                   ENDIF
     C*
     C     KDCVTP        IFNE      *BLANKS
     C                   MOVEL(P)  'KCVT'        KKTBID
     C                   MOVEL(P)  KDCVTP        KKGTKY
     C     GTKY          CHAIN     PGT02                              60
     C                   MOVEL(P)  GTDESC        SCPGDS
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCPGMN        KDPGMN
     C                   MOVEL     SCDESC        KDDESC
     C                   MOVEL     SCSTAT        KDSTAT
     C                   MOVEL     SCFIL         KDFILE
     C                   MOVEL     SCLIB         KDLIB
     C                   MOVEL     SCCVTP        KDCVTP
     C                   MOVEL     SCKYTP        KDTYPE
     C*
     C* MOVE FIELDS
     C*
     C                   DO        8             A
     C                   MOVEL     HD(A)         KH(A)
     C                   MOVE      HL(A)         KL(A)
     C                   MOVE      DS(A)         KS(A)
     C                   MOVE      DL(A)         KM(A)
     C                   MOVE      DD(A)         KD(A)
     C                   EXSR      MVUPDB
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* MVUPDB - Move update fields to data base fields
     C*================================================================
     C     MVUPDB        BEGSR
     C                   MOVEL     Q1USER        KDUSER
     C                   MOVEL     Q1PGM         KDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KDDT
     C                   MOVEL     DSTIME        KDTIME
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         IFEQ      '1'
     C                   MOVE      *ZEROS        ##RRN
     C*----------------------------------------------------------------
     C* F10=SEARCH               *  KEY TYPE  *
     C*----------------------------------------------------------------
     C     HFLD          IFEQ      'SCKYTP'
     C*
     C*** BEGIN DEL *** 01/27/00 ***********************************************
     C**********           MOVEL'KTYP'    ##TBID    P
     C**********
     C**********           CALL 'S2380R'  ACDPL
     C**********
     C********** ##RRN     CHAINPGT01                60
     C**********           MOVELGTKEY     SCKYTP
     C**********           MOVELGTDESC    SCTPDS    P
     C*** END   DEL *** 01/27/00 ***********************************************
     C*
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C                   CALL      'SC092R'      ACDPL1
     C*
     C     ##RRN         CHAIN     PKT01                              60
     C                   MOVEL     KTKTYP        SCKYTP
     C                   MOVEL     KTPDES        SCTPDS
     C*** END   ADD *** 01/27/00 ***********************************************
     C*
     C                   ELSE                                                   HFLD
     C*
     C*----------------------------------------------------------------
     C* F10=SEARCH               *  CONVERSION TYPE   *
     C*----------------------------------------------------------------
     C     HFLD          IFEQ      'SCCVTP'
     C*
     C                   MOVEL(P)  'KCVT'        ##TBID
     C*
     C                   CALL      'S2380R'      ACDPL
     C*
     C     ##RRN         CHAIN     PGT01                              60
     C                   MOVEL     GTKEY         SCCVTP
     C                   MOVEL(P)  GTDESC        SCPGDS
     C*** BEG ADD *** 06/10/98 **************************************
     C                   ELSE                                                   HFLD SCCVTP
     C*
     C     HFLD          IFEQ      'SCFIL'
     C                   SETON                                        9078
     C                   ELSE                                                   HFLD SCFIL
     C     HFLD          IFEQ      'SCLIB'
     C                   SETON                                        9080
     C                   ENDIF                                                  HFLD SCLIB
     C                   ENDIF                                                  HFLD SCFIL
     C*** END ADD *** 06/10/98 **************************************
     C                   ENDIF                                                  HFLD SCCVTP
     C                   ENDIF                                                  HFLD SCKYTP
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  INKJ = ON
     C                   ENDSR
     C*================================================================
     C*  PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*          MODE 'S' - Change status
     C*
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     KDSTAT        IFEQ      'I'
     C                   MOVE      'A'           KDSTAT
     C                   ELSE
     C                   MOVE      'I'           KDSTAT
     C                   ENDIF
     C                   EXSR      MVUPDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        KDIUSR
     C                   MOVEL     Q1PGM         KDIPGM
     C                   MOVEL     DFTIME        KDITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KDIDT
     C                   WRITE     DBRCDF
     C                   FEOD      SMKDP
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
