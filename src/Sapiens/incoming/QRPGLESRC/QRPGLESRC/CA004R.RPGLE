     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE CA003R - API TO SELECT RESERVE
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM DISPLAYS RESERVES FOR SELECTION            **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MANY PGMS                                               **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:53                                          **
     H**   CREATED:   02/08/05                                       **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:33   Retrieve brand Name                          *
     H*****************************************************************
     FCA004F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     FCDCBL7    IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F*
      /COPY SCOPYMT01D
     D*
     D COMAPI          C                   CONST('SA005R')
      ***** BEG DEL ***** 01/28/10 ************************************
     D***           'Progressive Data So- C         PDSNAM
     D***           'lutions, Inc.'
     D*
     D***PSDS      ESDSSZQ1P
      ***** BEG DEL ***** 01/28/10 ************************************
      ***** BEG ADD ***** 01/28/10 ************************************
     D*
     D***PY SCOPYMT01I
     D*
      ***** END ADD ***** 01/28/10 ************************************
     D*
     D                 DS
     D  FL                     1     20
     D                                     DIM(20) ASCEND
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D  SCFL15                15     15
     D  SCFL16                16     16
     D  SCFL17                17     17
     D  SCFL18                18     18
     D  SCFL19                19     19
     D  SCFL20                20     20
     D                 DS
     D  CC                     1     40
     D                                     DIM(20)
     D  SCCC01                 1      2
     D  SCCC02                 3      4
     D  SCCC03                 5      6
     D  SCCC04                 7      8
     D  SCCC05                 9     10
     D  SCCC06                11     12
     D  SCCC07                13     14
     D  SCCC08                15     16
     D  SCCC09                17     18
     D  SCCC10                19     20
     D  SCCC11                21     22
     D  SCCC12                23     24
     D  SCCC13                25     26
     D  SCCC14                27     28
     D  SCCC15                29     30
     D  SCCC16                31     32
     D  SCCC17                33     34
     D  SCCC18                35     36
     D  SCCC19                37     38
     D  SCCC20                39     40
     D                 DS
     D  SC                     1     40
     D                                     DIM(20)                              SECTION CDS/ADDED 71
     D  PMCC01                 1      2
     D  PMCC02                 3      4
     D  PMCC03                 5      6
     D  PMCC04                 7      8
     D  PMCC05                 9     10
     D  PMCC06                11     12
     D  PMCC07                13     14
     D  PMCC08                15     16
     D  PMCC09                17     18
     D  PMCC10                19     20
     D  PMCC11                21     22
     D  PMCC12                23     24
     D  PMCC13                25     26
     D  PMCC14                27     28
     D  PMCC15                29     30
     D  PMCC16                31     32
     D  PMCC17                33     34
     D  PMCC18                35     36
     D  PMCC19                37     38
     D  PMCC20                39     40
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCCLM#                 4     15

     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C*****                PARM           @@RRN   90
     C                   PARM                    @@POL#           13
     C                   PARM                    @@CLM#           12
     C                   PARM                    SCPRDS
     C*
     C     COMPL         PLIST
     C                   PARM                    UPCO#             3 0
     C                   PARM                    ##CONM           40
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKPOL#           13
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  CLAIM RESERVE FILE - TO CHECK FOR LOSS SECTIONS.
     C*
     C     CBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKCLM#           12
     C                   KFLD                    SC(X)
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C                   CALL      COMAPI        COMPL                  99
     C*
     C     *IN99         IFEQ      *ON
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********           MOVELPDSNAM    ##DATA    P
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'I'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL(P)  #BRND1        ##DATA
     C***  End   Chg  ***  01/28/10  **************************************
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM
     C*
     C                   MOVE      @@POL#        KKPOL#
     C                   MOVE      @@CLM#        KKCLM#
     C*
     C* LOAD
     C*
     C     PMKEY         CHAIN     PPM01                              21
     C                   MOVEL     PMPOL#        SCPOL#
     C                   MOVEL(P)  PMFIN         SCFIN
     C                   MOVEL(P)  @@CLM#        SCCLM#
     C                   MOVEL(P)  PMAGCO        SCAGT#
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C     PMCANC        IFEQ      'P'
     C                   MOVE      '1'           *IN05
     C                   ELSE
     C                   MOVE      '0'           *IN05
     C                   END
     C     PMCANC        IFEQ      'Q'
     C                   MOVE      '1'           *IN06
     C                   ELSE
     C                   MOVE      '0'           *IN06
     C                   END
     C*
     C*
     C*** In inquiry mode, display only sections that have some loss.
     C*
     C                   MOVEA     *BLANKS       CC
     C     @@MODE        IFEQ      '1'
     C* *IN52 = ON MEANS SKIP SCREEN 2 SINCE THERE IS NO LOSS SECTION
     C                   SETON                                        52
     C                   Z-ADD     1             X                 2 0
     C                   Z-ADD     1             Y                 2 0
     C     X             DOWLE     20
     C     SC(X)         IFNE      *BLANKS
     C     CBKEY         CHAIN     PCB01                              65
     C     *IN65         IFEQ      '0'
     C                   MOVE      SC(X)         CC(Y)
     C                   ADD       1             Y
     C                   SETOFF                                       52        GOTO SCREEN 2
     C                   ENDIF
     C                   ENDIF                                                  IF SC = '  '
     C                   ADD       1             X
     C                   ENDDO                                                  DOW <= 20
     C                   ELSE                                                   ADD/UPDATE MODE
     C                   SETOFF                                       52        GOTO SCREEN 2
     C                   MOVE      PMCC01        SCCC01
     C                   MOVE      PMCC02        SCCC02
     C                   MOVE      PMCC03        SCCC03
     C                   MOVE      PMCC04        SCCC04
     C                   MOVE      PMCC05        SCCC05
     C                   MOVE      PMCC06        SCCC06
     C                   MOVE      PMCC07        SCCC07
     C                   MOVE      PMCC08        SCCC08
     C                   MOVE      PMCC09        SCCC09
     C                   MOVE      PMCC10        SCCC10
     C                   MOVE      PMCC11        SCCC11
     C                   MOVE      PMCC12        SCCC12
     C                   MOVE      PMCC13        SCCC13
     C                   MOVE      PMCC14        SCCC14
     C                   MOVE      PMCC15        SCCC15
     C                   MOVE      PMCC16        SCCC16
     C                   MOVE      PMCC17        SCCC17
     C                   MOVE      PMCC18        SCCC18
     C                   MOVE      PMCC19        SCCC19
     C                   MOVE      PMCC20        SCCC20
     C                   END                                                    IF INQUIRY MODE
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     SCR01
     C* F3/F12
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   RETURN
     C                   ENDIF
     C* F14
     C*
     C*   IF CLAIMANT ID IS BLANK CALL THE APPROPIATE SECTIONS
     C*
     C***        SCCLID    IFEQ *BLANK
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     20
     C     *BLANK        LOOKUP    FL(X)                              60
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVEL     CC(X)         XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFNE      '1'
     C     CVVAL         CASEQ     'L'           LIAB
     C     CVVAL         CASEQ     'A'           AUTO
     C     CVVAL         CASEQ     'I'           INLMAR
     C     CVVAL         CASEQ     'G'           GLASS
     C     CVVAL         CASEQ     'B'           BOILMC
     C     CVVAL         CASEQ     'C'           FIRE
     C     CVVAL         CASEQ     'T'           CRIME
     C     CVVAL         CASEQ     'F'           FIDLTY
     C     CVVAL         CASEQ     'O'           BUSOWN
     C     CVVAL         CASEQ     'R'           FARM
     C                   ENDCS
     C                   ENDIF
     C*
     C*********  QQRTC     CABEQ'A'       TERM
     C*
     C                   ELSE
     C*
     C                   Z-ADD     20            X
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C********             ENDIF
     C*
     C******
     C******     *IN90     IFEQ *ON
     C*********            MOVE '1'       *IN90
     C******               ITER
     C******               ENDIF
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C*
     C*****************************************************************
     C*   SUBROUTINES TO CALL THE APPROPIATE PROGRAM AFTER SECTION    *
     C*   HAS BEEN SELECTED AND VALIDATED BY PROGRAM                  *
     C*****************************************************************
     C*
     C*
     C*   CALL LIABALITY PROGRAM
     C*
     C     LIAB          BEGSR
     C                   MOVEL     CC(X)         QQCODE
     C                   MOVEL     SCCLM#        QQCLM#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVE      ' '           QQRTC
     C                   CALL      'C0202R'
     C                   PARM      SCPRDS        QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQLOC#            3
     C                   PARM                    QQPRM#            3
     C                   PARM                    QQRTC             1
     C                   ENDSR
     C*
     C*   CALL AUTO PROGRAM
     C*
     C     AUTO          BEGSR
     C                   MOVEL     CC(X)         QQCODE
     C                   MOVEL     SCCLM#        QQCLM#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVE      ' '           QQRTC
     C                   CALL      'C0203R'
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQITEM            5
     C                   PARM                    QQCLAS            6
     C                   PARM                    QQRTC
     C*
     C*          CALL REMARKS PROGRAM
     C*          ------------------------------------------------------
     C     @@MODE        IFEQ      *OFF
     C****                 MOVE Q2RECF    HLRECF 10
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      UPCO#         QCCO#
     C                   MOVE      SCCLM#        QCCLM#           12
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C********             WRITESCR99
     C********             MOVE *BLANKS   Q2RECF
     C********             MOVE HLRECF    Q2RECF
     C                   END
     C                   ENDSR
     C*
     C*   CALL INLAND MARINE PROGRAM
     C*
     C     INLMAR        BEGSR
     C                   MOVEL     CC(X)         QQCODE            2
     C                   MOVEL     SCCLM#        QQCLM#           12
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      ' '           QQRTC             1
     C                   CALL      'C0205R'
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   ENDSR
     C*
     C*   CALL GLASS PROGRAM
     C*
     C     GLASS         BEGSR
     C                   MOVEL     CC(X)         QQCODE            2
     C                   MOVEL     SCCLM#        QQCLM#           12
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      ' '           QQRTC             1
     C                   CALL      'C0205R'
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   ENDSR
     C*
     C*   CALL BOILER MACHINERY PROGRAM
     C*
     C     BOILMC        BEGSR
     C*******************  MOVELCC,X      QQCODE  2
     C***                  MOVELSCPOL#    QQPOL# 13
     C***                  CALL 'C0000R'
     C***                  PARM           QQPRDS
     C***                  PARM           QQCALL
     C***                  PARM           QQPOL#
     C*******************  PARM           QQCODE
     C                   ENDSR
     C*
     C*   CALL FIRE PROGRAM
     C*
     C     FIRE          BEGSR
     C                   MOVEL     CC(X)         QQCODE            2
     C                   MOVEL     SCCLM#        QQCLM#           12
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      ' '           QQRTC             1
     C                   CALL      'C0201R'
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   ENDSR
     C*
     C*   CALL BURGLARY AND THIEFT PROGRAM
     C*
     C     CRIME         BEGSR
     C                   MOVEL     CC(X)         QQCODE
     C                   MOVEL     SCCLM#        QQCLM#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVE      ' '           QQRTC
     C                   CALL      'C0204R'
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   ENDSR
     C*
     C*   CALL FIDELITY AND FORGERY PROGRAM
     C*
     C     FIDLTY        BEGSR
     C                   MOVEL     CC(X)         QQCODE
     C                   MOVEL     SCCLM#        QQCLM#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVE      ' '           QQRTC
     C                   CALL      'C0204R'
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   ENDSR
     C*
     C*   CALL BUSINESSOWNER PROGRAM
     C*
     C     BUSOWN        BEGSR
     C                   MOVEL     CC(X)         QQCODE
     C                   MOVEL     SCCLM#        QQCLM#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVE      ' '           QQRTC
     C                   CALL      'C0206R'
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCODE
     C                   PARM                    QQCLM#
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   ENDSR
     C*
     C*   CALL FARM & RANCH OWNERS PROGRAM
     C*
     C     FARM          BEGSR
     C*******************  MOVELCC,X      QQCODE  2
     C***                  MOVELSCPOL#    QQPOL# 13
     C***                  CALL 'C0000R'
     C***                  PARM           QQPRDS
     C***                  PARM           QQCALL
     C***                  PARM           QQPOL#
     C*******************  PARM           QQCODE
     C                   ENDSR
     C****
     C*
     C*=====================================================
     C****       TITLE     BEGSR
     C*=====================================================
     C****                 MOVELTITL2     ##DATA
     C****                 CALL 'SRCENTER'
     C****                 PARM           ##DATA 80
     C****                 PARM 40        ##LENG  30
     C****                 MOVEL##DATA    SCTITL
     C****                 ENDSR
     C*
     C*=====================================================
     O*
