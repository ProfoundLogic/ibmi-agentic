     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0025R - EXPECTED LOSS & DISCOUNT RATIO WORK/WITH
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR EXPECTED LOSS & DISCOUNT RATIO          *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: E0025R02                                            *
     H*                                                               *
     H* CREATED: 06/14/2001                                           *
     H*                                                               *
     H* 06/14/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 06/19/24 C:291 Project: WC2952                                *
     H*                Exapand Expected Loss Rate to 8,3.             *
     H*                Changed SCLsRt field in display file to 8,3.   *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H*****************************************************************
     FE0025F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FEMEXP     IF   E             DISK
     F                                     RENAME(PEX01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FEMEXL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEX01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
      *** Beg Add ***** 06/19/24 **********************************
     D strLsRt         S              9
     D intPos          S              3  0
     D intLsRt         S              8  5
      *** End Add ***** 06/19/24 **********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D                 DS
     D  VIEW1                  1     71
     D  V1ST                   1      2
     D  V1EFDT                 5     14
     D  V1CLAS                21     25
      *** Beg Chg ***** 06/19/24 ***************************************
     D***  V1LRT1                34     34
     D***  V1LRT2                35     35
     D***  V1LRT3                36     39
     D  V1LRT1                31     39
      *** End Chg ***** 06/19/24 ***************************************
     D  V1DRT1                49     49
     D  V1DRT2                50     50
     D  V1DRT3                51     52
     D  V1STAT                58     58
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Expected Losses/Di-
     D                                     scount Ratio')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('E0025R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D DELETD          C                   CONST('*Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKCLAS
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C*
     C     STKY          KLIST
     C                   KFLD                    K1ST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EXCO#         KKCO#
     C     *LIKE         DEFINE    EXST          KKST
     C     *LIKE         DEFINE    EXEFDT        KKEFDT
     C     *LIKE         DEFINE    EXCLAS        KKCLAS
     C     *LIKE         DEFINE    EXEFDT        S1EFDT
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSST          IFNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
     C     SSFRCL        ORNE      *ZEROS
     C     SSTOCL        ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
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
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71  90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C*
     C                   ENDIF                                                  IF SRERR = 1
     C                   ENDIF                                                  IF 90 OFF
     C*
     C  N90SSFRCL        IFGT      SSTOCL
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSST
     C                   CLEAR                   SSEFDT
     C                   CLEAR                   SSFRCL
     C                   CLEAR                   SSTOCL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
      *
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     *LOVAL        KKST
     C                   Z-ADD     0             KKEFDT
     C                   Z-ADD     *LOVAL        KKCLAS
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL(P)  SCPST1        KKST
     C*
     C                   MOVE      *BLANKS       SRDTI
     C                   MOVE      SCPSTA        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   MOVEL(P)  SCPSTB        KKCLAS
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     SCST          KKST
     C*
     C                   MOVE      *BLANKS       SRDTI
     C                   MOVE      SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   Z-ADD     SCCLAS        KKCLAS
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
      *
     C                   MOVEL     EXST          SCST
      *
     C                   Z-ADD     EXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   MOVEL     EXCLAS        SCCLAS
      *** Beg Chg ***** 06/19/24 ***************************************
     C***                MOVEL     EXLSRT        SCLSRT
       SCLsRt = EXLsRt;
      *** End Chg ***** 06/19/24 ***************************************
     C                   MOVEL     EXDSRT        SCDSRT
     C                   MOVE      EXSTAT        SCSTAT
      *
     C                   MOVE      EXST          V1ST
      *
     C                   MOVEL(P)  SCEFDT        ##NUM
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C                   MOVEL     ##NUM         V1EFDT
      *
     C                   MOVEL(P)  SCCLAS        ##NUM
     C                   MOVEL(P)  '4'           ##MASK
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM
     C                   PARM                    ##MASK
     C                   MOVEL     ##NUM         V1CLAS
      *
      *** Beg Chg ***** 06/19/24 ***************************************
     C***   SCLSRT        IFEQ      *ZEROS
     C***                 MOVE      *BLANKS       V1LRT1
     C***                 MOVE      *BLANKS       V1LRT2
     C***                 MOVE      *BLANKS       V1LRT3
     C***                 ELSE
     C***                 MOVEL     SCLSRT        V1LRT1
     C***   V1LRT1        IFEQ      *ZERO
     C***                 MOVE      *BLANK        V1LRT1
     C***                 ENDIF
     C***                 MOVE      '.'           V1LRT2
     C***                 MOVE      SCLSRT        V1LRT3
     C***                 ENDIF                                                  SCLSRT = 0
       Clear V1LRT1;
       If EXLsRt <> *Zeros;
          intLsRt = EXLsRt;
          intLsRt = intLsRt/100;
          strLsRt = %Char(intLsRt);
          intPos =  %Scan(' ':strLsRt);
          If intPos <> 0;
             V1LRT1 = %Replace(%Subst(strLsRt:1:intPos-1):V1LRT1:
              (%Len(strLsRt)-(intPos-1))+1);
          Else;
             V1LRT1 = strLsRt;
          Endif;
       Endif;
      *** End Chg ***** 06/19/24 ***************************************
      *
     C     SCDSRT        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       V1DRT1
     C                   MOVE      *BLANKS       V1DRT2
     C                   MOVE      *BLANKS       V1DRT3
     C                   ELSE
     C                   MOVEL     SCDSRT        V1DRT1
     C     V1DRT1        IFEQ      *ZERO
     C                   MOVE      *BLANK        V1DRT1
     C                   ENDIF
     C                   MOVE      '.'           V1DRT2
     C                   MOVE      SCDSRT        V1DRT3
     C                   ENDIF                                                  SCDSRT = 0
      *
     C                   MOVE      EXSTAT        V1STAT
      *
     C                   MOVEL     VIEW1         SCDATA
      *
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 CHANGE STATUS
     C*================================================================
     C     OPT20         BEGSR
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
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
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   MOVE      HD(2)         SCHDL2
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1A         READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1A         READPE    DBRCDF                                 95
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
     C                   MOVEL(P)  DELETD        SCDATA
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
     C*
     C     SSST          IFNE      *BLANKS
     C     SSST          ANDNE     EXST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        COMP      EXEFDT                                 40
     C  N40              MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFRCL        IFNE      *ZEROS
     C     SSFRCL        ANDGT     EXCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFRCL
     C*
     C     SSTOCL        IFNE      *ZEROS
     C     SSTOCL        ANDLT     EXCLAS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  TOFRCL
     C*
     C                   ENDSR
** Column headings
                        Payroll      Expected     Discount
Opt   ST     Eff. Dt      Class     Loss Rate        Ratio   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
